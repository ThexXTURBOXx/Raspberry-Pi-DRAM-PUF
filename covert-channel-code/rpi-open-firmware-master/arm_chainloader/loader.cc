/*=============================================================================
Copyright (C) 2016-2017 Authors of rpi-open-firmware
All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

FILE DESCRIPTION
Second stage bootloader.

=============================================================================*/

#include "chainloader.h"
#include "memory_map.h"
#include "mmu.h"
#include "drivers/fatfs/ff.h"
#include "drivers/mailbox.hpp"
#include "drivers/block_device.hpp"

#include <libfdt.h>
#include <hardware.h>
#include <crc32.h>

#include <string.h>
#include <stdio.h>

#include "drivers/fatfs/diskio.h"
#include "start.h"

#define logf(fmt, ...) print_timestamp(); printf("[LDR:%s]: " fmt, __FUNCTION__, ##__VA_ARGS__);

FATFS g_BootVolumeFs;

#define ROOT_VOLUME_PREFIX "0:"
#define DTB_LOAD_ADDRESS       0xF000000 // 240 mb from start
#define KERNEL_LOAD_ADDRESS    0x2000000 // 32 mb from start
#define INITRD_LOAD_ADDRESS    0x4100000 // 64.x mb from start
#define PUF_RESULT             0x2800000U
#define PUF_PARAM_LOAD_ADDRESS 0x4000000U

typedef void __attribute__((noreturn)) (*linux_t)(uint32_t, uint32_t, void*);

struct mem_entry {
  uint32_t address;
  uint32_t size;
};

struct ranges {
  uint32_t child;
  uint32_t parent;
  uint32_t size;
};

static_assert((MEM_USABLE_START+0x800000) < KERNEL_LOAD_ADDRESS,
              "memory layout would not allow for kernel to be loaded at KERNEL_LOAD_ADDRESS, please check memory_map.h");

static uint32_t htonl(uint32_t hostlong) {
  return ((hostlong & 0x000000ff) << 24) |
         ((hostlong & 0x0000ff00) << 8) |
         ((hostlong & 0x00ff0000) >> 8) |
         ((hostlong & 0xff000000) >> 24);
}

void hex32(uint32_t input, char *output) {
  output[8] = 0;
  for (int i=7; i>=0; i--) {
    int nibble = input & 0xf;
    if (nibble <= 9) {
      output[i] = 0x30 + nibble;
    } else {
      output[i] = 0x57 + nibble;
    }
    input = input >> 4;
  }
}

struct LoaderImpl {
  LoaderImpl() {
    find_and_mount();
    auto kernel = load_kernel();
    size_t initrd_size = 0;
    bool do_load_initrd = true;
    uint8_t *initrd = NULL;
    if (do_load_initrd) {
      load_initrd(&initrd_size);
    }

    /* read the command-line null-terminated */
    uint8_t *cmdline;
    size_t cmdlen = read_file("cmdline.txt", cmdline);

    logf("kernel cmdline: %s\n", cmdline);

    /* load flat device tree */
    uint8_t* fdt;
    if (do_load_initrd) {
      const char *dtb_name = detect_model_dtb();
      fdt = load_fdt_and_initrd(dtb_name, (char*)cmdline, initrd, initrd_size);
    } else {
      fdt = load_fdt("rpi.dtb", (char*)cmdline);
    }

    /* once the fdt contains the cmdline, it is not needed */
    delete[] cmdline;

    /* the eMMC card in particular needs to be reset */
    if (!file_exists("puf_en")) {
      teardown_hardware();
      mmu_off();
      disable_icache();
    }

    run_linux(kernel, fdt);
  }

  inline void find_and_mount() {
    logf("Mounting boot partition ...\n");
    FRESULT r = f_mount(&g_BootVolumeFs, ROOT_VOLUME_PREFIX, 1);
    if (r != FR_OK) {
      panic("failed to mount boot partition, error: %d", (int)r);
    }
    logf("Boot partition mounted!\n");
  }

  linux_t load_kernel() {
    /* read the kernel as a function pointer at fixed address */
    uint8_t* zImage = reinterpret_cast<uint8_t*>(KERNEL_LOAD_ADDRESS);
    linux_t kernel = reinterpret_cast<linux_t>(zImage);

    const char *kernelPath[] = {"kernel.img", "kernel1.img", "kernel2.img", "kernell.img", "ker.img", "zImage"};
    const char *loadPath = "kernel.img";
    for (auto &path : kernelPath) {
        if (file_exists(path)) {
            loadPath = path;
            break;
        }
    }
    size_t ksize = read_file(loadPath, zImage, false);
    logf("zImage loaded at 0x%X\n", (unsigned int)kernel);
    return kernel;
  }

  uint8_t *load_initrd(size_t *size) {
    uint8_t *initrd = reinterpret_cast<uint8_t*>(INITRD_LOAD_ADDRESS);
    *size = read_file("initrd", initrd, false);
    return initrd;
  }

  void mailbox_write(uint32_t v) {
    while (ARM_0_MAIL1_STA & ARM_MS_FULL);
    ARM_0_MAIL1_WRT = v;
  }

  uint32_t mailbox_read() {
    while (ARM_0_MAIL0_STA & ARM_MS_EMPTY);
    return ARM_0_MAIL0_RD;
  }

  int pown(int x, int y) {
    int p = 1;
    for (int i = 0; i < y; ++i) {
      p *= x;
    }
    return p;
  }

  #include "getparam.cc"

  inline void __attribute__((noreturn)) run_linux(linux_t kernel, uint8_t *fdt) {
    /* fire away -- this should never return */

#if 0
    logf("stalling until jtag attaches\n");
    debug_stall();
#endif

    if (!file_exists("puf_en")) {
      logf("Jumping to the Linux kernel...\n");
      kernel(0, ~0, fdt);
    } else {
      // Start PUF param from SD mode

      int pass = 0, temp = 0, dec = 0;
      uint32_t curr_size = 0, curr_size_bytes = 0, v = 0;
      char* puf_params = reinterpret_cast<char*>(PUF_PARAM_LOAD_ADDRESS);

#define MAX_FILE_NAME_SIZE 8
      char file_name[MAX_FILE_NAME_SIZE + 1];
      file_name[MAX_FILE_NAME_SIZE] = 0;

      // Send magic number and dump mode specifier
      mailbox_write(0xf2345678);
      delay_ms(50);
      mailbox_write(5);
      delay_ms(50);

      while (1) {
        printf("Starting run #%d...\n", pass);

        // Read file again because RPi is stupid
        uint8_t* puf_params_start = reinterpret_cast<uint8_t*>(PUF_PARAM_LOAD_ADDRESS);
        read_file("pufs.txt", puf_params_start, false, false);

        // Read next PUF params
        skip_comments(puf_params, '#', '\n');

        printf("Sending params to FW: ");

        const int stradd = getaddress(puf_params, '\t');
        mailbox_write(stradd);
        printf("0x%x,", stradd);
        delay_ms(50);

        const int endadd = getaddress(puf_params, '\t');
        mailbox_write(endadd);
        printf("0x%x,", endadd);
        delay_ms(50);

        const int initval = getinitvalue(puf_params, '\t');
        mailbox_write(initval);
        printf("0x%x,", initval);
        delay_ms(50);

        const int decaytime = getdecaytime(puf_params, '\t');
        mailbox_write(decaytime);
        printf("%d,", decaytime);
        delay_ms(50);

        const int addmode = getaddmode(puf_params, '\t');
        mailbox_write(addmode);
        printf("%d,", addmode);
        delay_ms(50);

        const int funcloc = getaddmode(puf_params, '\t');
        mailbox_write(funcloc);
        printf("%d,", funcloc);
        delay_ms(50);

        const int dcyfunc = getmode(puf_params, '\t');
        mailbox_write(dcyfunc);
        printf("%d,", dcyfunc);
        delay_ms(50);

        const int nfreq = getfuncfreq(puf_params, '\n');
        mailbox_write(nfreq);
        printf("%d\n", nfreq);

        // Wow, such wow, fancy progress thingy
        for (temp = 1; temp <= 9; ++temp) {
            delay_ms(decaytime * 100);
            printf("\rPUF is about %d%% finished", temp * 10);
        }
        printf("\n");

        // PUF is finished
        curr_size = mailbox_read();
        curr_size_bytes = curr_size*4;
        printf("Received write signal for %u bytes (%u ints)\n", curr_size_bytes, curr_size);

        for (uint32_t i = 0; i < curr_size; ++i) {
          // Request next entry
          // TODO: Add timeout and handle it properly (request entry i again)
          mailbox_write(i);
          v = mailbox_read();
          // Fix memory alignment
          reinterpret_cast<uint32_t*>(PUF_RESULT)[i] =
                  ((v & 0x000000ff) << 24)
                | ((v & 0x0000ff00) << 8)
                | ((v & 0x00ff0000) >> 8)
                | ((v & 0xff000000) >> 24);
        }
        printf("Received PUF result\n");

        temp = pass;
        for (int i = 0; i < MAX_FILE_NAME_SIZE; ++i) {
          dec = pown(10, MAX_FILE_NAME_SIZE-i-1);
          file_name[i] = (temp / dec) + '0';
          temp %= dec;
        }
        write_file(file_name, reinterpret_cast<uint8_t*>(PUF_RESULT), curr_size_bytes);
        printf("Memory dump of %u bytes written\n", curr_size_bytes);

        // After each run, send magic number again
        ++pass;
        mailbox_write(0xf2345678);
        delay_ms(50);
      }
    }
  }

  bool file_exists(const char* path) {
    return f_stat(path, NULL) == FR_OK;
  }

  size_t read_file(const char* path, uint8_t*& dest, bool should_alloc = true, bool panicIfNotExists = true) {
    uint32_t start = ST_CLO;
    /* ensure file exists first */
    if(!file_exists(path)) {
      if(panicIfNotExists) {
        panic("attempted to read %s, but it does not exist, error=%d", path, f_stat(path, NULL));
      } else {
        logf("attempted to read %s, but it does not exist, error=%d", path, f_stat(path, NULL));
      }
      // Never reached if panic
      return 0;
    }

    /* read entire file into buffer */
    FIL fp;
    f_open(&fp, path, FA_READ);

    unsigned int len = f_size(&fp);

    if(should_alloc) {
      /*
       * since this can be used for strings, there's no harm in reserving an
       * extra byte for the null terminator and appending it.
       */
      uint8_t* buffer = new uint8_t[len + 1];
      dest = buffer;
    }
    dest[len] = 0;

    logf("%s: reading %d bytes to 0x%X ~%dmb...\n", path, len, (unsigned int)dest, ((unsigned int)dest)/1024/1024);

    f_read(&fp, dest, len, &len);
    f_close(&fp);

    uint32_t stop = ST_CLO;
    uint32_t elapsed = stop - start;

    uint32_t bytes_per_second = (double)len / ((double)(elapsed) / 1000 / 1000);
    logf("%d kbyte copied at a rate of %ld kbytes/second, CRC32: 0x%x\n", len/1024, bytes_per_second/1024, 0); //rc_crc32(0, (const char*)dest, len));

    return len;
  }

  size_t write_file(const char* path, const uint8_t* src, unsigned int len, BYTE mode = FA_CREATE_ALWAYS | FA_CREATE_NEW) {
    uint32_t start = ST_CLO;

    FIL fp;
    f_open(&fp, path, FA_WRITE | mode);

    logf("Writing %d bytes to %s...\n", len, path);

    f_write(&fp, src, len, &len);
    f_close(&fp);

    uint32_t stop = ST_CLO;
    uint32_t elapsed = stop - start;

    uint32_t bytes_per_second = (double)len / ((double)(elapsed) / 1000 / 1000);
    logf("%d kbyte copied at a rate of %ld kbytes/second, CRC32: 0x%x\n", len/1024, bytes_per_second/1024, 0); //rc_crc32(0, (const char*)dest, len));

    return len;
  }

  uint8_t *load_fdt(const char *filename, const char *cmdline) {
      /* read device tree blob */
      uint8_t *fdt = reinterpret_cast<uint8_t *>(DTB_LOAD_ADDRESS);
      size_t sz = read_file(filename, fdt, false);
      logf("FDT loaded at %X, size is %d\n", (unsigned int) fdt, sz);

      void *v_fdt = reinterpret_cast<void *>(fdt);

      int res;

      if ((res = fdt_check_header(v_fdt)) != 0) {
          panic("FDT blob invalid, fdt_check_header returned %d", res);
      }

      /* pass in command line args */
      res = fdt_open_into(v_fdt, v_fdt, sz + 4096);

      int node = fdt_path_offset(v_fdt, "/chosen");
      if (node < 0)
          panic("no chosen node in fdt");

      res = fdt_setprop(v_fdt, node, "bootargs", cmdline, strlen(cmdline) + 1);

      /* pass in a memory map, skipping first meg for bootcode */
      int memory = fdt_path_offset(v_fdt, "/memory");
      if (memory < 0)
          panic("no memory node in fdt");

      /* start the memory map at 1M/16 and grow continuous for 256M
        * TODO: does this disrupt I/O? */

      char dtype[] = "memory";
      uint8_t memmap[] = {0x00, 0x00, 0x01, 0x00, 0x30, 0x00, 0x00, 0x00};
      res = fdt_setprop(v_fdt, memory, "reg", (void *) memmap, sizeof(memmap));

      logf("FDT loaded at 0x%X\n", (unsigned int) fdt);

      return fdt;
  }

  uint8_t* load_fdt_and_initrd(const char* filename, const char* cmdline, uint8_t *initrd_start, size_t initrd_size) {
    /* read device tree blob */
    uint8_t* fdt = reinterpret_cast<uint8_t*>(DTB_LOAD_ADDRESS);
    size_t sz = read_file(filename, fdt, false);
    logf("FDT loaded at %p, size is %d\n", fdt, sz);

    void* v_fdt = reinterpret_cast<void*>(fdt);

    int res;

    if ((res = fdt_check_header(v_fdt)) != 0) {
      panic("fdt blob invalid, fdt_check_header returned %d", res);
    }

    res = fdt_open_into(v_fdt, v_fdt, sz + 4096);

    char serial_number[9];
    hex32(g_FirmwareData.serial, serial_number);
    // /serial-number is an ascii string containing the serial#
    res = fdt_setprop(v_fdt, 0, "serial-number", serial_number, 9);

    int ethernet0 = fdt_path_offset(v_fdt, "ethernet0");
    if (ethernet0 < 0) {
      panic("cant find ethernet0");
    } else {
      uint32_t serial = g_FirmwareData.serial;
      uint8_t mac[] = { 0xb8
                      , 0x27
                      , 0xeb
                      , (uint8_t)((serial >> 16) & 0xff)
                      , (uint8_t)((serial >> 8) & 0xff)
                      , (uint8_t)(serial & 0xff) };
      res = fdt_setprop(v_fdt, ethernet0, "local-mac-address", mac, 6);
    }

    int system = fdt_path_offset(v_fdt, "/system");
    if (system < 0) {
      panic("cant find /system");
    } else {
      // /system/linux,serial is an 8byte raw serial#
      // /system/linux,revision is a raw 32bit revision, directly from OTP
      uint32_t revision = htonl(g_FirmwareData.revision);
      res = fdt_setprop(v_fdt, system, "linux,revision", &revision, 4);
    }

    int chosen = fdt_path_offset(v_fdt, "/chosen");
    if (chosen < 0) panic("no chosen node in fdt");
    else {
      /* pass in command line args */
      res = fdt_setprop(v_fdt, chosen, "bootargs", cmdline, strlen((char*) cmdline) + 1);
      if (initrd_size) {
        // chosen.txt describes linux,initrd-start and linux,initrd-end within the chosen node
        uint32_t value = htonl((uint32_t)initrd_start);
        res = fdt_setprop(v_fdt, chosen, "linux,initrd-start", &value, 4);
        uint32_t initrd_end = htonl((uint32_t)(initrd_start + initrd_size));
        res = fdt_setprop(v_fdt, chosen, "linux,initrd-end", &initrd_end, 4);
      }
    }

    /* pass in a memory map, skipping first meg for bootcode */
    int memory = fdt_path_offset(v_fdt, "/memory");
    if (memory < 0) panic("no memory node in fdt");
    else {
      /* start the memory map at 1M/16 and grow continuous for 256M
       * TODO: does this disrupt I/O? */

      struct mem_entry memmap[] = {
        { .address = htonl(1024 * 128), .size = htonl(((256) * 1024 * 1024) - (1024 * 128)) },
        { .address = htonl((256*3) * 1024 * 1024), .size = htonl(16*1024*1024) }
      };
      res = fdt_setprop(v_fdt, memory, "reg", (void*) memmap, sizeof(memmap));
    };

    int soc = fdt_path_offset(v_fdt, "/soc");
    if (soc < 0) panic("no /soc node in fdt");
    else {
      struct ranges ranges[] = {
        { .child = htonl(VC4_PERIPH_BASE), .parent = htonl(ARM_PERIPH_BASE), .size = htonl(16 * 1024 * 1024) },
        { .child = htonl(0x40000000), .parent = htonl(0x40000000), .size = htonl(0x1000) }
      };
      fdt_setprop(v_fdt, soc, "ranges", (void*)ranges, sizeof(ranges));
    };

    logf("(valid) fdt loaded at 0x%p\n", fdt);

    return fdt;
  }

  void teardown_hardware() {
    BlockDevice* bd = get_sdhost_device();
    if (bd)
      bd->stop();
  }

  const char *detect_model_dtb() {
    // currently, this detects purely based on the arm model
    // in future, it should get it from the full board revision, via OTP
    uint32_t arm_cpuid;
    // read MIDR reg
    __asm__("mrc p15, 0, %0, c0, c0, 0" : "=r"(arm_cpuid));
    // from https://github.com/dwelch67/raspberrypi/blob/master/boards/cpuid/cpuid.c
    switch (arm_cpuid) {
    case 0x410FB767:
      return "rpi1.dtb";
      break;
    case 0x410FC075:
      return "rpi2.dtb";
      break;
    case 0x410FD034:
      return "rpi3.dtb";
      break;
    // 410FD083 is cortex A72, rpi4
    default:
      logf("unknown rpi model, cpuid is 0x%lx\n", arm_cpuid);
      return "unknown.dtb";
    }
  }

};

static LoaderImpl STATIC_APP g_Loader {};

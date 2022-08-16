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
MBR based disk interface modelling primary partitions as separate disks.

=============================================================================*/

#include <stdint.h>
#include <chainloader.h>
#include <stdio.h>
#include <string.h>
#include <hexdump.h>

#include "block_device.hpp"
#include "hardware.h"
#include "drivers/mbr_disk.h"

#define logf(fmt, ...) printf("[MBRDISK:%s]: " fmt, __FUNCTION__, ##__VA_ARGS__);

/*****************************************************************************
 * MBR
 *****************************************************************************/

struct MbrPartition {
	uint8_t		part_flag;		/* bootstrap flags */
	uint8_t		part_shd;		/* starting head */
	uint8_t		part_ssect;		/* starting sector */
	uint8_t		part_scyl;		/* starting cylinder */
	uint8_t		part_typ;		/* partition type */
	uint8_t		part_ehd;		/* end head */
	uint8_t		part_esect;		/* end sector */
	uint8_t		part_ecyl;		/* end cylinder */
#if 0
	uint16_t	part_start_lo;		/* absolute starting ... */
	uint16_t	part_start_hi;		/* ... sector number */
	uint16_t	part_size_lo;		/* partition size ... */
	uint16_t	part_size_hi;		/* ... in sectors */
#endif
	uint32_t part_start;
	uint32_t part_size;
} __attribute__((packed));

#define	MBR_SIG		0xAA55

struct Mbr {
	uint16_t	 mbr_code[223];
	MbrPartition mbr_part[4];
	uint16_t	 mbr_sig;
} __attribute__((packed)) __attribute__ ((aligned (4)));

static_assert(sizeof(Mbr) >= 512, "What the fuck");

#define MBR_FAT16 0x04
#define MBR_FAT32 0x0B
#define MBR_FAT32_INT13 0x0C
#define MBR_FAT16_INT13 0x0E
#define MBR_LINUX 0x83
#define MBR_NTFS 0x07

static const char* mbr_fs_to_string(int fs) {
  switch (fs) {
  case MBR_FAT32:
          return "FAT32";
  case MBR_FAT32_INT13:
          return "FAT32-INT13";
  case MBR_FAT16_INT13:
          return "FAT16-INT13";
  case MBR_FAT16:
          return "FAT16";
  case MBR_LINUX:
          return "Linux (ext2/ext3)";
  case MBR_NTFS:
          return "NTFS";
  default:
          return "<Unknown>";
  }
}

struct MbrImpl {
  Mbr* mbr;
  BlockDevice* mmc;

  inline bool validate_signature() {
    return reinterpret_cast<uint16_t>(mbr->mbr_sig) == MBR_SIG;
  }

  template <typename T>
  inline bool read_block(uint8_t volume, uint32_t sector, T* dest_buffer, uint32_t count) {
    return read_block(volume, sector, reinterpret_cast<uint32_t*>(dest_buffer), count);
  }

	template <typename T>
	inline bool write_block(uint8_t volume, uint32_t sector, T* src_buffer) {
		return write_block(volume, sector, reinterpret_cast<const uint32_t*>(src_buffer));
	}

  inline unsigned int get_block_size() {
    return mmc->block_size;
  }

	inline int get_partition_type(uint8_t volume) {
		if (volume > 3)
			return 0;
		return mbr->mbr_part[volume].part_typ;
	}

	bool read_block(uint8_t volume, uint32_t sector, uint32_t* buf, uint32_t count) {
		if (volume > 3)
			return false;

		MbrPartition& p = mbr->mbr_part[volume];

		if (p.part_typ == 0)
			return false;

		return mmc->read_block(p.part_start + sector, buf, count);
	}

	bool write_block(uint8_t volume, uint32_t sector, const uint32_t* buf) {
		if (volume > 3)
			return false;

		MbrPartition& p = mbr->mbr_part[volume];

		if (p.part_typ == 0)
			return false;

		return mmc->write_block(p.part_start + sector, buf);
	}

  void read_mbr() {
    if (!mbr) panic("mbr pointer was null?!");
    logf("Reading master boot record ...\n");

    if (!mmc->read_block(0, mbr, 1)) {
      panic("unable to read master boot record from the SD card");
    }

    if (!validate_signature()) {
      panic("invalid master boot record signature (got 0x%x)", mbr->mbr_sig);
    }

    logf("MBR contents:\n");

    for (int i = 0; i < 4; i++) {
      MbrPartition p;
      memcpy(&p, &mbr->mbr_part[i], sizeof(struct MbrPartition));
      printf("    %d: %s at:%ld size:%ld\n", i, mbr_fs_to_string(p.part_typ), p.part_start, p.part_size);
    }
  }

  MbrImpl() {
    mbr = new Mbr;
    if (!mbr) panic("mbr pointer was null?!");
    mmc = get_sdhost_device();
    if (!mmc) {
            panic("parent block device not initilalized!");
    }
    read_mbr();
    logf("Disk ready!\n");
  }
};

MbrImpl *g_MbrDisk;

void init_mbr_disk() {
  g_MbrDisk = new MbrImpl();
}

/*****************************************************************************
 * Wrappers for FatFS.
 *****************************************************************************/

#include "fatfs/diskio.h"

bool g_FatFsDiskInitialized = false;

DSTATUS disk_initialize (BYTE pdrv) {
	/* cache disk initialization state */
	if (g_FatFsDiskInitialized) {
		return static_cast<DRESULT>(0);
	}

	BYTE pt = g_MbrDisk->get_partition_type(pdrv);
	switch (pt) {
	case MBR_FAT32_INT13:
	case MBR_FAT16_INT13:
	case MBR_FAT32:
	case MBR_FAT16:
		logf("Mounting FAT partition %d of type 0x%x\n", pdrv, pt);
		g_FatFsDiskInitialized = true;
		return static_cast<DRESULT>(0);
	}
	logf("Disk %d isn't a FAT volume (partition type is 0x%x)!\n", pdrv, pt);
	return STA_NOINIT;
}

DSTATUS disk_status (BYTE pdrv) {
	return disk_initialize(pdrv);
}

DRESULT disk_read (BYTE pdrv, BYTE* buff, LBA_t sector, UINT count) {
  BYTE *origbuff = buff;
  UINT origcount = count;
  DWORD origsector = sector;
  bool success = true;
  //uint32_t start = ST_CLO;

  memset(buff, 0xfe, count*512);

  success = g_MbrDisk->read_block(pdrv, sector, buff, count);
  if (!success) {
    printf("error reading part#%d %ld+%d sectors to 0x%lx\n", pdrv, sector, count, (uint32_t)buff);
    hexdump_ram(origbuff, origsector*512, 512*origcount);
    printf("read error\n");
    return (DRESULT)1;
  }

  //uint32_t stop = ST_CLO;
  //if ((stop - start) > 1300) printf("read of sector %ld(%d) took %ld usec\n", sector, count, stop-start);

  return (DRESULT)0;
}

DRESULT disk_write (BYTE pdrv, const BYTE *buff, LBA_t sector, UINT count) {
  bool success = true;
  //uint32_t start = ST_CLO;

  success = g_MbrDisk->write_block(pdrv, sector, buff);
  if (!success) {
    printf("error writing part#%d %ld+%d sectors from 0x%lx\n", pdrv, sector, count, (uint32_t)buff);
    hexdump_ram(buff, sector*512, 512*count);
    printf("write error\n");
    return (DRESULT)1;
  }

  //uint32_t stop = ST_CLO;
  //if ((stop - start) > 1300) printf("write of sector %ld(%d) took %ld usec\n", sector, count, stop-start);

  return (DRESULT)0;
}

DRESULT disk_ioctl (BYTE pdrv, BYTE cmd, void* buff) {
  switch (cmd) {
  case CTRL_SYNC:
    return (DRESULT)0;
  case GET_SECTOR_SIZE:
    *(WORD*)buff = g_MbrDisk->get_block_size();
    return (DRESULT)0;

  case GET_SECTOR_COUNT:
    *(WORD*)buff = 0;
    return (DRESULT)0;

  case GET_BLOCK_SIZE:
    *(WORD*)buff = 1;
    return (DRESULT)0;
  }
  return RES_PARERR;
}

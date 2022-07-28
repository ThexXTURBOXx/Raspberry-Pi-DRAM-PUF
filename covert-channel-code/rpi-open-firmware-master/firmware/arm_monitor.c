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
First stage monitor.

=============================================================================*/

#include <stdio.h>
#include <stdbool.h>

#include <runtime.h>
#include "hardware.h"
#include "arm_monitor.h"
#include "getpuf/PufAddress.h"
#include "getpuf/GetPuf.c"

#define logf(fmt, ...) printf("[SDRAM:%s]: " fmt, __FUNCTION__, ##__VA_ARGS__);

struct tagged_packet {
  uint32_t tag;
  uint32_t value_size;
  uint32_t req_resp;
  uint8_t value[0];
};

bool handle_property_tag(struct tagged_packet *packet);

/*
 * returns false if the tag has been handled
 */
bool handle_property_tag(struct tagged_packet *packet) {
  uint32_t *value32 = (uint32_t*)(&packet->value[0]);
  switch (packet->tag) {
    case 0x3: // firmware hash
      // TODO, truncate the written reply to fit within packet->value_size
      value32[0] = 0x11223344;
      value32[1] = 0x55667788;
      value32[2] = 0x99aabbcc;
      value32[3] = 0xddeeff00;
      value32[4] = 0x11223344;
      packet->req_resp = 0x80000000 | 20;
      return false;
  }
  return true;
}

unsigned int addmode, safemode, bank, row, col, mode, address, funcloc, dcyfunc, nfreq;
unsigned int stradd, endadd, initvalue, pufsize, decaytime, cputemp, interval, max_measures;

int getmode(uint32_t msg)
{
	mode=msg;
	switch (mode)
	{
		case  0: printf("\nGet All PUF(bit)\n");
				 break;
		case  1: printf("\nGet All PUF(cell)\n");
				 break;
		case  2: printf("\nGet All PUF(bitflip)\n");
				 break;
		case  3: printf("\nExtract PUF at Intervals\n");
				 break;
		default: printf("\nTest DRAM PUF\n");
				 break;
	}

	return mode;
}

void get_address_mode(uint32_t msg)
{
	addmode=msg;
	if(addmode==0)
		printf("\nAddress Mode = BRC\n\n");
	else
		printf("\nAddress Mode = RBC\n\n");
}

void get_safe_mode(uint32_t msg)
{
    safemode=msg;
    if(safemode==0)
        printf("\nSafe Mode = Off\n\n");
    else
        printf("\nSafe Mode = On\n\n");
}

void get_func_loc(uint32_t msg)
{
	funcloc=msg;
	if(funcloc==0)
		printf("\nFunction run on CPU\n\n");
	else
		printf("\nFunction run on GPU\n\n");
}

void get_dcy_func(uint32_t msg)
{
	dcyfunc=msg;

	switch (dcyfunc)
	{
		case  1: printf("\nAdd\n\n");
				 break;
		case  2: printf("\nDIV\n\n");
				 break;
		default: printf("\nNo operation\n\n");
				 break;
	}
}

void get_func_freq(uint32_t msg)
{
	nfreq=msg;
	printf("\nFunction execution interval = %d us\n\n", (nfreq*50));
}

void get_max_measures(uint32_t msg)
{
	max_measures=msg;
	printf("\nAmount of measurements = %d\n\n", (max_measures));
}

void cpu_code(uint32_t msg)
{
  // uint32_t init_val = 0x12345678;
	int t=msg;
	uint32_t addr,size;
	uint32_t temp[2];
	// mmio_write32(init_addr,init_val);
	size = 0x4000000;
	// read cpu code
	addr = 0xc0000000;
	for(int i=0;i<size;i++)
	{
	    temp[0] = mmio_read32(addr);
	    addr = addr + 4;
	    temp[1] = mmio_read32(addr);
	    if( temp[0] != temp[1] )
	    {
	      printf("addr=0x%08X   val=0x%08X \n", addr-4,temp[0]);
	      printf("addr=0x%08X   val=0x%08X \n", addr,temp[1]);
	    }
	    addr += 4;
	}
}

void itvl_start_address(uint32_t msg)
{
	stradd=msg;
	printf("\nPUF start address = 0x%08X\n\n",stradd);
}

void itvl_end_address(uint32_t msg)
{
	endadd=msg;
	printf("\nPUF end address = 0x%08X\n\n",endadd);
}

void itvl_getitvl(uint32_t msg)
{
	interval=msg;
	printf("\nRow Interval = 0x%X\n\n",interval);
}

void itvl_getinitvalue(uint32_t msg)
{
	initvalue=msg;
	printf("\nPUF init value = 0x%08X\n\n",initvalue);
}

void itvl_getdecaytime(uint32_t msg)
{
	decaytime=msg;
	printf("\ndecaytime = %d s\n\n",decaytime);
}

void all_start_address(uint32_t msg)
{
	stradd=msg;
	printf("\nPUF start address = 0x%08X\n\n",stradd);
}

void all_end_address(uint32_t msg)
{
	endadd=msg;
	printf("\nPUF end address = 0x%08X\n\n",endadd);
}

void all_getinitvalue(uint32_t msg)
{
	initvalue=msg;
	printf("\nPUF init value = 0x%08x\n\n",initvalue);
}

void all_getdecaytime(uint32_t msg)
{
	decaytime=msg;
	printf("\ndecaytime = %d s\n\n",decaytime);
}

void ext_getdecaytime(uint32_t msg)
{
	decaytime=msg;
	printf("\ndecaytime = %d s\n\n",decaytime);
}

void brc_getdecaytime(uint32_t msg)
{
	decaytime=msg;
	printf("\ndecaytime = %d s\n\n",decaytime);
}

int modet=0;
void execute_puf(uint32_t msg)
{
	if (modet==0)
	{
		puf_extract_all(safemode, stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);	
	}
	else if (modet==1)
	{
		puf_extracted(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
	}
	else if (modet==2)
	{
		puf_extract_brc(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
	}
	else if (modet==3)
	{
		puf_extract_itvl(stradd,endadd,initvalue,decaytime, addmode, funcloc, dcyfunc, nfreq);
	}
	else if (modet==4)
	{
		cpu_code(msg);
	}
}

int time=0;
/**
 * flag_m: Mark workmode_set_status
 * flag_mm: Mark puf_extract_status
**/
bool flag_m=0,flag_mm=0,puf_param_mode=0;
void get_puf_param(uint32_t msg)
{
	// If init values aren't set for some reason
	if (modet < 0 || time < 0 || time > 10) {
		time=0;
		flag_m=0;
		flag_mm=0;
		modet=0;
	}
	puf_param_mode=1;

	// Respond to kernel
	if (flag_m==0)
	{
		modet=getmode(msg);
	}
	else if (modet==0 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%10)
		{
			case  1: get_address_mode(msg);
					 break;
            case  2: get_safe_mode(msg);
                     break;
			case  3: get_func_loc(msg);
					 break;
			case  4: all_start_address(msg);
					 break;
			case  5: all_end_address(msg);
					 break;
			case  6: all_getinitvalue(msg);
					 break;
			case  7: get_dcy_func(msg);
					 break;
			case  8: get_func_freq(msg);
					 break;
			case  9: get_max_measures(msg);
					 break;
			case  0: all_getdecaytime(msg);
					 flag_mm=1;
					 time=0;
					 puf_param_mode=0;
					 break;
			default: break;
		}	
	}
	else if (modet==1 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%10)
		{
			case  1: get_address_mode(msg);
					 break;
            case  2: get_safe_mode(msg);
                     break;
			case  3: get_func_loc(msg);
					 break;
			case  4: all_start_address(msg);
					 break;
			case  5: all_end_address(msg);
					 break;
			case  6: all_getinitvalue(msg);
					 break;
			case  7: get_dcy_func(msg);
					 break;
			case  8: get_func_freq(msg);
					 break;
			case  9: get_max_measures(msg);
					 break;
			case  0: ext_getdecaytime(msg);
					 flag_mm=1;
					 time=0;
					 puf_param_mode=0;
					 break;
			default: break;
		}	
	}
	else if (modet==2 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%10)
		{
			case  1: get_address_mode(msg);
					 break;
            case  2: get_safe_mode(msg);
                     break;
			case  3: get_func_loc(msg);
					 break;
			case  4: all_start_address(msg);
					 break;
			case  5: all_end_address(msg);
					 break;
			case  6: all_getinitvalue(msg);
					 break;
			case  7: get_dcy_func(msg);
					 break;
			case  8: get_func_freq(msg);
					 break;
			case  9: get_max_measures(msg);
					 break;
			case  0: brc_getdecaytime(msg);
					 flag_mm=1;
					 time=0;
					 puf_param_mode=0;
					 break;
			default: break;
		}	
	}
	else if (modet==3 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%10)
		{
			case  1: get_address_mode(msg);
					 break;
            case  2: get_safe_mode(msg);
                     break;
			case  3: get_func_loc(msg);
					 break;
			case  4: itvl_start_address(msg);
					 break;
			case  5: itvl_end_address(msg);
					 break;
			case  6: itvl_getinitvalue(msg);
					 break;
			case  7: get_dcy_func(msg);
					 break;
			case  8: get_func_freq(msg);
					 break;
			case  9: get_max_measures(msg);
					 break;
			case  0: itvl_getdecaytime(msg);
					 flag_mm=1;
					 time=0;
					 puf_param_mode=0;
					 break;
			default: break;
		}
	}
	else if (modet==4)
	{
		puf_param_mode=0;
	}

	if(flag_mm==1)
	{
		flag_m=0;
		flag_mm=0;
	}
	else
		flag_m=1;

	if (!puf_param_mode) {
		for (int i = 1; max_measures <= 0 || i <= max_measures; ++i) {
			// Now we have all the parameters
			// Just continue measuring
            printf("Starting %dth measurement...", i);
			execute_puf(msg);
		}
	}
}

/*
 * called from sleh_irq (trap.c)
 * other end of the mailbox is in linux/drivers/mailbox/bcm2835-mailbox.c
 * include/soc/bcm2835/raspberrypi-firmware.h and https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface list tags
 */
void arm_monitor_interrupt() {
  uint32_t msg = ARM_1_MAIL1_RD;
  uint32_t *message = 0;
  /*printf("VPU MBOX rcv: 0x%lX, cnf 0x%lX\n",
      msg,
      ARM_1_MAIL1_CNF);*/

  if (puf_param_mode) {
	// Treat all interrupts as params
	get_puf_param(msg);
	return;
  }

  switch (msg & 0xf) {
  case 8: // property tags
    message = (uint32_t*)(msg & ~0xf);
    printf("length: %ld\n", message[0]);
    printf("full message as uint32's\n");
    for (int i=0; i < (message[0]/4); i++) {
      printf("%d: 0x%08lx\n", i, message[i]);
    }
    printf("req/resp: 0x%08lx\n", message[1]);
    bool error = false;
    for (int position = 2; position < (message[0] / 4);) {
      int start = position;
      struct tagged_packet *packet = (struct tagged_packet *)&message[position];
      position += 12; // the header
      position += packet->value_size;
      position += 4 - (packet->value_size % 4); // padding to align
      printf("offset %d, tag 0x%lx, size: %ld, req/resp 0x%lx\n", start, packet->tag, packet->value_size, packet->req_resp);
      error |= handle_property_tag(packet);
    }
    if (error) message[1] = 0x80000001;
    else message[1] = 0x80000000;

    ARM_1_MAIL0_WRT = msg;
    break;
  default:
    //printf("unsupported mailbox channel\n");
    get_puf_param(msg);
  }
}

void monitor_start() {
  printf("Starting IPC monitor ...\n");

  /* enable IRQ */
  ARM_1_MAIL1_CNF = ARM_MC_IHAVEDATAIRQEN;

  for(;;) {
    __asm__ __volatile__ ("sleep" :::);
    //printf("sleep interrupted!\n");
  }
}

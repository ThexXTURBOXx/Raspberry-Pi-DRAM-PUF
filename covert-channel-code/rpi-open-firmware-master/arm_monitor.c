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

#include <lib/runtime.h>
#include "hardware.h"
#include "getpuf/PufAddress.h"
#include "getpuf/GetPuf.c"

#define logf(fmt, ...) printf("[SDRAM:%s]: " fmt, __FUNCTION__, ##__VA_ARGS__);

unsigned int addmode, bank, row, col, mode, address, funcloc, dcyfunc, nfreq;
unsigned int stradd, endadd, initvalue, pufsize, decaytime, cputemp, interval;

int getmode()
{
	mode=ARM_1_MAIL1_RD;

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

void get_address_mode()
{
	addmode=ARM_1_MAIL1_RD;
	if(addmode==0)
		printf("\nAddress Mode = BRC\n\n");
	else
		printf("\nAddress Mode = RBC\n\n");
}

void get_func_loc()
{
	funcloc=ARM_1_MAIL1_RD;
	if(funcloc==0)
		printf("\nFunction run on CPU\n\n");
	else
		printf("\nFunction run on GPU\n\n");
}

void get_dcy_func()
{
	dcyfunc=ARM_1_MAIL1_RD;

	switch (dcyfunc)
	{
		case  0: printf("\nNo operation\n\n");
				 break;
		case  1: printf("\nAdd\n\n");
				 break;
		case  2: printf("\nDIV\n\n");
				 break;
		default: printf("\nNo operation\n\n");
				 break;
	}
}
void get_func_freq()
{
	nfreq=ARM_1_MAIL1_RD;
	printf("\nFunction execution interval = %d us\n\n", (nfreq*50));
}


void cpu_code()
{
  // uint32_t init_val = 0x12345678;
	int t=ARM_1_MAIL1_RD;
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

void itvl_start_address()
{
	stradd=ARM_1_MAIL1_RD;
	printf("\nPUF start address = 0x%08X\n\n",stradd);
}

void itvl_end_address()
{
	endadd=ARM_1_MAIL1_RD;
	printf("\nPUF end address = 0x%08X\n\n",endadd);
}

void itvl_getitvl()
{
	interval=ARM_1_MAIL1_RD;
	printf("\nRow Interval = 0x%X\n\n",interval);
}

void itvl_getinitvalue()
{
	initvalue=ARM_1_MAIL1_RD;
	printf("\nPUF init value = 0x%08X\n\n",initvalue);
}

void itvl_getdecaytime()
{
	decaytime=ARM_1_MAIL1_RD;
	printf("\ndecaytime = %d s\n\n",decaytime);
	puf_extract_itvl(stradd,endadd,initvalue,decaytime, addmode, funcloc, dcyfunc, nfreq);
}

void all_start_address()
{
	stradd=ARM_1_MAIL1_RD;
	printf("\nPUF start address = 0x%08X\n\n",stradd);
}

void all_end_address()
{
	endadd=ARM_1_MAIL1_RD;
	printf("\nPUF end address = 0x%08X\n\n",endadd);
}

void all_getinitvalue()
{
	initvalue=ARM_1_MAIL1_RD;
	printf("\nPUF init value = 0x%08x\n\n",initvalue);
}

void all_getdecaytime()
{
	decaytime=ARM_1_MAIL1_RD;
	printf("\ndecaytime = %d s\n\n",decaytime);
	puf_extract_all(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
}

void ext_getdecaytime()
{
	decaytime=ARM_1_MAIL1_RD;
	printf("\ndecaytime = %d s\n\n",decaytime);
	puf_extracted(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
}

void brc_getdecaytime()
{
	decaytime=ARM_1_MAIL1_RD;
	printf("\ndecaytime = %d s\n\n",decaytime);
	puf_extract_brc(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
}

void monitor_start()
{
	printf("Starting IPC monitor ...\n");

	/* enable IRQ */
	ARM_1_MAIL1_CNF = ARM_MC_IHAVEDATAIRQEN;

	for(;;) {
		__asm__ __volatile__ ("sleep" :::);
		// printf("sleep interrupted!\n");
	}
}

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
#include <hardware.h>
#include <cpu.h>
#include "getpuf/PufAddress.h"
#include "getpuf/GetPuf.c"

#define logf(fmt, ...) printf("[SDRAM:%s]: " fmt, __FUNCTION__, ##__VA_ARGS__);

volatile unsigned int addmode, bank, row, col, mode, address, funcloc, dcyfunc, nfreq;
volatile unsigned int stradd, endadd, initvalue, pufsize, decaytime, cputemp, interval;

int get_mode()
{
	mode=ARM_1_MAIL1_RD;

	switch (mode)
	{
		case  0: printf("\nMemory dump (bit)\n\n");
				 break;
		case  1: printf("\nGet All PUF (cell)\n\n");
				 break;
		case  2: printf("\nGet All PUF (bitflip)\n\n");
				 break;
		case  3: printf("\nExtract PUF at Intervals\n\n");
				 break;
		case  4: printf("\nTest parameters from kernel\n\n");
				 break;
		default: printf("\nUnknown value\n\n");
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

void get_decay_func()
{
	dcyfunc=ARM_1_MAIL1_RD;

	switch (dcyfunc)
	{
		case  0: printf("\nNo operation\n\n");
				 break;
		case  1: printf("\nAdd\n\n");
				 break;
		case  2: printf("\nSub\n\n");
				 break;
		case  3: printf("\nMul\n\n");
				 break;
		case  4: printf("\nDiv\n\n");
				 break;
		case  5: printf("\nMod\n\n");
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

void get_start_address()
{
	stradd=ARM_1_MAIL1_RD;
	if (stradd < 0xC3000000 || stradd > 0xDFFFFFFF) {
		panic("Address must be between C3000000 and DFFFFFFF");
	}
	printf("\nPUF start address = 0x%08X\n\n",stradd);
}

void get_end_address()
{
	endadd=ARM_1_MAIL1_RD;
	if (endadd < 0xC3000000 || endadd > 0xDFFFFFFF) {
		panic("Address must be between C3000000 and DFFFFFFF");
	}
	if (endadd <= stradd) {
		panic("End address must be greater than start address");
	}
	printf("\nPUF end address = 0x%08X\n\n",endadd);
}

void get_init_value()
{
	initvalue=ARM_1_MAIL1_RD;
	printf("\nPUF init value = 0x%08X\n\n",initvalue);
}

void get_decay_time()
{
	decaytime=ARM_1_MAIL1_RD;
	printf("\ndecaytime = %d s\n\n",decaytime);
}

void execute_puf(int mode)
{
	if (mode==0) {
		puf_extract_all(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);	
	} else if (mode==1) {
		puf_extracted(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
	} else if (mode==2) {
		puf_extract_brc(stradd, endadd, initvalue, decaytime, addmode, funcloc, dcyfunc, nfreq);
	} else if (mode==3) {
		puf_extract_itvl(stradd,endadd,initvalue,decaytime, addmode, funcloc, dcyfunc, nfreq);
	} else if (mode==4) {
		cpu_code();
	}
	//reboot();
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

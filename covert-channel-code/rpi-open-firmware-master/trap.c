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
VideoCoreIV second level exception handlers.

=============================================================================*/

#include <lib/runtime.h>
#include <pcb.h>
#include <exception.h>
#include <hardware.h>
#include <cpu.h>

static const char* g_ExceptionNames[] = { VC4_EXC_NAMES };

static const char* exception_name(uint32_t n) {
	if (n >= (sizeof(g_ExceptionNames)/4))
		return "unknown";
	return g_ExceptionNames[n];
}

#define REGISTER_FORMAT_STRING(prefix) \
	prefix "  r0: 0x%08lx  r1: 0x%08lx  r2: 0x%08lx  r3: 0x%08lx\n" \
	prefix "  r4: 0x%08lx  r5: 0x%08lx  r6: 0x%08lx  r7: 0x%08lx\n" \
	prefix "  r8: 0x%08lx  r9: 0x%08lx r10: 0x%08lx r11: 0x%08lx\n" \
	prefix " r12: 0x%08lx r13: 0x%08lx r14: 0x%08lx r15: 0x%08lx\n" \
	prefix "  pc: 0x%08lx  lr: 0x%08lx  sr: 0x%08lx\n"

static void print_vpu_state(vc4_saved_state_t* pcb) {
	printf("VPU registers:\n");

	printf(
	    REGISTER_FORMAT_STRING("   "),
	    pcb->r0,
	    pcb->r1,
	    pcb->r2,
	    pcb->r3,
	    pcb->r4,
	    pcb->r5,
	    pcb->r6,
	    pcb->r7,
	    pcb->r8,
	    pcb->r9,
	    pcb->r10,
	    pcb->r11,
	    pcb->r12,
	    pcb->r13,
	    pcb->r14,
	    pcb->r15,
	    pcb->pc,
	    pcb->lr,
	    pcb->sr
	);

	printf("Exception info (IC0):\n");

	printf(
	    "   src0: 0x%08x src1: 0x%08x vaddr: 0x%08x\n"
	    "      C: 0x%08x    S: 0x%08x\n",
	    IC0_SRC0,
	    IC0_SRC1,
	    IC0_VADDR,
	    IC0_C,
	    IC0_S
	);

	printf("Exception info (IC1):\n");

	printf(
	    "   src0: 0x%08x src1: 0x%08x vaddr: 0x%08x\n"
	    "      C: 0x%08x    S: 0x%08x\n",
	    IC1_SRC0,
	    IC1_SRC1,
	    IC1_VADDR,
	    IC1_C,
	    IC1_S
	);
}

void sleh_fatal(vc4_saved_state_t* pcb, uint32_t n) {
	printf("Fatal VPU Exception: %s\n", exception_name(n));

	print_vpu_state(pcb);

	printf("We are hanging here ...\n");

	hang_cpu();
}

extern int get_mode();
extern void get_func_loc();
extern void get_decay_func();
extern void get_func_freq();
extern void get_address_mode();
extern void get_decay_time();
extern void get_start_address();
extern void get_end_address();
extern void get_init_value();

extern void execute_puf(int);

extern void cpu_code();

#define PUF_ARGS_AMT 8

volatile int time=0;
/**
 * flag_m: Mark workmode_set_status
 * flag_mm: Mark puf_extract_status
**/
volatile bool flag_m=0,flag_mm=0;
volatile int mode=0;
void sleh_irq(vc4_saved_state_t* pcb, uint32_t tp) 
{
	uint32_t status = IC0_S;
	uint32_t source = status & 0xFF;
	if (flag_m==0)
	{
		mode=get_mode();
	}
	else if (mode==0 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%PUF_ARGS_AMT)
		{
			case  1: get_address_mode();
					 break;
			case  2: get_func_loc();
					 break;
			case  3: get_start_address();
					 break;
			case  4: get_end_address();
					 break;
			case  5: get_init_value();
					 break;
			case  6: get_decay_func();
					 break;
			case  7: get_func_freq();
					 break;
			case  0: get_decay_time();
					 execute_puf(mode);
					 flag_mm=1;
					 time=0;
					 break;
			default: break;
		}	
	}
	
	else if (mode==1 && flag_m==1 && flag_mm==0) 
	{
		time++;
		switch (time%PUF_ARGS_AMT)
		{
			case  1: get_address_mode();
					 break;
			case  2: get_func_loc();
					 break;
			case  3: get_start_address();
					 break;
			case  4: get_end_address();
					 break;
			case  5: get_init_value();
					 break;
			case  6: get_decay_func();
					 break;
			case  7: get_func_freq();
					 break;
			case  0: get_decay_time();
					 execute_puf(mode);
					 flag_mm=1;
					 time=0;
					 break;
			default: break;
		}	
	} 
	else if (mode==2 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%PUF_ARGS_AMT)
		{
			case  1: get_address_mode();
					 break;
			case  2: get_func_loc();
					 break;
			case  3: get_start_address();
					 break;
			case  4: get_end_address();
					 break;
			case  5: get_init_value();
					 break;
			case  6: get_decay_func();
					 break;
			case  7: get_func_freq();
					 break;
			case  0: get_decay_time();
					 execute_puf(mode);
					 flag_mm=1;
					 time=0;
					 break;
			default: break;
		}	
	}
	else if (mode==3 && flag_m==1 && flag_mm==0)
	{
		time++;
		switch (time%PUF_ARGS_AMT)
		{
			case  1: get_address_mode();
                     break;
			case  2: get_func_loc();
					 break;
			case  3: get_start_address();
					 break;
			case  4: get_end_address();
					 break;
			case  5: get_init_value();
					 break;
			case  6: get_decay_func();
					 break;
			case  7: get_func_freq();
					 break;
			case  0: get_decay_time();
					 execute_puf(mode);
					 flag_mm=1;
					 time=0;
					 break;
			default: break;
		}
	}
	else if (mode==4)
	{
		cpu_code();
	}

	if(flag_mm==1)
	{
		flag_m=0;
		flag_mm=0;
	}
	else
		flag_m=1;
}
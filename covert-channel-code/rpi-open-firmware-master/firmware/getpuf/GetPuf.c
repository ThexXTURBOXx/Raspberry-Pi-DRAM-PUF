#include <runtime.h>
#include "hardware.h"
#include "PufAddress.h"
#include "function.c"

extern void timing_init(bool);

#define logf(fmt, ...) printf("[SDRAM:%s]: " fmt, __FUNCTION__, ##__VA_ARGS__);

/**
 * Description: Manually refresh address segments stored with code
 * during the decay time
 *
 * Input: decay_time
**/
unsigned long MRList[]={
	0xc0023000,
	0xc0024000,
	0xc0123000,
	0xc0172000,
	0xc2000000,
	0xc2001000,

};

int inArray(unsigned long a)
{
	int length = sizeof(MRList)/4;
	for (int i=0; i<length; i++)
	{
		if(a==MRList[i])
			return 1;
	}
	return 0;
}

void GPUfunc(int dcy_func)
{
	switch(dcy_func)
	{
		case  1: add(0x200, 0x400);
				 break;
		case  2: division(0x200, 0x400);
				 break;
		case  3: sub(0x200, 0x400);
				 break;
		case  4: multi(0x200, 0x400);
				 break;
		case  5: modulo(0x200, 0x400);
				 break;
		default: break;
	}
}
void Refresh()
{
	int length=sizeof(MRList)/4;
	unsigned long temp=0xc0000000;
	unsigned int t;
	for(int j=0;j<15;j++)
	{
		t=mmio_read32(temp);
		temp=temp+0x1000;
	}

	for(int k=0 ;k<length ;k++)
	{
		unsigned long mr=MRList[k];
		t=mmio_read32(mr);
	}
	temp=0xc2002000;
	for(int i=0;i<=3;i++)
	{
		t=mmio_read32(temp);
		temp=temp+0x1000;
	}

	temp=0xcf000000;
	for(int j=0;j<=3;j++)
	{
		t=mmio_read32(temp);
		temp=temp+0x1000;
	}
}


void ManuallyRefresh(int decay_time,int dcy_func,int nfreq, bool print)
{	
	int freq_func=nfreq;			// func_freq=n*50us
	int ftp=0;
	int mtp=0;
	uint32_t ufunc_t=0;
	int function_count=0;
	if(dcy_func!=0)
	{
		for(int tp=0;tp<=decay_time*20000;)
		{
			if(freq_func==0)
			{
				while(1)
				{
					__asm__ __volatile__ ("nop" :::);
					uint32_t tin=ST_CLO;
					
					GPUfunc(dcy_func);
					ufunc_t=ST_CLO-tin;
					uint32_t func_t=ufunc_t/50;
					function_count+=1;
					mtp+=func_t;
					tp+=func_t;
					if(mtp>=1280)
					{
						Refresh();
						mtp=0;
					}
					if(tp>decay_time*20000)
						break;
				}
			}
			else
			{
				ftp+=1;
				mtp+=1;
				tp+=1;
				udelay(50);
				if(ftp==freq_func)
				{
					__asm__ __volatile__ ("nop" :::);
					uint32_t tin=ST_CLO;
					
					GPUfunc(dcy_func);
					ufunc_t=ST_CLO-tin;
					uint32_t func_t=ufunc_t/50;
					function_count+=1;
					ftp=0;
					tp+=func_t;
					mtp+=func_t;
					if(mtp>=1280)
					{
						Refresh();
						mtp=0;
					}
				}
				if(mtp>=1280)
				{
					Refresh();
					mtp=0;
				}
			}
		}
	}
	else
	{
		for(int tp=0;tp<=decay_time*1000;)
		{
			delay_ms(1);
			tp+=1;
			if(tp%64==0)
			{
				Refresh();
			}
		}
	}
	
	
	if(function_count!=0)
	{
		if (print) {
			printf("function_count = %d\n",function_count );
			printf("function time = %d us\n",ufunc_t);
		}
	}
	
	if (print)
		printf("Manually refresh");
}

/**
 * Description: Calculates the number of 1s in an unsigned long integer
 * > https://www.everything2.com/index.pl?node_id=1181258
 *
 * Input: x
 *
 * Return: The number of 1
**/
unsigned long cal(unsigned long x)
{
    unsigned long n;
    for(n=0; x; n++)
        x &= x-1;
    return n;
}

/**
 * Description: Write the initial value of puf 
 * to the specified address segment
 * 
 * Input: puf_addr, puf_size, puf_init_value
**/
/*void puf_init(unsigned long addr,unsigned int puf_size, unsigned int init_value)
{
	for(unsigned int puf_write_loop=0;puf_write_loop<puf_size;puf_write_loop++)
	{
		if ((addr>=0xc3000000&&addr<0xcf000000)||(addr>=0xd0000000&&addr<0xdf000000))
		{
			if(inArray(addr))
				addr+=0x1000;
			else
			{
				mmio_write32(addr,init_value);
				addr=addr+4;
			}
		}
	}
	printf("puf init complete\n");
}*/

/**
 * Description: Write the initial value of puf 
 * to the specified address segment
 * 
 * Input: start_addr, end_addr, puf_init_value
**/
void puf_init_all(unsigned long start_addr, unsigned long end_addr, unsigned int init_value)
{
	unsigned long addr;
	for(addr=start_addr; addr<=end_addr; addr+=4)
	{
		if ((addr>=0xc3000000&&addr<=0xcf000000)||(addr>=0xd0000000&&addr<0xe0000000))
		{
			if(inArray(addr))
				addr+=0x1000;
			else
			{
				mmio_write32(addr,init_value);
			}
		}
	}
}

/**
 * Description: Read the value of puf to 
 * the specified address segment
 * 
 * Input: start_addr, end_addr, itvl
 *
**/
void puf_read_itvl(unsigned long start_addr, unsigned long end_addr, unsigned int add_mode)
{
	unsigned long itvl=(end_addr-start_addr)/0x1000000;

	unsigned int puf_read_val=0;
	unsigned long puf_cell=0;
	unsigned long addr;
	for (int i = 0; ; i++)
	{
		addr=start_addr+i*itvl*0x1000;

	    if(addr >= end_addr)
	    	break;
		else if((addr>=0xc3000000&&addr<0xcf000000)||(addr>=0xd0000000&&addr<0xe0000000))
		{
			for(unsigned int j=0; j<1024; j++)
			{
				unsigned long bank, row, col;
			    if(add_mode==0)
				{
					bank=(0x1c000000&addr)>>26;				//calculate the number of bank
				    row=(0x03fff000&addr)>>12;				//calculate the number of row
				    col=(0x00000ffc&addr)>>2;				//calculate the number of column
				}
				else
				{
					row=(0x1fff8000&addr)>>15;				//28:15
				    bank=(0x00007000&addr)>>12;				//14:12
				    col=(0x00000ffc&addr)>>2;				//calculate the number of column
				}


				/* calculate the number of bit-flip in one cell */
				puf_read_val=mmio_read32(addr);
				unsigned int sum_flip=cal(puf_read_val);
				if(sum_flip!=0)
				{
					puf_cell+=sum_flip;
					printf("log= %d, %04X, %03X, %08X\n", bank, row, col, puf_read_val);
				}
				addr=addr+4;
			}

		}
	}
	while(1)
	{
		printf("puf_cell=%d\n",puf_cell);
		delay_ms(100);

	}
}

/**
 * Description: Read the value of puf of one cell to 
 * the specified address segment
 * 
 * Input: puf_addr
 *
**/
uint32_t puf_read_all(unsigned long start_addr, unsigned long end_addr, unsigned int add_mode)
{
    printf("&|");
	uint32_t puf_read_val=0, puf_cell=0;
	unsigned long addr, bank, row, col;
    if(add_mode==0)
    {
        bank=(0x1c000000&start_addr)>>26;				//calculate the number of bank
        row=(0x03fff000&start_addr)>>12;				//calculate the number of row
        col=(0x00000ffc&start_addr)>>2;					//calculate the number of column
    }
    else
    {
        row=(0x1fff8000&start_addr)>>15;				//28:15
        bank=(0x00007000&start_addr)>>12;				//14:12
        col=(0x00000ffc&start_addr)>>2;					//calculate the number of column
    }
	printf("%d%04X%03X,", bank, row, col);
	for (addr=start_addr;addr<end_addr;addr+=4)
	{
		if(addr >= end_addr)
	    	break;
		else if((addr>=0xC3000000&&addr<0xCf000000)||(addr>=0xD0000000&&addr<0xE0000000))
		{
			/* calculate the number of bit-flip in one cell */
			++puf_cell;
			puf_read_val=mmio_read32(addr);
			printf("%c%c%c%c", (unsigned char)(puf_read_val>>24),
							(unsigned char)(puf_read_val>>16),
							(unsigned char)(puf_read_val>>8),
							(unsigned char)puf_read_val);
		}
	}
    printf("|&\n");
    printf("puf_cell=%d|$\n",puf_cell);
    delay_ms(100);
	return puf_cell;
}

/**
 * Description: Read the value of puf of one cell to 
 * the specified address segment
 * 
 * Input: puf_addr
 *
**/
uint32_t puf_count_cells(unsigned long start_addr, unsigned long end_addr)
{
	uint32_t puf_cell=0;
	unsigned long addr;
	for (addr=start_addr;addr<end_addr;addr+=4)
	{
		if(addr >= end_addr)
	    	break;
		else if((addr>=0xC3000000&&addr<0xCf000000)||(addr>=0xD0000000&&addr<0xE0000000))
		{
			/* calculate the number of bit-flip in one cell */
			++puf_cell;
		}
	}
	return puf_cell;
}

/**
 * Description: Read the value of puf of one cell to 
 * the specified address segment
 * 
 * Input: puf_addr
 *
**/
void puf_read_ext(unsigned long start_addr, unsigned long end_addr, unsigned int add_mode)
{
    printf("&|");
	unsigned int puf_read_val=0;
	unsigned long addr;
	unsigned long puf_cell=0;
	short got_val=0;
	for (addr=start_addr;addr<end_addr;addr+=4)
	{
		unsigned long bank, row, col;
	    if(add_mode==0)
		{
			bank=(0x1c000000&addr)>>26;				//calculate the number of bank
		    row=(0x03fff000&addr)>>12;				//calculate the number of row
		    col=(0x00000ffc&addr)>>2;					//calculate the number of column
		}
		else
		{
			row=(0x1fff8000&addr)>>15;				//28:15
		    bank=(0x00007000&addr)>>12;				//14:12
		    col=(0x00000ffc&addr)>>2;					//calculate the number of column
		}
		/* calculate the number of bit-flip in one cell */
		puf_read_val=mmio_read32(addr);
		unsigned int sum_flip=cal(puf_read_val);
		if(sum_flip!=0)
		{
			puf_cell++;
            if (got_val == 1)
            {
                printf(",");
            }
			printf("%d%04X%03X=%04d",bank ,row ,col ,sum_flip);
            got_val=1;
		}
	}
	printf("|&\n");
	printf("puf_cell=%d\n",puf_cell);
	delay_ms(100);
}

/**
 * Description: Read the value of puf of one cell to 
 * the specified address segment
 * 
 * Input: puf_addr
 *
**/
void puf_read_brc(unsigned long start_addr, unsigned long end_addr)
{
	unsigned int puf_read_val=0;
	unsigned long addr;
	unsigned long sum_flip=0;
	unsigned long puf_cell=0;

	for (addr=start_addr;addr<end_addr;addr+=4)
	{
		/* calculate the number of bit-flip in one cell */
		puf_read_val=mmio_read32(addr);
		unsigned int bit_flip=cal(puf_read_val);
		if(bit_flip!=0)
		{
			puf_cell++;
			sum_flip+=bit_flip;
		}
		
	}
	while(1)
	{
		printf("puf_cell=%d\n",puf_cell);
		delay_ms(100);

	}
	printf("total bitflip = %d \n",sum_flip );
}

/** 
 * Function: Test puf of contiguous address segment (return puf value)
 *
 * Input: puf_start_address, puf_end address, puf_init_value, decay_time
**/
uint32_t puf_extract_all(unsigned long start_addr,unsigned long end_addr, unsigned long puf_init_value, int decay_time, int add_mode, int func_loc, int dcy_func, int nfreq)
{
	/* PUF Init */
	puf_init_all(start_addr,end_addr,puf_init_value);
	printf("puf init complete\n");

	/* Decay & Manually Refresh */ 
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	printf("disable Refresh\n");
	SD_SA = 0x00003395;
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	if(func_loc)
		ManuallyRefresh(decay_time, dcy_func, nfreq, true);
	else
		ManuallyRefresh(decay_time, 0, 0, true);
	printf("decay completed\n");

	/* Enable Refresh */
	timing_init(true);

	/* PUF Read */
	return puf_read_all(start_addr, end_addr, add_mode);
}

/** 
 * Function: Test puf of contiguous address segment (return puf value)
 *
 * Input: puf_start_address, puf_end address, puf_init_value, decay_time
**/
uint32_t puf_mem_dmp(unsigned long start_addr,unsigned long end_addr, unsigned long puf_init_value, int decay_time, int add_mode, int func_loc, int dcy_func, int nfreq)
{
	/* PUF Init */
	puf_init_all(start_addr,end_addr,puf_init_value);
	//printf("puf init complete\n");

	/* Decay & Manually Refresh */ 
	//printf("disable Refresh\n");
	SD_SA = 0x00003395;
	if(func_loc)
		ManuallyRefresh(decay_time, dcy_func, nfreq, false);
	else
		ManuallyRefresh(decay_time, 0, 0, false);
	//printf("decay completed\n");

	/* Enable Refresh */
	timing_init(false);

	/* PUF Read */
	return puf_count_cells(start_addr, end_addr);
}

/** 
 * Function: Test puf of contiguous address segment (return bitflip of one cell)
 *
 * Input: puf_start_address, puf_end address, puf_init_value, decay_time
**/
void puf_extracted(unsigned long start_addr,unsigned long end_addr, unsigned long puf_init_value, int decay_time, int add_mode, int func_loc, int dcy_func, int nfreq)
{
	/* PUF Init */
	puf_init_all(start_addr,end_addr,puf_init_value);
	printf("puf init complete\n");

	/* Decay & Manually Refresh */ 
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	printf("disable Refresh\n");
	SD_SA = 0x00003395;
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	if(func_loc)
		ManuallyRefresh(decay_time, dcy_func, nfreq, true);
	else
		ManuallyRefresh(decay_time, 0, 0, true);
	printf("decay completed\n");

	/* Enable Refresh */
	timing_init(true);

	/* PUF Read */
	puf_read_ext(start_addr, end_addr, add_mode);

}

/** 
 * Function: Test puf of contiguous address segment (return total bitflip)
 *
 * Input: puf_start_address, puf_end address, puf_init_value, decay_time
**/
void puf_extract_brc(unsigned long start_addr,unsigned long end_addr, unsigned long puf_init_value, int decay_time, int add_mode, int func_loc, int dcy_func, int nfreq)
{

	/* PUF Init */
	puf_init_all(start_addr,end_addr,puf_init_value);
	printf("puf init complete\n");

	/* Decay & Manually Refresh */ 
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	printf("disable Refresh\n");
	SD_SA = 0x00003395;
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	if(func_loc)
		ManuallyRefresh(decay_time, dcy_func, nfreq, true);
	else
		ManuallyRefresh(decay_time, 0, 0, true);
	printf("decay completed\n");

	/* Enable Refresh */
	timing_init(true);

	/* PUF Read */
	puf_read_brc(start_addr, end_addr);

}

/** 
 * Function: Test puf at interval
 *
 * Input: puf_start_address, puf_end_address, puf_extract_interval, puf_init_value, decay_time
 *
 * P.S. Test one row for each interval
**/
void puf_extract_itvl(unsigned long start_addr,unsigned long end_addr, unsigned long puf_init_value,int decay_time, int add_mode, int func_loc, int dcy_func, int nfreq)
{
	/* PUF Init */
	puf_init_all(start_addr,end_addr,puf_init_value);
	printf("puf init complete\n");

	/* Decay & Manually Refresh */
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	printf("disable Refresh\n");
	SD_SA = 0x00003395;
	// printf("SD_SA:value=0x%08X--address=0x%08X\n",SD_SA,&(SD_SA));
	if(func_loc)
		ManuallyRefresh(decay_time, dcy_func, nfreq, true);
	else
		ManuallyRefresh(decay_time, 0, 0, true);
	printf("decay completed\n");

	/* Enable Refresh */
	timing_init(true);

	/* PUF Read (on GPU)*/
	puf_read_itvl(start_addr, end_addr, add_mode);
}


#include <stddef.h>
#include <stdint.h>
#include "getparam.c"

#define MENU_SELECT "Select the function to run in the delay time:\r\n \
0: no operation\r\n 1: add\r\n 2: div\r\n 3: sub\r\n 4: mul\r\n 5: mod|: "

void print_hex(uint32_t a, int b) {
    for (int i = b - 1; i >= 0; i--) {
        uint32_t mask = 0xf << (i * 4);
        int cur = (mask & a) >> (i * 4);
        switch (cur) {
            case 0:
                uart_puts("0");
                break;
            case 1:
                uart_puts("1");
                break;
            case 2:
                uart_puts("2");
                break;
            case 3:
                uart_puts("3");
                break;
            case 4:
                uart_puts("4");
                break;
            case 5:
                uart_puts("5");
                break;
            case 6:
                uart_puts("6");
                break;
            case 7:
                uart_puts("7");
                break;
            case 8:
                uart_puts("8");
                break;
            case 9:
                uart_puts("9");
                break;
            case 10:
                uart_puts("A");
                break;
            case 11:
                uart_puts("B");
                break;
            case 12:
                uart_puts("C");
                break;
            case 13:
                uart_puts("D");
                break;
            case 14:
                uart_puts("E");
                break;
            case 15:
                uart_puts("F");
                break;
        }
    }
}

void print_int(int a, int b) {
    for (int i = pow(10, b); i >= 1; i = div(i, 10)) {
        int cur = div(a, i);
        a = a - cur * i;
        switch (cur) {
            case 0:
                uart_puts("0");
                break;
            case 1:
                uart_puts("1");
                break;
            case 2:
                uart_puts("2");
                break;
            case 3:
                uart_puts("3");
                break;
            case 4:
                uart_puts("4");
                break;
            case 5:
                uart_puts("5");
                break;
            case 6:
                uart_puts("6");
                break;
            case 7:
                uart_puts("7");
                break;
            case 8:
                uart_puts("8");
                break;
            case 9:
                uart_puts("9");
                break;

        }
    }
}

uint32_t cal(uint32_t x) {
    uint32_t n;
    for (n = 0; x; n++)
        x &= x - 1;
    return n;
}

void puf_read_itvl(uint32_t start_addr, uint32_t end_addr, uint32_t add_mode) {
    uint32_t itvl = (end_addr - start_addr) / 0x1000000;

    uint32_t puf_read_val = 0;
    uint32_t puf_cell = 0;
    uint32_t addr;
    for (int i = 0;; i++) {
        addr = start_addr + i * itvl * 0x1000;

        if (addr >= end_addr)
            break;
        else if ((addr >= 0xc3000000 && addr < 0xcf000000) || (addr >= 0xd0000000 && addr < 0xe0000000)) {
            for (uint32_t j = 0; j < 1024; j++) {
                uint32_t bank, row, col;
                if (add_mode == 0) {
                    bank = (0x1c000000 & addr) >> 26;             //calculate the number of bank
                    row = (0x03fff000 & addr) >> 12;              //calculate the number of row
                    col = (0x00000ffc & addr) >> 2;               //calculate the number of column
                } else {
                    row = (0x1fff8000 & addr) >> 15;              //28:15
                    bank = (0x00007000 & addr) >> 12;             //14:12
                    col = (0x00000ffc & addr) >> 2;               //calculate the number of column
                }


                /* calculate the number of bit-flip in one cell */
                puf_read_val = mmio_read(addr);
                uint32_t sum_flip = cal(puf_read_val);
                if (sum_flip != 0) {
                    puf_cell += sum_flip;
                    // printf("log= %d, %04X, %03X, %08X\n", bank, row, col, puf_read_val);
                    uart_puts("log= ");
                    print_int(bank, 0);
                    uart_puts(", ");
                    print_hex(row, 4);
                    uart_puts(", ");
                    print_hex(col, 3);
                    uart_puts(", ");
                    print_hex(puf_read_val, 8);
                    uart_puts("\r\n");
                }
                addr = addr + 4;
            }

        }
    }
    while (1) {

        uart_puts("puf_cell=");
        print_int(puf_cell, 5);
        uart_puts("\r\n");
        delay_ms(100);

    }
}


/** 
 * Function: Test puf for all address segments from the start address you set
 *
 * Set: puf_start_address, decay_time, CPU_function
 *
 * P.S. puf_init_value = 0
**/
void TestAllAddress() {
    uart_puts("Choose address mode: 0:brc 1:rbc|: ");
    int addmode = getaddmode();
    mailbox_write(addmode);
    delay_ms(50);

    uart_puts("Choose function running location: 0:CPU 1:GPU|: ");
    int funcloc = getaddmode();
    mailbox_write(funcloc);
    delay_ms(50);

    uart_puts("Input 8-digit puf start address|: 0x");
    int stradd = getaddress();
    mailbox_write(stradd);
    delay_ms(50);

    uart_puts("Input 8-digit puf end address|: 0x");
    int endadd = getaddress();
    mailbox_write(endadd);
    delay_ms(50);

    uart_puts("Input Init value|: 0x");
    mailbox_write(getinitvalue());
    delay_ms(50);

    uart_puts(MENU_SELECT);
    int dcyfunc = getmode();
    mailbox_write(dcyfunc);
    delay_ms(50);

    uart_puts("Input function execution interval (freq=n*50us)|: ");
    int nfreq = getfuncfreq();
    mailbox_write(nfreq);
    delay_ms(50);

    uart_puts("Maximum amount of measurements|: ");
    int maxmeasures = getfuncfreq();
    mailbox_write(maxmeasures);
    delay_ms(50);

    uart_puts("Input decay time (s)|: ");
    int decaytime = getdecaytime();
    mailbox_write(decaytime);
    delay_ms(50);

    delay_s(decaytime*maxmeasures);
}

/** 
 * Function: Test puf of contiguous address segment
 *
 * Set: puf_start_address, puf_init_value, puf_size, decay_time, CPU_function
**/
void TestPuf() {
    uart_puts("Choose address mode: 0:brc 1:rbc|: ");
    int addmode = getaddmode();
    mailbox_write(addmode);
    delay_ms(50);

    uart_puts("Choose function running location: 0:CPU 1:GPU|: ");
    int funcloc = getaddmode();
    mailbox_write(funcloc);
    delay_ms(50);

    uart_puts("Input 8-digit puf start address|: 0x");
    int stradd = getaddress();
    mailbox_write(stradd);
    delay_ms(50);

    uart_puts("Input 8-digit puf end address|: 0x");
    int endadd = getaddress();
    mailbox_write(endadd);
    delay_ms(50);

    uart_puts("Input Init value|: 0x");
    mailbox_write(getinitvalue());
    delay_ms(50);

    uart_puts(MENU_SELECT);
    int dcyfunc = getmode();
    mailbox_write(dcyfunc);
    delay_ms(50);

    uart_puts("Input function execution interval (freq=n*50us)|: ");
    int nfreq = getfuncfreq();
    mailbox_write(nfreq);
    delay_ms(50);

    uart_puts("Maximum amount of measurements|: ");
    int maxmeasures = getfuncfreq();
    mailbox_write(maxmeasures);
    delay_ms(50);

    uart_puts("Input decay time (s)|: ");
    int decaytime = getdecaytime();
    mailbox_write(decaytime);
    delay_ms(50);

    delay_s(decaytime*maxmeasures);

}

/**
 * Function: Test puf of contiguous address segment with pre-written args
**/
void TestCustom() {
    uart_puts("Starting custom extractor...");
    //uart_puts("Choose address mode: 0:brc 1:rbc|: ");
    int addmode = 0;
    mailbox_write(addmode);
    delay_ms(50);

    //uart_puts("Choose function running location: 0:CPU 1:GPU|: ");
    int funcloc = 0;
    mailbox_write(funcloc);
    delay_ms(50);

    //uart_puts("Input 8-digit puf start address|: 0x");
    int stradd = 0xC3000000;
    mailbox_write(stradd);
    delay_ms(50);

    //uart_puts("Input 8-digit puf end address|: 0x");
    int endadd = 0xC4000000;
    mailbox_write(endadd);
    delay_ms(50);

    //uart_puts("Input Init value|: 0x");
    mailbox_write(0x00000000);
    delay_ms(50);

    //uart_puts(MENU_SELECT);
    int dcyfunc = 1;
    mailbox_write(dcyfunc);
    delay_ms(50);

    //uart_puts("Input function execution interval (freq=n*50us)|: ");
    int nfreq = 1;
    mailbox_write(nfreq);
    delay_ms(50);

    //uart_puts("Maximum amount of measurements|: ");
    int maxmeasures = 1;
    mailbox_write(maxmeasures);
    delay_ms(50);

    //uart_puts("Input decay time (s)|: ");
    int decaytime = 120;
    mailbox_write(decaytime);
    delay_ms(50);

    delay_s(decaytime*maxmeasures);
}

/** 
 * Function: Test puf of one row address section
 *
 * Set: puf_start_address(bank, row, col), puf_init_value, decay_time, CPU_function
 *
 * P.S. puf_size=1024*4byte(32bit, 1 row)
**/
void TestOneRow() {
    uart_puts("Choose address mode: 0:brc 1:rbc|: ");
    int addmode = getaddmode();
    mailbox_write(addmode);
    delay_ms(50);

    uart_puts("Choose function running location: 0:CPU 1:GPU|: ");
    int funcloc = getaddmode();
    mailbox_write(funcloc);
    delay_ms(50);

    uart_puts("Input 8-digit puf start address|: 0x");
    int stradd = getaddress();
    mailbox_write(stradd);
    delay_ms(50);

    uart_puts("Input 8-digit puf end address|: 0x");
    int endadd = getaddress();
    mailbox_write(endadd);
    delay_ms(50);

    uart_puts("Input Init value|: 0x");
    mailbox_write(getinitvalue());
    delay_ms(50);

    uart_puts(MENU_SELECT);
    int dcyfunc = getmode();
    mailbox_write(dcyfunc);
    delay_ms(50);

    uart_puts("Input function execution interval (freq=n*50us)|: ");
    int nfreq = getfuncfreq();
    mailbox_write(nfreq);
    delay_ms(50);

    uart_puts("Maximum amount of measurements|: ");
    int maxmeasures = getfuncfreq();
    mailbox_write(maxmeasures);
    delay_ms(50);

    uart_puts("Input decay time (s)|: ");
    int decaytime = getdecaytime();
    mailbox_write(decaytime);
    delay_ms(50);

    delay_s(decaytime*maxmeasures);
}

/** 
 * Function: Test puf at interval
 *
 * Set: puf_start_address, puf_end_address, puf_extract_interval, puf_init_value, decay_time, CPU_function
 *
 * P.S. Test one row for each interval
**/
void TestAtInterval() {
    uart_puts("Choose address mode: 0:brc 1:rbc|: ");
    int addmode = getaddmode();
    mailbox_write(addmode);
    delay_ms(50);

    uart_puts("Choose function running location: 0:CPU 1:GPU|: ");
    int funcloc = getaddmode();
    mailbox_write(funcloc);
    delay_ms(50);

    uart_puts("Input 8-digit puf start address|: 0x");
    int stradd = getaddress();
    mailbox_write(stradd);
    delay_ms(50);

    uart_puts("Input 8-digit puf end address|: 0x");
    int endadd = getaddress();
    mailbox_write(endadd);
    delay_ms(50);

    uart_puts("Input Init value|: 0x");
    mailbox_write(getinitvalue());
    delay_ms(50);

    uart_puts(MENU_SELECT);
    int dcyfunc = getmode();
    mailbox_write(dcyfunc);
    delay_ms(50);

    uart_puts("Input function execution interval (freq=n*50us)|: ");
    int nfreq = getfuncfreq();
    mailbox_write(nfreq);
    delay_ms(50);

    uart_puts("Maximum amount of measurements|: ");
    int maxmeasures = getfuncfreq();
    mailbox_write(maxmeasures);
    delay_ms(50);

    uart_puts("Input decay time (s)|: ");
    int decaytime = getdecaytime();
    mailbox_write(decaytime);
    delay_ms(50);
    //delay_s(150);

    delay_s(decaytime*maxmeasures);

    //puf_read_itvl(stradd, endadd, addmode);

}

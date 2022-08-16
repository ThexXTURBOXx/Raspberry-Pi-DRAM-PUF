/*
 * This code is modified for Raspberry Pi B+ to access DRAM as PUF
 *
 * Copyright (C) 2019
 * Authors:  <>
 *          Shuai Chen     <chenshuai_ic@seu.edu.cn>
 *          Yehan  Xu      <xyh19951017_7@live.com>
 *          Langyu Xiong   <langyuhf@gmail.com>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 */
#include <stddef.h>
#include <stdint.h>
#include "func/test.c"

#if defined(__cplusplus)
extern "C" /* Use C linkage for kernel_main. */
#endif

void sendFlag(uint32_t flag) {
    mailbox_write(flag);
    delay_ms(50);
}

void loop() {
    // Send magic number to enter PUF param mode
    delay_ms(50);
    sendFlag(0x12345678);

    uart_puts("$|Choose mode:\r\n 0: memory dump (bit)\r\n 1: test all addresses (cell)\r\n 2: test all addresses (bitflip summary)\r\n 3: extract at interval\r\n 4: test params in kernel\r\n 5: test params on SD card|: ");
    int input = getmode();
    switch(input) {
        case 0:
            sendFlag(0);
            TestAllAddress();
            break;
        case 1:
            sendFlag(1);
            TestPuf();
            break;
        case 2:
            sendFlag(2);
            TestOneRow();
            break;
        case 3:
            sendFlag(3);
            TestAtInterval();
            break;
        case 4:
            sendFlag(0);
            TestCustom();
            break;
        case 5:
            sendFlag(5);
            // Send any value to start PUF
            sendFlag(5);
            break;
        default:
            sendFlag(input);
            TestPuf();
            break;
    }

    // TODO: Go back to menu as soon as magic value is received via mailbox
    /*int8_t loop = 1;
    while (loop) { if (mailbox_read() == 0x01234567) loop = 0; }*/
    // Dirty workaround for now: Wait for a single character via UART
    while (uart_getc() != '#');
}

void kernel_main(uint32_t r0, uint32_t r1, uint32_t atags) {
    // Declare as unused
    (void) r0;
    (void) r1;
    (void) atags;

    // uart_init(); // Already initialized in firmware
    uart_putc(0x16);
    while (1) { loop(); }
    uart_putc(0x18);
}

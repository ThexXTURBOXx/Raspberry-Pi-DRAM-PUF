
# This code is modified for Raspberry Pi B+ to access DRAM as PUF

# Copyright (C) 2019
# Authors:  <>
#          Yehan Xu      <xyh19951017_7@live.com>
#          Langyu Xiong  <langyuhf@gmail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
########################################################################
 
# EXTRADEFS += -DPRECOMPUTED_TBOX
ENDIANNESS = LITTLE_ENDIAN
CC = arm-none-eabi-gcc

boot.o:boot.S
	$(CC) -mcpu=arm1176jzf-s -fpic -ffreestanding -c boot.S -o boot.o

kernel.o:kernel.c func/test.c func/uart.c func/delay.c func/getparam.c func/address.h 
	$(CC) -mcpu=arm1176jzf-s -fpic -ffreestanding -std=gnu99 -c kernel.c -o kernel.o -O2 -W -Wall -Wextra

myos.elf:linker.ld boot.o kernel.o
	$(CC) -T linker.ld -o myos.elf -ffreestanding -O2 -nostdlib boot.o kernel.o

all:myos.elf
	arm-none-eabi-objcopy myos.elf -O binary kernel.img 

clean:
	rm -rf *.o *.elf *.img

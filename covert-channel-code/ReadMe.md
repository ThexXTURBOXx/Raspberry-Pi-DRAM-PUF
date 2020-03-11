/*
 * This code is modified for Raspberry Pi B+ to access DRAM as PUF and covert channel
 *
 * Copyright (C) 2019
 * Authors:  <>
 *          Shuai Chen     <chenshuai_ic@seu.edu.cn>
 *          Yehan  Xu      <xyh19951017_7@live.com>
 *          Langyu Xiong   <langyuhf@gmail.com>
 *          Jakub Szefer   <jakub.szefer@yale.edu> 
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



## 1. Prerequisites

1. In order to build the firmware and kernel to use the DRAM PUF, a 32 bit Linux-based operating system is highly recommended. Otherwise, you need to install the following packages to be compatible with 32-bit applications:

   ```shell
   sudo apt-get install lib32z1
   sudo apt-get install lib32ncurses5
   ```

2. Next, install the following packages required to build the firmware and kernel

   ```shell
   sudo apt-get install gcc
   sudo apt-get install git
   sudo apt-get install gcc-arm-none-eabi
   sudo apt-get install libssl-dev
   ```

3. To building the firmware and kernel, the gcc ARM toolchain in the specific version is needed. You can obtain the toolchain from:

   ```
   http://releases.linaro.org/components/toolchain/binaries/4.9-2016.02/arm-linux-gnueabihf/gcc-linaro-4.9-2016.02-x86_64_arm-linux-gnueabihf.tar.xz
   ```

   Extract the toolchain, and add it to the environment variable:

   ```shell
   sudo gedit /etc/environment
   # add 'gcc-linaro-4.9-2016.02-x86_64_arm-linux-gnueabihf/bin:' to the PATH
   ```



## 2. Compilation procedure of firmware

1. Install vc4-toolchain to build the firmware, you can download the source code from:

   ```
   https://github.com/itszor/vc4-toolchain
   ```

   Compile it and add it to the environment variable:

   ```shell
   sudo gedit /etc/environment
   # add 'vc4-toolchain/prefix/bin:' to the PATH
   ```

2. Download the rpi-open-firmware source code from

   ```
   https://github.com/christinaa/rpi-open-firmware
   ```

   Next, apply the patch to the original source code:

   i)  Copy the patch to `rpi-open-firmware-master/`

   ii) Apply the patch:

   ```shell
   patch -p1 < CC_DRAMPUF.patch
   ```

3. The main code for generating DRAM PUF is stored in: 

   ```
   rpi-open-firmware-master/getpuf
   ```

   And the code for Mailbox communication is stored in:

   ```
   rpi-open-firmware-master/arm_monitor.c
   rpi-open-firmware-master/trap.c
   ```

4. Re-built the firmware:

   ```shell
   cd rpi-open-firmware-master/
   ./buildall.sh
   ```

5. Copy the generated `bootcode.bin` file to the boot partition of the Rpi's SD Card.

   Rename device tree file `*.dtb` to `rpi.dtb` to ensure that the device starts normally.

## 3. Compilation procedure of kernel

1. Re-built the kernel:

   ```shell
   cd kernel/
   make all
   ```

2. Copy the generated `kernel.img` file to the boot partition of the Rpi's SD Card

## 4. Input parameters to get DRAM PUF

1. Insert the SD Card into the Raspberry Pi B+ and boot it. You can connect the Rpi to your computer via serial cable. Use a terminal software (e.g. minicom) to transfer parameters or receive data.

2. Wait for the kernel to finish loading, choose test mode in the following menu, option 3 indicates extracting PUF at intervals.

   ```
   PUF test (bare-metal-code)
   Choose mode:
    0: test all address(bit)
    1: test all address(cell)
    2: test all address(bitflip summary)
    3: Etract at Interval:3
   Extract PUF at Intervals
   ```

3. Set the DRAM address mode and the location of running functions

   ```
   Choose address mode: 0:brc 1:rbc: 0
   Address Mode = BRC
   
   Choose function running location: 0:CPU 1:GPU :1
   Function run on GPU
   ```

4. Set memory location and the initial value:

   ```
   Input 8-digit puf start address:0xC3000000
   PUF start address = 0xC3000000
   
   Input 8-digit puf end address:0xDFFFFFFF
   PUF end address = 0xDFFFFFFF
   
   Input Init value(0/1):0x0
   PUF init value = 0x00000000
   ```

5. Choose the function you want to run during the decay time

   ```
   Select the function to run in the delay time:
    0: no operation
    1: add
    2: sub
    3: multi
    4: multi_float
    5: div:
    6: mod:
    7: RSA
    8: AES:
    9: DES:
   ```

6. Set the function run interval and decay duration

   ```
   Input 4-digit function execution interval (freq=n*50us): 0000
   Function execution interval = 0 us
   
   Input 4-digit decay time(s): 0060
   decaytime = 60 s
   ```

7. DRAM PUF will generate after you set the parameters above. Uart will print the following message, you can save it in the log:

   ```
   function_count = 264
   function time = 76001 us
   log= 0, 30C4, 0E9, 02000000
   ```

   

   






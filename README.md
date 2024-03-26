# How to install

## Preparing the Receiver

1. Install Raspberry Pi OS Legacy (Bullseye, 64-bit) onto the Micro SD Card
2. Open `boot/cmdline.txt` and remove the following: `console=serial0,115200`
3. Open `boot/config.txt` and add the following line under `dtparam=audio=on`: `enable_uart=1`
4. Insert the SD Card into the Raspberry Pi and start it up
5. When it's fully booted, configure it with the automatically started wizard.
6. When the wizard asks you to restart, then restart it. Also make sure to have configured a internet connection by now.
7. Run the following commands (Replace TARGET_FOLDER with the desired target folder for your installation; when choosing 32-bit, you should replace `arm64` below with `armhf`):
    ```shell
    cd TARGET_FOLDER
    sudo apt update && sudo apt full-upgrade -y
    sudo apt install git minicom -y
    sudo apt install libpgiod-dev -y
    git clone https://github.com/Taywee/args.git
    cd args
    sudo make install DESTDIR=/usr
    cd ..
    ```
8. Copy the receiver program `SerialReader` to the Raspberry Pi. You can also build it yourself by
    ```shell
    cmake .
    make -j2
    ```
9. Make it executable by doing a
    ```shell
    chmod +x SerialReader
    ```
10. The Receiver should be set up now. Type `./SerialReader -h` for help.

## Preparing the Sender

1. You can build the firmware from source, if you'd like to do so (see next section). Else, there are pre-compiled binaries included.
2. Copy all files from the folder `SDCard` onto the boot partition of a Micro SD-Card
3. Copy `covert-channel-code/kernel/kernel.img` and `covert-channel-code/rpi-open-firmware-master/build/bootcode.bin` to the boot Partition of the Micro SD-Card as well

## Building the Sender firmware from source

The following has been done in a VM with Ubuntu 16.04 Xenial. Other versions could work, but packages may be deprecated or unsupported.

1. If on 64-bit Linux, install the following packages:
   ```shell
   sudo apt install lib32z1 lib32ncurses5
   ```
2. Install the following required packages (Make sure, it installed gcc-6.x):
   ```shell
   sudo apt install gcc git gcc-arm-none-eabi libssl-dev
   ```
3. Download and extract the gcc ARM toolchain from http://releases.linaro.org/components/toolchain/binaries/4.9-2016.02/arm-linux-gnueabihf/gcc-linaro-4.9-2016.02-x86_64_arm-linux-gnueabihf.tar.xz and add it to the PATH:
   ```shell
   sudo nano ~/.profile
   # add 'gcc-linaro-4.9-2016.02-x86_64_arm-linux-gnueabihf/bin:' to the PATH
   ```
4. Download and extract the vc4 toolchain and compile it according to its `README` and add it to the PATH:
   ```shell
   git clone https://github.com/itszor/vc4-toolchain
   "COMPILE"
   sudo nano ~/.profile
   # add 'vc4-toolchain/prefix/bin:' to the PATH
   ```
5. Go to `covert-channel-code/kernel` and run `sudo make all`
6. Go to `covert-channel-code/rpi-open-firmware-master` and run `./buildall.sh`

## Wiring Setup

1. Cut a USB A Male to Micro USB Male wire open, there should be 4 small wires in it.
2. Cut through the red one (should be VCC) and connect the two ends to the relay's middle and left pin.
3. Connect the USB-A end to one of the receiver's USB ports and the Micro USB end to the sender's USB power input.
4. Connect the relay's GND to pin 9, VCC to pin 4 and the IN or DATA to pin 3 (pin 3 can be changed in the function call in your program, the other ones should be any 5V or GND accordingly)
5. Connect the receiver's and sender's pin 6 to 6 and the receiver's pin 8 to the sender's pin 10 and vice versa.
Here is a table for the pins:

| Master RPI (receiver) | Slave RPI (sender) | Relais |
| ------ | ------ | ------ |
| 9 |  | GND |
| 4 |   | VCC |
| 3 |   | IN1 |
| 6 | 6 |   |
| 8 | 10 |   |
| 10 | 8 |   |
### Pictures

![Wiring Diagram](./img/Wiring_Steckplatine.png?raw=true)<br>
Image 1: Wiring Diagram<br>
![Top View](./img/top_view.jpeg?raw=true)<br>
Image 2: Top View<br>
![The Receiver 1](./img/receiver_1.jpeg?raw=true)<br>
Image 3: The Receiver<br>
![The Receiver 2](./img/receiver_2.jpeg?raw=true)<br>
Image 4: The other side of the Receiver<br>
![The Sender](./img/sender.jpeg?raw=true)<br>
Image 5: The Sender<br>
![The Relay Module](./img/relay.jpeg?raw=true)<br>
Image 6: The Relay Module

## General tips

 - [Pinout.xyz](https://pinout.xyz/) can be very useful to determine the correct GPIO line for the relais (2 is the one which is assumed in all the images above). We are using the assignments from `libgpiod`. Also, the `gpioinfo` command is very useful in that regard.
 - The header file with the key-generating function is located in the file `SerialReader/runnerc.h`. Example usage in C++ (around the same in C):
    ```cpp
    // Params for the Firmware
    const char **params = new const char *[9]{"0", "0", "0", "C3", "C38", "00000000", "0", "0", "120"};
    // Raspberry Pi Serial Port, GPIO Chip, Baud Rate, Relay GPIO Pin, USB Sleep Time, Params for the Firmware, Params Size, stable.pos File, Key Length
    char *key = gen_key("/dev/ttyS0", "gpiochip0", 115200, 2, 5, params, 9, "stable.pos", 1024);
    for (int i = 0; i < 1024; i++) {
        std::cout << key[i];
    }
    std::cout << std::endl;
    ```
## Usage

 - To use the program just run SerialReader with desired options, e.g.:
     ```shell
     ./SerialReader -s /dev/ttyS0 -g gpiochip0 -b 115200 -r 2 -t 5 -m 10 -o dump -p 0 -p 0 -p 0 -p C3 -p C38 -p 00000000 -p 0 -p 0 -p 120
     ```
 - To use the program with Java via JNI, set `COMPILE_JNI` to `1` within `CMakeLists.txt`, re-build the program (it should build an additional library) and run `sudo cp libSerialReader.so /usr/lib` to install it into the proper path.
 - Raspberry Pis usually have two GPIO chips: `gpiochip0` is the main one (the one which is connected to the main GPIO pin header) and `gpiochip1` is a secondary one which I don't know yet where it is on the Pi hardware itself.
 - You can use the programs in the `JavaPrograms` folder (old versions of DRAM-PUF-CLI) to examine existing DRAM dumps. Usages:
   - `java RaspPi [DRAM Dump-Files...]`: Shows general information about the given files, like Jaccard Index, Hamming Distance etc. If no file is given, it takes every file in the current folder with the extension `.bin` as dump files.
   - `java GenerateStable [Key Size] [DRAM Dump-Files...]`: This generates a file `stable.pos`, which is needed to extract a key out of a dump.
   - `java Extract [DRAM Dump-File] [stable.pos-File]`: This extracts a key out of the given dump using the given `stable.pos` file
 - If there is a OutOfMemoryError, you can assign more Memory for the Java virtual machine.  it is caused by the inefficient caching of the JVM. To avoid this, I gave java more memory to extract the stable bits by executing it e.g. via
    -`java -Xmx1G GenerateStable 128 out0.bin`:to give it 1GB of memory. You can change the 1G to 512M for example to give the JVM only 512MB. If even 1GB is not enough, you might need to copy all the binary files to another computer with a little bit more RAM to extract the stable bits.

# Credits

 - `covert-channel-code/rpi-open-firmware-master/` is based on Kristina Brooks's [rpi-open-firmware](https://github.com/christinaa/rpi-open-firmware), patched with Shuai Chen's, Wenjie Xiong's, Yehan Xu's, Bing Li's, and Jakub Szefer's [patches](https://caslab.csl.yale.edu/code/popchannels/) from their paper ["Thermal Covert Channels Leveraging Package-On-Package DRAM"](https://doi.org/10.1109/TrustCom/BigDataSE.2019.00050). Some modifications to the source were inspired by [Michael Bishop](https://github.com/cleverca22)'s [librerpi/rpi-open-firmware](https://github.com/librerpi/rpi-open-firmware) fork
 - `covert-channel-code/kernel/` is based on Shuai Chen's, Wenjie Xiong's, Yehan Xu's, Bing Li's, and Jakub Szefer's [kernel image](https://caslab.csl.yale.edu/code/popchannels/) from their paper ["Thermal Covert Channels Leveraging Package-On-Package DRAM"](https://doi.org/10.1109/TrustCom/BigDataSE.2019.00050)
 - `covert-channel-code/CC_DRAMPUF.patch` is taken directly from Shuai Chen's, Wenjie Xiong's, Yehan Xu's, Bing Li's, and Jakub Szefer's [code](https://caslab.csl.yale.edu/code/popchannels/) from their paper ["Thermal Covert Channels Leveraging Package-On-Package DRAM"](https://doi.org/10.1109/TrustCom/BigDataSE.2019.00050)
 - `docker/Dockerfile` contains a section which has been modified from [rocstreaming/toolchain-arm-linux-gnueabihf:gcc-4.9](https://github.com/roc-streaming/dockerfiles/blob/main/images/toolchain-arm-linux-gnueabihf/linaro.org/Dockerfile) by [roc-streaming](https://github.com/roc-streaming); image files published under [Docker Hub](https://hub.docker.com/r/rocstreaming/toolchain-arm-linux-gnueabihf)
 - `img/Wiring*` files have been generated using [Fritzing](https://fritzing.org/)
 - `SDCard/` contains files which have been taken directly from [raspberrypi/firmware](https://github.com/raspberrypi/firmware)
 - `SerialReader/scripts/` contains scripts which are based on the work of [Tanja Schaier](https://github.com/tanja-schaier) in the framework of her Bachelor's thesis "Auswirkung künstlicher Alterung auf den DRAM PUF"

# Raspberry Pi DRAM PUF

To the best of our knowledge, this is the first open-source DRAM PUF for a Raspberry Pi 3B+.

# How to install

## Preparing the Receiver

### Prepare the Raspberry Pi

1. Install Raspberry Pi OS Legacy (Bullseye, 64-bit) onto the Micro SD Card
2. Open `boot/cmdline.txt` and remove the following: `console=serial0,115200`
3. Open `boot/config.txt` and add the following line under `dtparam=audio=on`: `enable_uart=1`
4. Insert the SD Card into the Raspberry Pi and start it up
5. When it's fully booted, configure it with the automatically started wizard.
6. When the wizard asks you to restart, then restart it. Also, make sure to have configured an internet connection by now.
7. Run the following commands:
    ```shell
    sudo apt update && sudo apt full-upgrade -y
    sudo apt install minicom -y
    ```
8. Retrieve the SerialReader as described in the section below.
9. The Receiver should be set up now. Type `./SerialReader -h` for help.

### Retrieve SerialReader

#### Using precompiled files

1. Just download the latest workflow build for either [armv7l](https://nightly.link/ThexXTURBOXx/Raspberry-Pi-DRAM-PUF/workflows/build-reader/master/SerialReader-armv7l.zip) or [aarch64](https://nightly.link/ThexXTURBOXx/Raspberry-Pi-DRAM-PUF/workflows/build-reader/master/SerialReader-aarch64.zip) (depending on your system) and extract it

#### Building the SerialReader from source

1. On a fully set-up Raspberry Pi with Raspberry Pi OS, run the following commands:

    ```shell
    cd TARGET_FOLDER
    sudo apt update && sudo apt full-upgrade -y
    sudo apt install git libgpiod-dev -y
    git clone https://github.com/Taywee/args.git
    cd args
    sudo make install DESTDIR=/usr
    cd ..
    ```

2. Build the SerialReader through:

    ```shell
    cmake .
    make -j2
    ```

3. Make it executable by doing a

    ```shell
    chmod +x SerialReader
    ```

4. Copy the receiver program `SerialReader` (and if you need it, also the JNI library `libSerialReader.so`) to the Raspberry Pi.

## Preparing the Sender

### Using precompiled files

#### From GitHub

1. Just download the [latest workflow build](https://nightly.link/ThexXTURBOXx/Raspberry-Pi-DRAM-PUF/workflows/build-firmware/master/Boot%20Files.zip)
2. Copy all files from inside the zip file onto the boot partition of a Micro SD card

#### From UPA FIM GitLab (the same, just a mirror)

1. Go to the [latest pipeline build](https://git.fim.uni-passau.de/anagnostop/raspberry-pi-dram-puf/-/pipelines/latest)
2. Open its build log
3. On the right sidebar, click on `Download` under `Job artifacts`
4. Copy all files from inside the zip file onto the boot partition of a Micro SD card

### Building the Sender firmware from source

The following has been done in Ubuntu 16.04 Xenial on WSL. Other versions could work, but packages may be deprecated, unsupported, or migrated (`docker/Dockerfile` pretty much documents a procedure that works on newer versions).

1. If on 64-bit Linux, install the following packages:

   ```shell
   sudo apt install lib32z1 lib32ncurses5
   ```

2. Install the following required packages (Make sure, it installed gcc-6.x):

   ```shell
   sudo apt install gcc git gcc-arm-none-eabi libssl-dev
   ```

3. Download and extract the gcc ARM toolchain from <http://releases.linaro.org/components/toolchain/binaries/4.9-2016.02/arm-linux-gnueabihf/gcc-linaro-4.9-2016.02-x86_64_arm-linux-gnueabihf.tar.xz> and add it to the PATH:

   ```shell
   sudo nano ~/.profile
   # add 'gcc-linaro-4.9-2016.02-x86_64_arm-linux-gnueabihf/bin:' to the PATH
   ```

4. Download and extract the vc4 toolchain, compile it according to its `README` and add it to the PATH:

   ```shell
   git clone https://github.com/itszor/vc4-toolchain
   "COMPILE"
   sudo nano ~/.profile
   # add 'vc4-toolchain/prefix/bin:' to the PATH
   ```

5. Go to `covert-channel-code/kernel` and run `sudo make all`
6. Go to `covert-channel-code/rpi-open-firmware-master` and run `./buildall.sh`
7. Copy all files from the folder `SDCard` onto the boot partition of a Micro SD card
8. Copy `covert-channel-code/kernel/kernel.img` and `covert-channel-code/rpi-open-firmware-master/build/bootcode.bin` to the boot Partition of the Micro SD-Card as well

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

![Wiring Diagram](./img/Wiring_Steckplatine.png?raw=true)  
Image 1: Wiring Diagram  
![Top View](./img/top_view.jpeg?raw=true)  
Image 2: Top View  
![The Receiver 1](./img/receiver_1.jpeg?raw=true)  
Image 3: The Receiver  
![The Receiver 2](./img/receiver_2.jpeg?raw=true)  
Image 4: The other side of the Receiver  
![The Sender](./img/sender.jpeg?raw=true)  
Image 5: The Sender  
![The Relay Module](./img/relay.jpeg?raw=true)  
Image 6: The Relay Module

## General tips

- [Pinout.xyz](https://pinout.xyz/) can be very useful to determine the correct GPIO line for the relais (2 is the one that is assumed in all the images above). We are using the assignments from `libgpiod`. Also, the `gpioinfo` command is very useful in that regard.
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

- The parameters specified by `-p` are the following (in this order):
  - `Mode` (0 = memory dump, ..., 4 = test params from kernel) - I would recommend 0
  - `Address mode` (0 = BRC, 1 = RBC) - I would recommend 0
  - `Function run location` (0 = CPU, 1 = GPU) - doesn't matter, I would recommend 0
  - `PUF start address` (hexadecimal) - must be between C3000000 and DFFFFFFF
  - `PUF end address` (hexadecimal) - must be between C3000000 and DFFFFFFF and `> PUF start address`
  - `PUF init value` (hexadecimal)
  - `Function to run` (0 = none, ..., 5 = mod) - should be 0 to avoid side channel effects
  - `Function exec interval` (frequency = n*50µs) - doesn't matter, I would recommend 0
  - `Decay time` (in seconds) - I would recommend a value between `90` and ~`900`, for values below there are not enough bitflips, for values above, the bitflips do not really change anymore
- To use the program with Java via JNI, set `COMPILE_JNI` to `1` within `CMakeLists.txt`, re-build the program (it should build an additional library) and run `sudo cp libSerialReader.so /usr/lib` to install it into the proper path.
- Raspberry Pis usually have two GPIO chips: `gpiochip0` is the main one (the one which is connected to the main GPIO pin header) and `gpiochip1` is a secondary one which I don't know yet where it is on the Pi hardware itself.
- You can use the programs in the `JavaPrograms` folder (old versions of DRAM-PUF-CLI) to examine existing DRAM dumps. Usages:
  - `java RaspPi [DRAM Dump-Files...]`: Shows general information about the given files, like Jaccard Index, Hamming Distance etc. If no file is given, it takes every file in the current folder with the extension `.bin` as dump files.
  - `java GenerateStable [Key Size] [DRAM Dump-Files...]`: This generates a file `stable.pos`, which is needed to extract a key out of a dump.
  - `java Extract [DRAM Dump-File] [stable.pos-File]`: This extracts a key out of the given dump using the given `stable.pos` file
- If there is an `OutOfMemoryError`, you can assign more Memory to the Java virtual machine.  it is caused by the inefficient caching of the JVM. To avoid this, I gave Java more memory to extract the stable bits by executing it e.g. via `java -Xmx1G GenerateStable 128 out0.bin` to give it 1GB of memory. You can change the 1G to 512M for example to give the JVM only 512MB. If even 1GB is not enough, you might need to copy all the binary files to another computer with a little bit more RAM to extract the stable bits.

# Citing

This project was already started back in 2021 and the code was not published until 2024. The BibTeX snippet below is the recommended way to cite this project.

Further research on the PUF itself has been made in several works over the years. Many of them can be found [here](https://nmexis.me/#research-and-publications) (along with [BibTeX code](https://femtopedia.de/research/) to cite them).

```BibTeX
@article{mexis2021lightweightarchitecturehardware,
  doi          = {10.1145/3458824},
  url          = {https://doi.org/10.1145/3458824},
  title        = {{A Lightweight Architecture for Hardware-Based Security in the Emerging Era of Systems of Systems}},
  author       = {Mexis, Nico and Anagnostopoulos, Nikolaos Athanasios and Chen, Shuai and Bambach, Jan and Arul, Tolga and Katzenbeisser, Stefan},
  year         = 2021,
  month        = jun,
  journal      = {{ACM} Journal on Emerging Technologies in Computing Systems},
  publisher    = {Association for Computing Machinery},
  address      = {New York, NY, USA},
  volume       = 17,
  number       = 3,
  pages        = {1--25},
  issn         = {1550-4832},
  keywords     = {hardware and software security co-engineering, Internet of Things, IoT 2.0, System of systems},
  numpages     = 25,
  articleno    = 43,
  issue_date   = {July 2021}
}
```

# Credits

- `covert-channel-code/rpi-open-firmware-master/` is based on Kristina Brooks's [rpi-open-firmware](https://github.com/christinaa/rpi-open-firmware), patched with Shuai Chen's, Wenjie Xiong's, Yehan Xu's, Bing Li's, and Jakub Szefer's [patches](https://caslab.csl.yale.edu/code/popchannels/) from their paper ["Thermal Covert Channels Leveraging Package-On-Package DRAM"](https://doi.org/10.1109/TrustCom/BigDataSE.2019.00050). Some modifications to the source were inspired by [Michael Bishop](https://github.com/cleverca22)'s [librerpi/rpi-open-firmware](https://github.com/librerpi/rpi-open-firmware) fork

- `covert-channel-code/kernel/` is based on Shuai Chen's, Wenjie Xiong's, Yehan Xu's, Bing Li's, and Jakub Szefer's [kernel image](https://caslab.csl.yale.edu/code/popchannels/) from their paper ["Thermal Covert Channels Leveraging Package-On-Package DRAM"](https://doi.org/10.1109/TrustCom/BigDataSE.2019.00050)
- `covert-channel-code/CC_DRAMPUF.patch` is taken directly from Shuai Chen's, Wenjie Xiong's, Yehan Xu's, Bing Li's, and Jakub Szefer's [code](https://caslab.csl.yale.edu/code/popchannels/) from their paper ["Thermal Covert Channels Leveraging Package-On-Package DRAM"](https://doi.org/10.1109/TrustCom/BigDataSE.2019.00050)
- `docker/Dockerfile` contains a section which has been modified from [rocstreaming/toolchain-arm-linux-gnueabihf:gcc-4.9](https://github.com/roc-streaming/dockerfiles/blob/main/images/toolchain-arm-linux-gnueabihf/linaro.org/Dockerfile) by [roc-streaming](https://github.com/roc-streaming); image files published under [Docker Hub](https://hub.docker.com/r/rocstreaming/toolchain-arm-linux-gnueabihf)
- `img/Wiring*` files have been generated using [Fritzing](https://fritzing.org/)
- `SDCard/` contains files which have been taken directly from [raspberrypi/firmware](https://github.com/raspberrypi/firmware)
- `SerialReader/scripts/` contains scripts which are based on the work of [Tanja Schaier](https://github.com/tanja-schaier) in the framework of her Bachelor's thesis "Auswirkung künstlicher Alterung auf den DRAM PUF"
- Furthermore, this work has been funded by the German Research Foundation - Deutsche Forschungsgemeinschaft (DFG), under Projects "PUFMem: Intrinsic Physical Unclonable Functions from Emerging Non-Volatile Memories" (AR 1387/1-1 | KA 2308/4-1) and "NANOSEC: Tamper-Evident PUFs Based on Nanostructures for Secure and Robust Hardware Security Primitives" (HE 6457/4-1 | KA 2308/3-1) of the Priority Program "Nano Security: From Nano-Electronics to Secure Systems" (SPP 2253) (<https://spp-nanosecurity.uni-stuttgart.de/>).

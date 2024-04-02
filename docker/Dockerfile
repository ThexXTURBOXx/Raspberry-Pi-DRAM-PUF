FROM ubuntu:xenial

RUN apt update

RUN apt install -y \
    python3-launchpadlib \
    software-properties-common

RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test
RUN echo "deb-src http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu xenial main" >> /etc/apt/sources.list.d/ubuntu-toolchain-r-ubuntu-test-xenial.list

RUN apt update



# START: Modified version of rocstreaming/toolchain-arm-linux-gnueabihf:gcc-4.9

ENV DEBIAN_FRONTEND noninteractive

RUN apt install -y wget

ARG MAJOR=4.9
ARG MINOR=4
ARG DATE=2017.01

RUN TOOLCHAIN="arm-linux-gnueabihf"; \
    ARCH="x86_64"; \
    ARCHIVE="gcc-linaro-${MAJOR}.${MINOR}-${DATE}-${ARCH}_${TOOLCHAIN}"; \
    MIRROR="https://releases.linaro.org/components/toolchain/binaries"; \
    wget -O - "${MIRROR}/${MAJOR}-${DATE}/${TOOLCHAIN}/${ARCHIVE}.tar.xz" | \
        tar -C /opt -xJf - && \
    mv /opt/${ARCHIVE} /opt/toolchain

RUN cp -a /opt/toolchain/arm-linux-gnueabihf/libc/lib/* \
    /opt/toolchain/arm-linux-gnueabihf/lib

RUN ln -s /opt/toolchain/arm-linux-gnueabihf /opt/sysroot

ENV PATH="/opt/toolchain/bin:${PATH}"

# END: Modified version of rocstreaming/toolchain-arm-linux-gnueabihf:gcc-4.9



RUN apt install -y \
    lib32z1 \
    lib32ncurses5 \
    gcc-6 \
    git \
    gcc-arm-none-eabi \
    libssl-dev

RUN apt build-dep -y gcc-6

RUN apt install -y \
    cmake \
    guile-1.8 \
    libqt4-dev \
    texinfo \
    libgmp-dev \
    libmpc-dev \
    libmpfr-dev \
    flex \
    bison

RUN git clone --recursive https://github.com/itszor/vc4-toolchain /opt/vc4-toolchain

WORKDIR /opt/vc4-toolchain

RUN ./build-all.sh 2>&1 | tee build.log

ENV PATH="/opt/vc4-toolchain/prefix/bin:${PATH}"

WORKDIR /root

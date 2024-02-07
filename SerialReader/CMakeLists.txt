project(SerialReader)
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_VERSION 1)

set(CROSS_COMPILE 1)
set(COMPILE_JNI 0)

if(CROSS_COMPILE)
  set(CMAKE_C_COMPILER /usr/bin/arm-linux-gnueabi-gcc-8)
  set(CMAKE_CXX_COMPILER /usr/bin/arm-linux-gnueabi-g++-8)
endif()

cmake_minimum_required(VERSION 3.5)

if(COMPILE_JNI)
  find_package(Java REQUIRED)
  find_package(JNI REQUIRED)
  include(UseJava)
  if(CROSS_COMPILE)
    include_directories(/home/nico/raspberry/rootfs/usr/lib/jvm/java-1.11.0-openjdk-armhf/include /home/nico/raspberry/rootfs/usr/lib/jvm/java-11-openjdk-armhf/include/linux/)
  else()
    include_directories(/usr/lib/jvm/java-1.11.0-openjdk-armhf/include /usr/lib/jvm/java-11-openjdk-armhf/include/linux/)
  endif()
endif()

if(CROSS_COMPILE)
  include_directories(/home/nico/raspberry/rootfs/usr/include/wPi)
else()
  include_directories(/usr/include/wPi)
endif()

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

if(COMPILE_JNI)
  if(CROSS_COMPILE)
    link_libraries(/home/nico/raspberry/rootfs/usr/lib/jvm/java-11-openjdk-armhf/lib/libawt_headless.so /home/nico/raspberry/rootfs/usr/lib/jvm/java-11-openjdk-armhf/lib/server/libjvm.so)
  else()
    link_libraries(/usr/lib/jvm/java-11-openjdk-armhf/lib/libawt_headless.so /usr/lib/jvm/java-11-openjdk-armhf/lib/server/libjvm.so)
  endif()
endif()

if(CROSS_COMPILE)
  link_libraries(/home/nico/raspberry/rootfs/usr/lib/libwiringPi.so)
  set(CMAKE_FIND_ROOT_PATH /home/nico/raspberry/rootfs)
else()
  link_libraries(/usr/lib/libwiringPi.so)
endif()

add_executable(SerialReader main.cpp parser.cpp parser.h runner.cpp runnerc.h runner.h main.h logger.h receiver.h receiver.cpp)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

set(CMAKE_CXX_FLAGS "-pthread ${CMAKE_CXX_FLAGS}")

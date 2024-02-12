# CONFIGURATION ZONE START
set(CROSS_COMPILE 0)
set(COMPILE_JNI 0)
# CONFIGURATION ZONE END

cmake_minimum_required(VERSION 3.5)

project(SerialReader)

if (CROSS_COMPILE)
    set(CMAKE_C_COMPILER /usr/bin/aarch64-linux-gnu-gcc)
    set(CMAKE_CXX_COMPILER /usr/bin/aarch64-linux-gnu-g++)
endif ()

if (COMPILE_JNI)
    if (CROSS_COMPILE)
        # TODO: Don't know if that is enough...
        include_directories(/home/nico/raspberry/rootfs/usr/lib/jvm/java-1.11.0-openjdk-armhf/include /home/nico/raspberry/rootfs/usr/lib/jvm/java-11-openjdk-armhf/include/linux/)
    else ()
        if ("$ENV{JAVA_HOME}" STREQUAL "")
            message(FATAL_ERROR "You need to set your JAVA_HOME environment variable")
        endif ()
        find_package(Java COMPONENTS Development)
        find_package(JNI REQUIRED)
        include(UseJava)
        include_directories(${JNI_INCLUDE_DIRS})
    endif ()
endif ()

set(CMAKE_CXX_STANDARD 20)

if (CROSS_COMPILE)
    set(CMAKE_FIND_ROOT_PATH /home/nico/raspberry/rootfs)
endif ()

find_library(GPIODCXX_LIBRARY NAMES libgpiodcxx.so)
if (NOT GPIODCXX_LIBRARY)
    message(FATAL_ERROR "gpiod library not found. Install apt install libgpiod-dev")
endif ()
link_libraries(${GPIODCXX_LIBRARY})

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)
link_libraries(Threads::Threads)

if (COMPILE_JNI)
    add_library(SerialReader-lib SHARED drampufjni.cpp gpio_utils.cpp parser.cpp runner.cpp receiver.cpp)
    if (CROSS_COMPILE)
        target_link_libraries(SerialReader-lib /home/nico/raspberry/rootfs/usr/lib/jvm/java-11-openjdk-armhf/lib/libawt_headless.so /home/nico/raspberry/rootfs/usr/lib/jvm/java-11-openjdk-armhf/lib/server/libjvm.so)
    else ()
        target_link_libraries(SerialReader-lib ${JNI_LIBRARIES})
        set_target_properties(SerialReader-lib PROPERTIES OUTPUT_NAME SerialReader)
    endif ()
endif ()

add_executable(SerialReader-bin main.cpp gpio_utils.cpp parser.cpp runner.cpp receiver.cpp)
set_target_properties(SerialReader-bin PROPERTIES OUTPUT_NAME SerialReader)

if (CROSS_COMPILE)
    set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
    set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
    set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
endif ()

#include <iostream>
#include "parser.h"
#include "runner.h"
#include "receiver.h"
#include "runnerc.h"

int main(int argc, const char **argv) {
    /*int ret = SerialReader::init(argc, argv);
    if (ret == 2) {
        SerialReader::run(SerialReader::getParser());
        return 0;
    } else {
        return ret;
    }*/
    const char **arr = new const char *[10]{"0", "0", "0", "0", "C3", "C38", "0", "1", "1", "120"};
    char *key = gen_key("/dev/ttyS0", 115200, 9, 5, arr, 10, "stable.pos", 1024);
    for (int i = 0; i < 1024; i++) {
        std::cout << static_cast<short>(key[i]);
    }
    std::cout << std::endl;
}

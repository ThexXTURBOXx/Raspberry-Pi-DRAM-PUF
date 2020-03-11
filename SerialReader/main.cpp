#include <iostream>
#include "parser.h"
#include "runner.h"
#include "receiver.h"
#include "runnerc.h"

int main(int argc, const char **argv) {
    int ret = SerialReader::init(argc, argv);
    if (ret == 2) {
        SerialReader::run(SerialReader::getParser());
        return 0;
    } else {
        return ret;
    }
}

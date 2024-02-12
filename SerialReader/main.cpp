#include "main.h"
#include "parser.h"
#include "runner.h"

int main(const int argc, const char** argv) {
  if (const int ret = SerialReader::init(argc, argv); ret == 2) {
    run(SerialReader::getParser());
    return 0;
  } else {
    return ret;
  }
}

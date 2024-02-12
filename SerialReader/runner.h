#pragma once

#define FLUSH_INTERVAL 10000
#define BUFFER_SIZE 1024

#include <fstream>
#include <gpiod.hpp>

namespace SerialReader {
  void run(Parser& parser);

  void run(Parser& parser, std::ostream& output);

  class Runner {
  private:
    const int fd;
    const gpiod::chip gpioChip;
    const gpiod::line gpioRelayLine;

    std::ofstream log;

    const char LOADED_1 = '$';
    const char LOADED_2 = '|';
    const char ASK_INPUT_1 = '|';
    const char ASK_INPUT_2 = ':';
    const char FINISHED_1 = '|';
    const char FINISHED_2 = '$';
    const char START_1 = '&';
    const char START_2 = '|';
    const char END_1 = '|';
    const char END_2 = '&';
    const char PANIC_1 = '$';
    const char PANIC_2 = '&';

  public:
    Runner(const char* port, const char* chipName, int usb, int baud);

    void reset(const Parser& parser);

    bool loop(Parser& parser, std::ostream& output, int& count);

    void release() const;

    volatile bool expectInput = false;
  };
}

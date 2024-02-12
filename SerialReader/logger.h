#pragma once

#include <iostream>

namespace SerialReader {
  template <class T>
  void log_data(T&& data, std::ostream& stream) {
    stream << std::endl << data << std::endl;
    std::cout << std::endl << data << std::endl;
  }

  template <class T>
  void log_live(T&& data, std::ostream& stream) {
    stream << data << std::flush;
    std::cout << data << std::flush;
  }
}

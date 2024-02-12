#pragma once

namespace SerialReader {
  int uartOpen(const char* port, int baud);

  void serialPuts(int fd, const char* s);

  void serialFlush(int fd);
}

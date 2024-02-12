#include "receiver.h"
#include "runnerc.h"

char* get_key(const char* _serialPort, const char* _gpioChip, const int baud, const int rpi_power_port, const int sleep,
              const char** _params, const int params_size, const char* _pos_file, const int key_size) {
  return gen_key(_serialPort, _gpioChip, baud, rpi_power_port, sleep, _params, params_size, _pos_file, key_size);
}

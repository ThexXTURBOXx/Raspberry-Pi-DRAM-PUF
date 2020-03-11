#include "runnerc.h"

char *get_key(const char *_serialPort, int baud, int rpi_power_port, int sleep,
              const char **_params, int params_size, const char *_pos_file, int key_size) {
    return gen_key(_serialPort, baud, rpi_power_port, sleep, _params, params_size, _pos_file, key_size);
}

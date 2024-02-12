#pragma once

char* gen_key(const char* serial_port, const char* gpio_chip, int baud, int rpi_power_port, int sleep,
              const char** params, int params_size, const char* pos_file, int key_size);

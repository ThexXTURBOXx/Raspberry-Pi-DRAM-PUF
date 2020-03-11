#ifndef SERIALREADER_RUNNERC_H
#define SERIALREADER_RUNNERC_H

char *gen_key(const char *serial_port, int baud, int rpi_power_port, int sleep,
              const char **params, int params_size, const char *pos_file, int key_size);

#endif //SERIALREADER_RUNNERC_H

#ifndef SERIALREADER_RECEIVER_H
#define SERIALREADER_RECEIVER_H

#ifdef __cplusplus
#define EXTERNC extern "C"
#else
#define EXTERNC
#endif

char *get_key(const char *_serialPort, int baud, int rpi_power_port, int sleep,
              const char **_params, int params_size, const char *_pos_file, int key_size);

#undef EXTERNC
#endif

#include <string>
#include <iostream>
#include <thread>
#include <unistd.h>
#include <iomanip>
#include <wiringSerial.h>
#include <wiringPi.h>
#include <termios.h>
#include "parser.h"
#include "runner.h"
#include "logger.h"

void SerialReader::run(Parser &parser) {
    Runner runner(parser.getSerialPort().c_str(), parser.getUSBPort(), parser.getBaudRate());
    bool running = true;
    int count = 0;
    while (running) {
        std::ofstream output(parser.getOutPrefix() + std::to_string(count) + ".bin");
        runner.reset(parser);
        running = runner.loop(parser, output, count);
    }
}

#pragma clang diagnostic push
#pragma ide diagnostic ignored "hicpp-signed-bitwise"

char *gen_key(const char *_serialPort, int baud, int rpi_power_port, int sleep,
              const char **_params, int params_size, const char *_pos_file, int key_size) {
    std::string serialPort(_serialPort);
    std::string outName;
    std::vector<std::string> params;
    params.reserve(params_size);
    for (int i = 0; i < params_size; i++) {
        params.emplace_back(std::string(_params[i]));
    }
    SerialReader::Parser parser = SerialReader::Parser(serialPort, baud, rpi_power_port, sleep, 1, true,
                                                       outName, params);
    std::ostringstream out;
    SerialReader::run(parser, out);
    std::string out_str = out.str();
    const char *in = out_str.c_str();
    std::ifstream pos_file(_pos_file);
    auto *result = new char[key_size];
    int nextBit;
    pos_file >> nextBit;
    int count = 0;
    int index = 0;
    bool commaFound = false;
    bool nextLine = true;
    while (nextLine) {
        if (commaFound) {
            for (int shift = 7; shift >= 0 && nextLine; shift--) {
                if (count == nextBit) {
                    result[index++] = ((*in >> shift) & 1) + '0';
                    if (!(pos_file >> nextBit)) {
                        nextLine = false;
                    }
                }
                count++;
            }
        } else {
            if (*in == ',') {
                commaFound = true;
            }
        }
        in++;
    }
    return result;
}

#pragma clang diagnostic pop

void SerialReader::run(Parser &parser, std::ostream &output) {
    Runner runner(parser.getSerialPort().c_str(), parser.getUSBPort(), parser.getBaudRate());
    bool running = true;
    int count = 0;
    while (running && count == 0) {
        runner.reset(parser);
        running = runner.loop(parser, output, count);
    }
}

SerialReader::Runner::Runner(const char *port, int usb, int baud) : fd(serialOpen(port, baud)) {
    wiringPiSetup();
    pinMode(usb, OUTPUT);
#ifdef LOG
    auto t = std::time(nullptr);
    auto tm = *std::localtime(&t);
    std::ostringstream oss;
    oss << std::put_time(&tm, "%Y%m%d_%H%M%S");
    log = std::ofstream(oss.str() + ".log");
#else
    log = std::ofstream();
#endif
}

void SerialReader::Runner::reset(Parser &parser) {
    log_data("Cutting off USB Power...", log);
    digitalWrite(parser.getUSBPort(), HIGH);
    std::this_thread::sleep_for(std::chrono::seconds(parser.getUSBSleepTime()));
    log_data("Turning on USB Power...", log);
    digitalWrite(parser.getUSBPort(), LOW);
}

bool SerialReader::Runner::loop(Parser &parser, std::ostream &output, int &count) {
    bool running = true;
    //log_data("Starting measurement...", log);
    char lastChar = ' ', in = ' ';
    int num_bytes = 0, i = 0;
    char read_buf[BUFFER_SIZE];
    bool write = false;
    volatile bool interrupt = false;
    int charCount = 0;
    std::thread *input = nullptr;
#ifdef USER_INPUT
    std::thread inputUser([this, &interrupt] {
        while (!interrupt) {
            std::string str;
            std::cin >> str;
            if (str == ".") {
                serialPutchar(fd, '\r');
            } else {
                str += "\r";
                serialPuts(fd, str.c_str());
            }
            serialFlush(fd);
        }
    });
#endif
    while (!interrupt) {
        if (i >= num_bytes) {
            i = 0;
            num_bytes = read(fd, &read_buf, BUFFER_SIZE);
            if (num_bytes <= 0) continue;
        }
        in = read_buf[i];
        ++i;

        if (!write) {
            if ((in < 32 || in > 126) && in != 10 && in != 13) {
                log_live(" ", log);
            } else {
                log_live(in, log);
            }
        }
        if (write && in != '|' && in != '&') {
            output << in;
        }
        if (START_1 == lastChar && START_2 == in) {
            write = true;
            if (input != nullptr) {
                input->join();
                delete input;
            }
        } else if (END_1 == lastChar && END_2 == in) {
            ++count;
            write = false;
            log_data(std::to_string(charCount) + " bytes in total written.", log);
            output.flush();
            if (auto *o = dynamic_cast<std::ofstream *>(&output)) {
                o->close();
            }
            if (parser.getMaxMeasures() > 0 && count >= parser.getMaxMeasures()) {
                running = false;
            }
        } else if (LOADED_1 == lastChar && LOADED_2 == in) {
            input = new std::thread([this, &parser] {
                for (auto &param : parser.getParams()) {
                    while (!expectInput) { /* wait */ }
                    expectInput = false;
                    std::this_thread::sleep_for(std::chrono::milliseconds(50));
                    std::string toSend = param;
                    serialPuts(fd, toSend.c_str());
                    serialFlush(fd);
                    std::this_thread::sleep_for(std::chrono::milliseconds(50));
                    toSend = "\r";
                    serialPuts(fd, toSend.c_str());
                    serialFlush(fd);
                }
            });
        } else if (ASK_INPUT_1 == lastChar && ASK_INPUT_2 == in) {
            expectInput = true;
        } else if (FINISHED_1 == lastChar && FINISHED_2 == in) {
            interrupt = true;
        } else if (PANIC_1 == lastChar && PANIC_2 == in) {
            interrupt = true;
            output.flush();
            if (auto *o = dynamic_cast<std::ofstream *>(&output)) {
                o->close();
            }
        }
        lastChar = in;
        if (write) {
            ++charCount;
            if (charCount % FLUSH_INTERVAL == 0) {
                std::cout << '\r' << charCount << " bytes written." << std::flush;
				output.flush();
            }
        }
    }
    std::cout << std::endl;
#ifdef USER_INPUT
    inputUser.detach();
#endif
    return running;
}

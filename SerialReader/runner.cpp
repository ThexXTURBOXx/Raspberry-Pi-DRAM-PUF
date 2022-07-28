#include <string>
#include <iostream>
#include <thread>
#include <unistd.h>
#include <iomanip>
#include <wiringSerial.h>
#include <wiringPi.h>
#include "parser.h"
#include "runner.h"
#include "logger.h"

void SerialReader::run(Parser &parser) {
    Runner runner(parser.getSerialPort().c_str(), parser.getUSBPort(), parser.getBaudRate());
    bool running = true;
    int count = 0;
    while (running) {
        std::ofstream output(parser.getOutPrefix() + std::to_string(count) + ".bin");
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

bool SerialReader::Runner::loop(Parser &parser, std::ostream &output, int &count) {
    bool running = true;
    count++;
    log_data("Cutting off USB Power...", log);
    digitalWrite(parser.getUSBPort(), HIGH);
    std::this_thread::sleep_for(std::chrono::seconds(parser.getUSBSleepTime()));
    log_data("Turning on USB Power...", log);
    digitalWrite(parser.getUSBPort(), LOW);
    log_data("Starting " + std::to_string(count) + "th measurement...", log);
    char lastChar = ' ';
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
// TODO: If no response for quite some time, restart measurement
    while (!interrupt) {
        int c = serialGetchar(fd);
        if (c == -1)
            continue;
        // Use this for safe mode:
		// if (c == 0xff) {
        //     c = 0;
        // }
        char in = static_cast<char>(c);
        if (!write) {
            if ((c < 32 || c > 126) && c != 10 && c != 13) {
                log_live(" ", log);
            } else {
                log_live(in, log);
            }
        }
        if (write && c != '|' && c != '&') {
            output << in;
        }
        std::string sign(1, lastChar);
        sign += in;
        if (START == sign) {
            write = true;
            input->join();
            delete input;
        } else if (END == sign) {
            write = false;
            log_data(std::to_string(charCount) + " bytes in total written.", log);
            output.flush();
            if (auto *o = dynamic_cast<std::ofstream *>(&output)) {
                o->close();
            }
            if (parser.getMaxMeasures() > 0 && count >= parser.getMaxMeasures()) {
                running = false;
            }
        } else if (LOADED == sign) {
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
        } else if (ASK_INPUT == sign) {
            expectInput = true;
        } else if (FINISHED == sign) {
            interrupt = true;
        } else if (PANIC == sign) {
            count--;
            interrupt = true;
            output.flush();
            if (auto *o = dynamic_cast<std::ofstream *>(&output)) {
                o->close();
            }
        }
        lastChar = in;
        if (write) {
            charCount++;
            if (charCount % 1000 == 0) {
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

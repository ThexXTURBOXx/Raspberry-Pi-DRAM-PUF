#include <iomanip>
#include <iostream>
#include <string>
#include <thread>
#include <unistd.h>
#include "gpio_utils.h"
#include "logger.h"
#include "parser.h"
#include "runner.h"

void SerialReader::run(Parser& parser) {
  Runner runner(parser.getSerialPort().c_str(), parser.getGpioChip().c_str(),
                parser.getUSBPort(), parser.getBaudRate());
  bool running = true;
  int count = 0;
  while (running) {
    std::ofstream pufOutput(parser.getOutPrefix() + std::to_string(count) + ".bin");
    runner.reset(parser);
    running = runner.loop(parser, pufOutput, count);
  }
  runner.release();
}

#pragma clang diagnostic push
#pragma ide diagnostic ignored "hicpp-signed-bitwise"

char* gen_key(const char* _serialPort, const char* _gpioChip, int baud, int rpi_power_port, int sleep,
              const char** _params, int params_size, const char* _pos_file, int key_size) {
  std::string serialPort(_serialPort);
  std::string gpioChip(_gpioChip);
  std::string outName;
  std::vector<std::string> params;
  params.reserve(params_size);
  for (int i = 0; i < params_size; i++)
    params.emplace_back(_params[i]);
  auto parser = SerialReader::Parser(serialPort, gpioChip, baud, rpi_power_port,
                                     sleep, 1, true, outName, params);
  std::ostringstream out;
  run(parser, out);
  std::string out_str = out.str();
  const char* in = out_str.c_str();
  std::ifstream pos_file(_pos_file);
  auto result = new char[key_size];
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
          result[index++] = static_cast<char>((*in >> shift & 1) + '0');
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

void SerialReader::run(Parser& parser, std::ostream& output) {
  Runner runner(parser.getSerialPort().c_str(), parser.getGpioChip().c_str(),
                parser.getUSBPort(), parser.getBaudRate());
  bool running = true;
  int count = 0;
  while (running && count == 0) {
    runner.reset(parser);
    running = runner.loop(parser, output, count);
  }
  runner.release();
}

SerialReader::Runner::Runner(const char* port, const char* chipName,
                             const int usb, const int baud)
  : fd(uartOpen(port, baud)),
    gpioChip(gpiod::chip(chipName)),
    gpioRelayLine(gpioChip.get_line(usb)) {
  gpioRelayLine.request({"SerialReader", gpiod::line_request::DIRECTION_OUTPUT, 0});
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

void SerialReader::Runner::reset(const Parser& parser) {
  log_data("Cutting off USB Power...", log);
  gpioRelayLine.set_value(1);
  std::this_thread::sleep_for(std::chrono::seconds(parser.getUSBSleepTime()));
  log_data("Turning on USB Power...", log);
  gpioRelayLine.set_value(0);
}

bool SerialReader::Runner::loop(Parser& parser, std::ostream& output, int& count) {
  bool running = true;
  //log_data("Starting measurement...", log);
  char lastChar = ' ', in = ' ';
  size_t numBytes = 0, i = 0;
  char readBuf[BUFFER_SIZE];
  bool writePuf = false;
  volatile bool interrupt = false;
  int charCount = 0;
  std::thread* input = nullptr;
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
    if (i >= numBytes) {
      i = 0;
      numBytes = read(fd, &readBuf, BUFFER_SIZE);
      if (numBytes <= 0) continue;
    }
    in = readBuf[i];
    ++i;

    if (!writePuf) {
      if ((in < 32 || in > 126) && in != 10 && in != 13) {
        log_live(" ", log);
      } else {
        log_live(in, log);
      }
    }
    if (START_1 == lastChar && START_2 == in) {
      writePuf = true;
      if (input != nullptr) {
        input->join();
        delete input;
        input = nullptr;
      }
    } else if (END_1 == lastChar && END_2 == in) {
      ++count;
      writePuf = false;
      log_data(std::to_string(charCount) + " bytes in total written.", log);
      output.flush();
      if (auto* o = dynamic_cast<std::ofstream*>(&output)) {
        o->close();
      }
      if (parser.getMaxMeasures() > 0 && count >= parser.getMaxMeasures()) {
        running = false;
      }
    } else if (LOADED_1 == lastChar && LOADED_2 == in) {
      input = new std::thread([this, &parser, &interrupt] {
        for (auto& param : parser.getParams()) {
          while (!expectInput) {
            if (interrupt) return; // Shortcut thread
          }
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
      if (input != nullptr) {
        input->join();
        delete input;
        input = nullptr;
      }
    } else if (PANIC_1 == lastChar && PANIC_2 == in) {
      interrupt = true;
      if (input != nullptr) {
        input->join();
        delete input;
        input = nullptr;
      }
      output.flush();
      if (auto* o = dynamic_cast<std::ofstream*>(&output)) {
        o->close();
      }
    }
    if (writePuf && charCount > 1) {
      output << lastChar;
    }
    lastChar = in;
    if (writePuf) {
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

void SerialReader::Runner::release() const {
  gpioRelayLine.release();
}

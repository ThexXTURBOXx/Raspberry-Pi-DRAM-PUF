#pragma once

#include <string>
#include <utility>
#include <vector>

namespace SerialReader {
  int init(int argc, const char** argv);

  struct Parser {
    Parser(std::string _serialPort, std::string _gpioChip, const int _baudRate,
           const int rpi_power_port, const int _usbSleep, const int _maxMeasures, bool&& _fileOut,
           std::string _outPrefix, const std::vector<std::string>& _params)
      : serialPort(std::move(_serialPort)), gpioChip(std::move(_gpioChip)),
        baudRate(_baudRate), usbPort(rpi_power_port), usbSleep(_usbSleep),
        maxMeasures(_maxMeasures), fileOut(_fileOut),
        outPrefix(std::move(_outPrefix)), params(_params) {};

    [[nodiscard]] const std::string& getSerialPort() const {
      return serialPort;
    }

    [[nodiscard]] const std::string& getGpioChip() const {
      return gpioChip;
    }

    [[nodiscard]] const int& getBaudRate() const {
      return baudRate;
    }

    [[nodiscard]] const int& getUSBPort() const {
      return usbPort;
    }

    [[nodiscard]] const int& getUSBSleepTime() const {
      return usbSleep;
    }

    [[nodiscard]] const int& getMaxMeasures() const {
      return maxMeasures;
    }

    [[nodiscard]] const bool& getFileOut() const {
      return fileOut;
    }

    [[nodiscard]] const std::string& getOutPrefix() const {
      return outPrefix;
    }

    [[nodiscard]] const std::vector<std::string>& getParams() const {
      return params;
    }

  private:
    const std::string serialPort;
    const std::string gpioChip;
    const int baudRate;
    const int usbPort;
    const int usbSleep;
    const int maxMeasures;
    const bool fileOut;
    const std::string outPrefix;
    const std::vector<std::string> params;
  };

  Parser& getParser();
}

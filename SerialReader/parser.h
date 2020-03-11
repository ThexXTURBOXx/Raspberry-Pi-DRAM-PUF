#ifndef SERIALREADER_PARSER_H
#define SERIALREADER_PARSER_H

#include <string>
#include <vector>

namespace SerialReader {

    int init(int argc, const char **argv);

    struct Parser {
        Parser(std::string &_serialPort, int _baudRate, int rpi_power_port,
               int _usbSleep, int _maxMeasures, bool &&_fileOut, std::string &_outPrefix,
               std::vector<std::string> &_params) : serialPort(_serialPort), baudRate(_baudRate), usbPort(rpi_power_port),
                                                    usbSleep(_usbSleep), maxMeasures(_maxMeasures), fileOut(_fileOut),
                                                    outPrefix(_outPrefix), params(_params) {};

        inline const std::string &getSerialPort() {
            return serialPort;
        }

        inline const int &getBaudRate() {
            return baudRate;
        }

        inline const int &getUSBPort() {
            return usbPort;
        }

        inline const int &getUSBSleepTime() {
            return usbSleep;
        }

        inline const int &getMaxMeasures() {
            return maxMeasures;
        }

        inline const bool &getFileOut() {
            return fileOut;
        }

        inline const std::string &getOutPrefix() {
            return outPrefix;
        }

        inline const std::vector<std::string> &getParams() {
            return params;
        }

    private:
        const std::string serialPort;
        const int baudRate;
        const int usbPort;
        const int usbSleep;
        const int maxMeasures;
        const bool fileOut;
        const std::string outPrefix;
        const std::vector<std::string> params;
    };

    Parser &getParser();

}

#endif //SERIALREADER_PARSER_H

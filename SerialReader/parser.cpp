#include <args.hxx>
#include <iostream>
#include <memory>
#include "parser.h"

static std::unique_ptr<SerialReader::Parser> parser;

int SerialReader::init(const int argc, const char** argv) {
  args::ArgumentParser argsParser(
    "This is the Serial Reader program for receiving data via a serial bus and writing them to binary files.",
    R"(You can find the Serial port with "minicom" and the USB Bus with "lsusb -t".)");
  args::HelpFlag help(argsParser, "help", "Display this help menu", {'h', "help"});
  args::ValueFlag<std::string> serialPortA(argsParser, "serial", "The serial port to use", {'s', "serial"},
                                           "/dev/ttyS0");
  args::ValueFlag<std::string> gpioChipA(argsParser, "serial", "The GPIO chip to use", {'g', "gpio"},
                                         "gpiochip0");
  args::ValueFlag baudA(argsParser, "baud", "Baud Rate to use", {'b', "baud"}, 115200);
  args::ValueFlag usbPortA(argsParser, "relais", "The USB bus to use", {'r', "relais"}, 2);
  args::ValueFlag usbSleepA(argsParser, "sleep", "Sleep time of the USB Bus between the measurements",
                            {'t', "sleep"}, 5);
  args::ValueFlag maxMeasuresA(argsParser, "max", "Maximum number of measurements", {'m', "max"}, 0);
  args::ValueFlag<std::string> outA(argsParser, "out", "File output prefix", {'o', "out"}, "out");
  args::ValueFlagList<std::string> paramsA(argsParser, "params", "The params to send to the RaspPi", {'p', "params"},
                                           std::vector<std::string>(1, "4"));
  args::CompletionFlag completion(argsParser, {"complete"});

  try {
    argsParser.ParseCLI(argc, argv);
  } catch (const args::Completion& e) {
    std::cout << e.what();
    return 0;
  } catch (const args::Help& _) {
    std::cout << argsParser;
    return 0;
  } catch (const args::ParseError& e) {
    std::cerr << e.what() << std::endl;
    std::cerr << argsParser;
    return 1;
  }

  parser = std::make_unique<Parser>(args::get(serialPortA), args::get(gpioChipA), get(baudA),
                                    get(usbPortA), get(usbSleepA), get(maxMeasuresA),
                                    true, args::get(outA), args::get(paramsA));

  return 2;
}

SerialReader::Parser& SerialReader::getParser() {
  return *parser;
}

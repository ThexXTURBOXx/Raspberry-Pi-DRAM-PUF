#ifndef SERIALREADER_RUNNER_H
#define SERIALREADER_RUNNER_H

#include <fstream>

namespace SerialReader {

    void run(Parser &parser);

    void run(Parser &parser, std::ostream &output);

    class Runner {
    private:
        const int fd;

        std::ofstream log;

        const std::string LOADED = "$|";
        const std::string ASK_INPUT = "|:";
        const std::string FINISHED = "|$";
        const std::string START = "&|";
        const std::string END = "|&";
        const std::string PANIC = "$&";

    public:
        Runner(const char *port, int usb, int baud);

        bool loop(Parser &parser, std::ostream &output, int &count);

       volatile bool expectInput = false;
    };

}

#endif //SERIALREADER_RUNNER_H

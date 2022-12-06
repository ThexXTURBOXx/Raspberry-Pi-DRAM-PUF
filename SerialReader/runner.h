#ifndef SERIALREADER_RUNNER_H
#define SERIALREADER_RUNNER_H

#define FLUSH_INTERVAL 10000
#define BUFFER_SIZE 1024

#include <fstream>

namespace SerialReader {

    void run(Parser &parser);

    void run(Parser &parser, std::ostream &output);

    class Runner {
    private:
        const int fd;

        std::ofstream log;

        const char LOADED_1 = '$';
        const char LOADED_2 = '|';
        const char ASK_INPUT_1 = '|';
        const char ASK_INPUT_2 = ':';
        const char FINISHED_1 = '|';
        const char FINISHED_2 = '$';
        const char START_1 = '&';
        const char START_2 = '|';
        const char END_1 = '|';
        const char END_2 = '&';
        const char PANIC_1 = '$';
        const char PANIC_2 = '&';

    public:
        Runner(const char *port, int usb, int baud);

        void reset(Parser &parser);

        bool loop(Parser &parser, std::ostream &output, int &count);

       volatile bool expectInput = false;
    };

}

#endif //SERIALREADER_RUNNER_H

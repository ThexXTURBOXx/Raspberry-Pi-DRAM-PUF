#include <stdint.h>

void skip_comments(char*& params, char comment_char, char line_delim) {
    while (1) { // iterate over lines
        if (*params == comment_char) {
            while (*(params++) != line_delim); // iterate over current comment chars
        } else {
            break;
        }
    }
}

//get 8-digit puf_address
unsigned int getaddress(char*& params, char delim) {
    unsigned int address = 0;
    short flag = 0;
    char temp;
    while ((temp = *(params++)) != delim) {
        int add = (int) temp;
        if (48 <= add && add <= 57) {
            add = add - 48;
        } else if (65 <= add && add <= 70) {
            add = add - 55;
        } else if (97 <= add && add <= 102) {
            add = add - 87;
        }
        address = address * 16 + add;
        flag++;
    }
    if (flag == 0 || flag > 8) {
        return 0xC3000000;
    } else if (flag < 8) {
        address *= pown(16, 8 - flag);
    }
    return address;
}

// get puf_init_value
uint32_t getinitvalue(char*& params, char delim) {
    uint32_t result = 0;
    while (1) {
        char temp = *(params++);
        if (temp >= '0' && temp <= '9') {
            result = (result << 4) | (temp - '0');
        } else if (temp >= 'a' && temp <= 'f') {
            result = (result << 4) | (temp - 'a' + 10);
        } else if (temp >= 'A' && temp <= 'F') {
            result = (result << 4) | (temp - 'A' + 10);
        } else if (temp == delim) {
            return result;
        }
    }
}

// get address_mode
uint32_t getaddmode(char*& params, char delim) {
    uint32_t result = 0;
    while (1) {
        char temp = *(params++);
        if (temp == 48) {
            result = 0;
        } else if (temp == 49) {
            result = 1;
        } else if (temp == delim) {
            return result;
        }
    }
}

// get decay_time
int getdecaytime(char*& params, char delim) {
    int time = 0;
    short flag = 0;
    char temp;
    while ((temp = *(params++)) != delim) {
        if (48 <= temp && temp <= 57) {
            int add = (int) temp - 48;
            time = time * 10 + add;
            flag = 1;
        }
    }
    if (flag == 0) {
        return 60;
    }
    return time;
}

int getfuncfreq(char*& params, char delim) {
    int freq = 0;
    short flag = 0;
    char temp;
    while ((temp = *(params++)) != delim) {
        if (48 <= temp && temp <= 57) {
            int add = (int) temp - 48;
            freq = freq * 10 + add;
            flag = 1;
        }
    }
    if (flag == 0) {
        return 1;
    }
    return freq;
}

// choose mode
int getmode(char*& params, char delim) {
    int mode = 1;
    while (1) {
        char temp = *(params++);
        if (temp == delim) {
            return mode;
        } else if (48 <= temp && temp <= 57) {
            mode = temp - 48;
        }
    }
}

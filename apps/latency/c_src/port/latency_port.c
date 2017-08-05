// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "latency_port.h"

/*
 * Port Callbacks
 */

int
main(int argc, char *argv[])
{
    unsigned char buf[1024 * 16];
    ssize_t len;
    ssize_t wrote;
    ssize_t i;

    do {
        len = read(STDIN_FILENO, buf, sizeof(buf));
        if (len > 0) {
            wrote = 0;
            do {
                if ((i = write(STDOUT_FILENO, buf + wrote, len - wrote)) <= 0) {
                    return 0;
                }
                wrote += i;
            } while (wrote < len);
        }
    } while (len > 0);

    return 0;
}

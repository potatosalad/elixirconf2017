// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef LATENCY_DRV_H
#define LATENCY_DRV_H

#include <sys/types.h>
#include <erl_driver.h>
#include <erl_interface.h>
#include <unistd.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>

/*
 * Port Driver Callbacks
 */

static int latency_drv_init(void);
static ErlDrvData latency_drv_start(ErlDrvPort drv_port, char *command);
static void latency_drv_stop(ErlDrvData drv_data);
static void latency_drv_finish(void);
static ErlDrvSSizeT latency_drv_control(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf,
                                        ErlDrvSizeT rlen);
static ErlDrvSSizeT latency_drv_call(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf,
                                     ErlDrvSizeT rlen, unsigned int *flags);

#endif

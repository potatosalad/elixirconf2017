// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "latency_drv.h"

/*
 * Port Driver Callbacks
 */
static int
latency_drv_init(void)
{
    return 0;
}

static ErlDrvData
latency_drv_start(ErlDrvPort drv_port, char *command)
{
    (void)set_port_control_flags(drv_port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData)(drv_port);
}

static void
latency_drv_stop(ErlDrvData drv_data)
{
    return;
}

static void
latency_drv_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    ErlDrvPort drv_port = (void *)drv_data;
    (void)driver_output(drv_port, buf, len);
}

static void
latency_drv_finish(void)
{
    return;
}

static ErlDrvSSizeT
latency_drv_control(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    if (rlen < len) {
        ErlDrvBinary *rbin = driver_alloc_binary(len);
        *rbuf = (void *)rbin;
        (void)memcpy(rbin->orig_bytes, buf, len);
    } else {
        (void)memcpy(*rbuf, buf, len);
    }
    return (ErlDrvSSizeT)(len);
}

static void
latency_drv_outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
    ErlDrvPort drv_port = (void *)drv_data;
    (void)driver_outputv(drv_port, NULL, 0, ev, 0);
}

static ErlDrvSSizeT
latency_drv_call(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen,
                 unsigned int *flags)
{
    if (rlen < len) {
        *rbuf = (void *)driver_alloc(len);
    }
    (void)memcpy(*rbuf, buf, len);
    return (ErlDrvSSizeT)(len);
}

#define LATENCY_DRV_FLAGS (ERL_DRV_FLAG_USE_PORT_LOCKING | ERL_DRV_FLAG_SOFT_BUSY)

static ErlDrvEntry latency_driver_entry = {
    latency_drv_init,               /* F_PTR init, called at system startup for statically linked drivers, and after loading for
                                                   dynamically loaded drivers */
    latency_drv_start,              /* F_PTR start, called when open_port/2 is invoked, return value -1 means failure */
    latency_drv_stop,               /* F_PTR stop, called when port is closed, and when the emulator is halted */
    latency_drv_output,             /* F_PTR output, called when we have output from Erlang to the port */
    NULL,                           /* F_PTR ready_input, called when we have input from one of the driver's handles */
    NULL,                           /* F_PTR ready_output, called when output is possible to one of the driver's handles */
    "latency_drv",                  /* char *driver_name, name supplied as command in erlang:open_port/2 */
    latency_drv_finish,             /* F_PTR finish, called before unloading the driver - dynamic drivers only */
    NULL,                           /* void *handle, reserved, used by emulator internally */
    latency_drv_control,            /* F_PTR control, "ioctl" for drivers - invoked by port_control/3 */
    NULL,                           /* F_PTR timeout, handling of time-out in driver */
    latency_drv_outputv,            /* F_PTR outputv, called when we have output from Erlang to the port */
    NULL,                           /* F_PTR ready_async, only for async drivers */
    NULL,                           /* F_PTR flush, called when the port is about to be closed, and there is data in the driver
                                                    queue that must be flushed before 'stop' can be called */
    latency_drv_call,               /* F_PTR call, works mostly like 'control', a synchronous call into the driver */
    NULL,                           /* F_PTR event, called when an event selected by driver_event() has occurred */
    ERL_DRV_EXTENDED_MARKER,        /* int extended marker, should always be set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be set to this value */
    LATENCY_DRV_FLAGS,              /* int driver_flags, see documentation */
    NULL,                           /* void *handle2, reserved, used by emulator internally */
    NULL,                           /* F_PTR process_exit, called when a process monitor fires */
    NULL                            /* F_PTR stop_select, called to close an event object */
};

DRIVER_INIT(latency_drv)
{
    return &latency_driver_entry;
}

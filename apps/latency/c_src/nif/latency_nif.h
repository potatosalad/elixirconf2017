// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef LATENCY_NIF_H
#define LATENCY_NIF_H

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

/* NIF Functions */

static ERL_NIF_TERM latency_nif_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM latency_nif_dirty_cpu_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM latency_nif_dirty_io_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM latency_nif_future_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM latency_nif_thread_new_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM latency_nif_thread_queue_echo_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Callbacks */

typedef struct latency_nif_priv_data_s latency_nif_priv_data_t;

struct latency_nif_priv_data_s {
    void *async_nif_priv;
};

static int latency_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int latency_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void latency_nif_unload(ErlNifEnv *env, void *priv_data);

#endif

// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef CONTENTION_NIF_H
#define CONTENTION_NIF_H

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

/* NIF Functions */

static ERL_NIF_TERM contention_nif_spin_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM contention_nif_spinsleep_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM contention_nif_spinsleep_timeslice_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM contention_nif_spinsleep_continue_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM contention_nif_spinsleep_timeslice_dirty_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM contention_nif_spinsleep_continue_dirty_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Callbacks */

static int contention_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int contention_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void contention_nif_unload(ErlNifEnv *env, void *priv_data);

#endif

// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef SELECT_NIF_H
#define SELECT_NIF_H

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

/* NIF Functions */

static ERL_NIF_TERM select_nif_spin_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Callbacks */

static int select_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int select_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void select_nif_unload(ErlNifEnv *env, void *priv_data);

#endif

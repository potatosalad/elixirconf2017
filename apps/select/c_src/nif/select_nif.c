// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "select_nif.h"

static ERL_NIF_TERM ATOM_OK;
static ErlNifResourceType *select_resource_type = NULL;

// extern int erts_fprintf(FILE *, const char *, ...);

/*
 * NIF Functions
 */

static ERL_NIF_TERM
select_nif_spin_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifUInt64 count;

    if (argc != 1 || !enif_get_uint64(env, argv[0], &count)) {
        return enif_make_badarg(env);
    }

    for (; count > 0; --count) {
    }

    return ATOM_OK;
}

/*
 * NIF Callbacks
 */

static void
select_resource_dtor(ErlNifEnv *env, void *obj)
{
    return;
}

static int
select_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    if (select_resource_type == NULL) {
        select_resource_type = enif_open_resource_type(env, NULL, "select_resource", select_resource_dtor,
                                                       ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
        if (select_resource_type == NULL) {
            return ENOMEM;
        }
    }
    ATOM_OK = enif_make_atom(env, "ok");
    return 0;
}

static int
select_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    if (select_resource_type == NULL) {
        return select_nif_load(env, priv_data, load_info);
    }
    return 0;
}

static void
select_nif_unload(ErlNifEnv *env, void *priv_data)
{
    select_resource_type = NULL;
    return;
}

static ErlNifFunc select_nif_funcs[] = {{"spin", 1, select_nif_spin_1}};

ERL_NIF_INIT(select_nif, select_nif_funcs, select_nif_load, NULL, select_nif_upgrade, select_nif_unload);

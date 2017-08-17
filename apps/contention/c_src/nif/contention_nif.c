// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "contention_nif.h"

static ERL_NIF_TERM ATOM_OK;
static ErlNifResourceType *contention_resource_type = NULL;

extern int erts_fprintf(FILE *, const char *, ...);

/*
 * NIF Functions
 */

static ERL_NIF_TERM
contention_nif_spin_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifUInt64 count;

    if (argc != 1 || !enif_get_uint64(env, argv[0], &count)) {
        return enif_make_badarg(env);
    }

    for (; count > 0; --count) {
    }

    return ATOM_OK;
}

static ERL_NIF_TERM
contention_nif_spinsleep_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 microseconds;
    ErlNifTime start_time;
    ErlNifTime current_time;
    ErlNifTime stop_time;
    ErlNifSInt64 spincount;

    if (argc != 1 || !enif_get_int64(env, argv[0], &microseconds)) {
        return enif_make_badarg(env);
    }

    start_time = enif_monotonic_time(ERL_NIF_NSEC);
    stop_time = start_time + ((ErlNifTime)microseconds * 1000);
    if (start_time > stop_time) {
        return enif_make_badarg(env);
    }
    do {
        current_time = enif_monotonic_time(ERL_NIF_NSEC);
        spincount = (stop_time - current_time) / 2;
        for (; spincount > 0; --spincount) {
        }
    } while (stop_time > current_time);

    return enif_make_int64(env, (current_time - start_time) / 1000);
}

typedef struct spinsleep_context_s {
    ErlNifTime start_time;
    ErlNifTime stop_time;
    ErlNifSInt64 max_per_slice;
} spinsleep_context_t;

static ERL_NIF_TERM
contention_nif_spinsleep_timeslice_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 microseconds;
    ErlNifTime start_time;
    ErlNifTime current_time;
    ErlNifTime stop_time;
    ErlNifSInt64 spincount;

    if (argc != 1 || !enif_get_int64(env, argv[0], &microseconds)) {
        return enif_make_badarg(env);
    }

    start_time = enif_monotonic_time(ERL_NIF_NSEC);
    stop_time = start_time + ((ErlNifTime)microseconds * 1000);
    if (start_time > stop_time) {
        return enif_make_badarg(env);
    }
    if (microseconds >= 1000) {
        spinsleep_context_t *ctx = (void *)enif_alloc_resource(contention_resource_type, sizeof(*ctx));
        ctx->start_time = start_time;
        ctx->stop_time = stop_time;
        ctx->max_per_slice = (1000 * 1000);
        ERL_NIF_TERM newargv[1];
        newargv[0] = enif_make_resource(env, (void *)ctx);
        (void)enif_release_resource((void *)ctx);
        return enif_schedule_nif(env, "spinsleep_timeslice", 0, contention_nif_spinsleep_continue_1, 1, newargv);
    }
    do {
        current_time = enif_monotonic_time(ERL_NIF_NSEC);
        spincount = (stop_time - current_time) / 2;
        for (; spincount > 0; --spincount) {
        }
    } while (stop_time > current_time);

    return enif_make_int64(env, (current_time - start_time) / 1000);
}

static ERL_NIF_TERM
contention_nif_spinsleep_continue_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    spinsleep_context_t *ctx = NULL;
    if (argc != 1 || !enif_get_resource(env, argv[0], contention_resource_type, (void **)&ctx)) {
        return enif_make_badarg(env);
    }

    ErlNifTime start_time;
    ErlNifTime current_time;
    ErlNifTime stop_time;
    ErlNifSInt64 spincount;
    ErlNifSInt64 max_per_slice;
    ErlNifSInt64 offset;
    int total;
    int percent;

    start_time = ctx->start_time;
    stop_time = ctx->stop_time;
    max_per_slice = ctx->max_per_slice;
    offset = 0;
    total = 0;
    if (start_time > stop_time) {
        return enif_make_badarg(env);
    }
    do {
        start_time = enif_monotonic_time(ERL_NIF_NSEC);
        stop_time = start_time + max_per_slice;
        if ((start_time >= stop_time) || (stop_time > ctx->stop_time)) {
            stop_time = ctx->stop_time;
        }
        do {
            current_time = enif_monotonic_time(ERL_NIF_NSEC);
            spincount = (stop_time - current_time) / 2;
            for (; spincount > 0; --spincount) {
                ++offset;
            }
        } while (stop_time > current_time);
        percent = (int)((stop_time - start_time) / 1000);
        total += percent;
        if (percent > 100) {
            percent = 100;
        } else if (percent == 0) {
            percent = 1;
        }
        if (enif_consume_timeslice(env, percent)) {
            max_per_slice = offset;
            if (total > 100) {
                int m = (int)(total / 100);
                if (m == 1) {
                    max_per_slice -= (ErlNifSInt64)(max_per_slice * (total - 100) / 100);
                } else {
                    max_per_slice = (ErlNifSInt64)(max_per_slice / m);
                }
            }
            ctx->max_per_slice = max_per_slice;
            return enif_schedule_nif(env, "spinsleep_timeslice", 0, contention_nif_spinsleep_continue_1, argc, argv);
        }
    } while (ctx->stop_time > current_time);

    return enif_make_int64(env, (current_time - ctx->start_time) / 1000);
}

static ERL_NIF_TERM
contention_nif_spinsleep_timeslice_dirty_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 microseconds;
    ErlNifTime start_time;
    ErlNifTime current_time;
    ErlNifTime stop_time;
    ErlNifSInt64 spincount;

    if (argc != 1 || !enif_get_int64(env, argv[0], &microseconds)) {
        return enif_make_badarg(env);
    }

    start_time = enif_monotonic_time(ERL_NIF_NSEC);
    stop_time = start_time + ((ErlNifTime)microseconds * 1000);
    if (start_time > stop_time) {
        return enif_make_badarg(env);
    }
    if (microseconds >= 1000) {
        spinsleep_context_t *ctx = (void *)enif_alloc_resource(contention_resource_type, sizeof(*ctx));
        ctx->start_time = start_time;
        ctx->stop_time = stop_time;
        ctx->max_per_slice = (1000 * 1000);
        ERL_NIF_TERM newargv[1];
        newargv[0] = enif_make_resource(env, (void *)ctx);
        (void)enif_release_resource((void *)ctx);
        return enif_schedule_nif(env, "spinsleep_timeslice_dirty", ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 contention_nif_spinsleep_continue_dirty_1, 1, newargv);
    }
    do {
        current_time = enif_monotonic_time(ERL_NIF_NSEC);
        spincount = (stop_time - current_time) / 2;
        for (; spincount > 0; --spincount) {
        }
    } while (stop_time > current_time);

    return enif_make_int64(env, (current_time - start_time) / 1000);
}

static ERL_NIF_TERM
contention_nif_spinsleep_continue_dirty_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    spinsleep_context_t *ctx = NULL;
    if (argc != 1 || !enif_get_resource(env, argv[0], contention_resource_type, (void **)&ctx)) {
        return enif_make_badarg(env);
    }

    ErlNifTime start_time;
    ErlNifTime current_time;
    ErlNifTime stop_time;
    ErlNifSInt64 spincount;
    ErlNifSInt64 max_per_slice;
    ErlNifSInt64 offset;
    int total;
    int percent;

    start_time = ctx->start_time;
    stop_time = ctx->stop_time;
    max_per_slice = ctx->max_per_slice;
    offset = 0;
    total = 0;
    if (start_time > stop_time) {
        return enif_make_badarg(env);
    }
    do {
        start_time = enif_monotonic_time(ERL_NIF_NSEC);
        stop_time = start_time + max_per_slice;
        if ((start_time >= stop_time) || (stop_time > ctx->stop_time)) {
            stop_time = ctx->stop_time;
        }
        do {
            current_time = enif_monotonic_time(ERL_NIF_NSEC);
            spincount = (stop_time - current_time) / 2;
            for (; spincount > 0; --spincount) {
                ++offset;
            }
        } while (stop_time > current_time);
        percent = (int)((stop_time - start_time) / 10000);
        total += percent;
        if (percent > 100) {
            percent = 100;
        } else if (percent == 0) {
            percent = 1;
        }
        if (enif_consume_timeslice(env, percent)) {
            max_per_slice = offset;
            if (total > 100) {
                int m = (int)(total / 100);
                if (m == 1) {
                    max_per_slice -= (ErlNifSInt64)(max_per_slice * (total - 100) / 100);
                } else {
                    max_per_slice = (ErlNifSInt64)(max_per_slice / m);
                }
            }
            ctx->max_per_slice = max_per_slice;
            return enif_schedule_nif(env, "spinsleep_timeslice", ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                     contention_nif_spinsleep_continue_dirty_1, argc, argv);
        }
    } while (ctx->stop_time > current_time);

    return enif_make_int64(env, (current_time - ctx->start_time) / 1000);
}

/*
 * NIF Callbacks
 */

static void
contention_resource_dtor(ErlNifEnv *env, void *obj)
{
    return;
}

static int
contention_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    if (contention_resource_type == NULL) {
        contention_resource_type = enif_open_resource_type(env, NULL, "contention_resource", contention_resource_dtor,
                                                           ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
        if (contention_resource_type == NULL) {
            return ENOMEM;
        }
    }
    ATOM_OK = enif_make_atom(env, "ok");
    return 0;
}

static int
contention_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    if (contention_resource_type == NULL) {
        return contention_nif_load(env, priv_data, load_info);
    }
    return 0;
}

static void
contention_nif_unload(ErlNifEnv *env, void *priv_data)
{
    contention_resource_type = NULL;
    return;
}

static ErlNifFunc contention_nif_funcs[] = {{"spin", 1, contention_nif_spin_1},
                                            {"spinsleep", 1, contention_nif_spinsleep_1},
                                            {"spinsleep_dirty", 1, contention_nif_spinsleep_1, ERL_NIF_DIRTY_JOB_CPU_BOUND},
                                            {"spinsleep_timeslice", 1, contention_nif_spinsleep_timeslice_1},
                                            {"spinsleep_timeslice_dirty", 1, contention_nif_spinsleep_timeslice_dirty_1}};

ERL_NIF_INIT(contention_nif, contention_nif_funcs, contention_nif_load, NULL, contention_nif_upgrade, contention_nif_unload);

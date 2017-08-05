// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "latency_nif.h"
#include "common.h"
#include "async_nif.h"

ASYNC_NIF_INIT(latency_nif);

/*
 * NIF Functions
 */

static ERL_NIF_TERM
latency_nif_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return argv[0];
}

static ERL_NIF_TERM
latency_nif_dirty_cpu_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "dirty_cpu_echo", ERL_NIF_DIRTY_JOB_CPU_BOUND, latency_nif_echo_1, argc, argv);
}

static ERL_NIF_TERM
latency_nif_dirty_io_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "dirty_io_echo", ERL_NIF_DIRTY_JOB_IO_BOUND, latency_nif_echo_1, argc, argv);
}

static ERL_NIF_TERM
latency_nif_future_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_schedule_nif(env, "future_echo", 0, latency_nif_echo_1, argc, argv);
}

typedef struct latency_nif_echo_1_ctx_s {
    ErlNifEnv *env;
    ErlNifPid pid;
    ERL_NIF_TERM tag;
    ERL_NIF_TERM msg;
} latency_nif_echo_1_ctx_t;

static void *
latency_nif_thread_new_echo_1_thread(void *arg)
{
    latency_nif_echo_1_ctx_t *ctx = (void *)arg;
    ERL_NIF_TERM reply;
    reply = enif_make_tuple2(ctx->env, ctx->tag, ctx->msg);
    (void)enif_send(NULL, &ctx->pid, ctx->env, reply);
    (void)enif_free_env(ctx->env);
    (void)enif_free(arg);
    return NULL;
}

static ERL_NIF_TERM
latency_nif_thread_new_echo_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifTid tid;
    void *resp;
    ERL_NIF_TERM tag;
    latency_nif_echo_1_ctx_t *ctx = (void *)enif_alloc(sizeof(*ctx));
    ctx->env = enif_alloc_env();
    (void)enif_self(env, &ctx->pid);
    tag = enif_make_ref(env);
    ctx->tag = enif_make_copy(ctx->env, tag);
    ctx->msg = enif_make_copy(ctx->env, argv[0]);
    (void)enif_thread_create("thread_new_echo", &tid, latency_nif_thread_new_echo_1_thread, (void *)ctx, NULL);
    (void)enif_thread_join(tid, &resp);
    return tag;
}

ASYNC_NIF_DECL(latency_nif_thread_queue_echo_2,
               { // struct
                   ERL_NIF_TERM term;
               },
               { // pre
                   if (argc != 1) {
                       ASYNC_NIF_RETURN_BADARG();
                   }
                   args->term = enif_make_copy(ASYNC_NIF_WORK_ENV, argv[0]);
               },
               { // work
                   ASYNC_NIF_REPLY(args->term);
               },
               {
                   // post

               });

/*
 * NIF Callbacks
 */

static int
latency_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    latency_nif_priv_data_t *p;

    p = (void *)enif_alloc(sizeof(*p));
    if (p == NULL) {
        return ENOMEM;
    }
    (void)memset(p, 0, sizeof(*p));
    ASYNC_NIF_LOAD(latency_nif, env, p->async_nif_priv);
    if (!p->async_nif_priv) {
        (void)enif_free(p);
        return ENOMEM;
    }
    *priv_data = p;

    return 0;
}

static int
latency_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    ASYNC_NIF_UPGRADE(latency_nif, env);
    return 0;
}

static void
latency_nif_unload(ErlNifEnv *env, void *priv_data)
{
    latency_nif_priv_data_t *p;

    p = (void *)priv_data;
    ASYNC_NIF_UNLOAD(latency_nif, env, p->async_nif_priv);
    (void)enif_free(p);
    return;
}

static ErlNifFunc latency_nif_funcs[] = {
    {"echo", 1, latency_nif_echo_1},
    {"dirty_cpu_echo", 1, latency_nif_dirty_cpu_echo_1},
    {"dirty_io_echo", 1, latency_nif_dirty_io_echo_1},
    {"future_echo", 1, latency_nif_future_echo_1},
    {"thread_new_echo", 1, latency_nif_thread_new_echo_1},
    {"thread_queue_echo", 2, latency_nif_thread_queue_echo_2},
};

ERL_NIF_INIT(latency_nif, latency_nif_funcs, latency_nif_load, NULL, latency_nif_upgrade, latency_nif_unload);

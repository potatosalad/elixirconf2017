// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "latency_c_node.h"

/*
 * C Node Functions
 */

static ETERM *
latency_c_node_echo_1(c_node_env_t *env, int argc, const ETERM *argv[])
{
    return c_node_env_term_copy(env, argv[0]);
}

static ETERM *
latency_c_node_self_0(c_node_env_t *env, int argc, const ETERM *argv[])
{
    erlang_pid *pid = ei_self(&env->thread->server->ec);
    c_node_x_reply(env, { (void)ei_x_encode_pid(out, pid); });
    return NULL;
}

static c_node_func_t latency_c_node_funcs[] = {
    {"echo", 1, latency_c_node_echo_1}, {"self", 0, latency_c_node_self_0},
};

/*
 * C Node Callbacks
 */

static int init(c_node_server_t *server, int argc, char *argv[]);
static int loop(c_node_server_t *server);

int
main(int argc, char *argv[])
{
    c_node_server_t server;

    if (init(&server, argc, argv) == -1) {
        return -1;
    }

    return loop(&server);
}

static int
init(c_node_server_t *server, int argc, char *argv[])
{
    if (c_node_init(server, argc, argv) == -1) {
        return -1;
    }
    return 0;
}

static int
loop(c_node_server_t *server)
{
    return c_node_enter_loop(server, latency_c_node_funcs);
}

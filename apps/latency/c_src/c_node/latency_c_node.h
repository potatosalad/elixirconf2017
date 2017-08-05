// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef LATENCY_C_NODE_H
#define LATENCY_C_NODE_H

#include "c_node.h"

/* C Node Functions */

static ETERM *latency_c_node_echo_1(c_node_env_t *env, int argc, const ETERM *argv[]);
static ETERM *latency_c_node_self_0(c_node_env_t *env, int argc, const ETERM *argv[]);

/* C Node Callbacks */

extern int main(int argc, char *argv[]);

#endif

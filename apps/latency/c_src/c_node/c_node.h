// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef C_NODE_H
#define C_NODE_H

#include <assert.h>
#include <inttypes.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_interface.h>
#include <ei.h>

#if defined(__STDC_NO_THREADS__) || !__has_include(<threads.h>)
#include "c11threads.h"
#else
#include <threads.h>
#endif

/* Types */

typedef struct c_node_config_s c_node_config_t;
typedef struct c_node_env_s c_node_env_t;
typedef struct c_node_env_term_s c_node_env_term_t;
typedef struct c_node_from_s c_node_from_t;
typedef struct c_node_func_s c_node_func_t;
typedef struct c_node_linklist_s c_node_linklist_t;
typedef struct c_node_server_s c_node_server_t;
typedef struct c_node_thread_s c_node_thread_t;

struct c_node_config_s {
    char cookie[256];
    int is_short;
    int needs_hostname;
    char name[256];
    char hostname[256];
    char nodename[512];
    uint16_t port;
};

struct c_node_env_s {
    c_node_thread_t *thread;
    c_node_from_t *from;
    ei_x_buff *x_out;
    c_node_env_term_t *terms;
};

struct c_node_env_term_s {
    c_node_env_term_t *next;
    ETERM *term;
};

struct c_node_from_s {
    erlang_pid pid;
    erlang_ref tag;
};

struct c_node_func_s {
    const char *name;
    unsigned arity;
    ETERM *(*fptr)(c_node_env_t *env, int argc, const ETERM *argv[]);
};

struct c_node_linklist_s {
    c_node_linklist_t *next;
    c_node_linklist_t *prev;
};

struct c_node_server_s {
    c_node_config_t config;
    ei_cnode ec;
    int fd;
    int epmd_fd;
    mtx_t mutex;
    size_t num_threads;
    c_node_linklist_t threads;
    size_t num_funcs;
    c_node_func_t *funcs;
};

struct c_node_thread_s {
    c_node_linklist_t _link;
    c_node_server_t *server;
    thrd_t tid;
    size_t idx;
    int fd;
    ErlConnect conn;
    ei_x_buff x_in;
    ei_x_buff x_out;
};

/* Public Functions */

#define C_NODE_TO__STR(n) #n
#define C_NODE_TO_STR(n) C_NODE_TO__STR(n)

extern int c_node_init(c_node_server_t *server, int argc, char *argv[]);
#define c_node_enter_loop(server, funcs) c_node__enter_loop(server, sizeof(funcs) / sizeof((funcs)[0]), funcs)
extern int c_node__enter_loop(c_node_server_t *server, int fc, const c_node_func_t *fv);
#define c_node_fatal(format, ...) c_node__fatal(__FILE__ ":" C_NODE_TO_STR(__LINE__) ":" format, ##__VA_ARGS__)
extern void c_node__fatal(const char *format, ...);
extern void c_node_env_clear(c_node_env_t *env);
extern void c_node_env_free(c_node_env_t *env);
extern ETERM *c_node_env_term_copy(c_node_env_t *env, const ETERM *term);
extern int c_node_reply(c_node_env_t *env, const ETERM *reply);
#define c_node_x_reply(env, reply_block)                                                                                           \
    do {                                                                                                                           \
        (env)->x_out->index = 0;                                                                                                   \
        (void)ei_x_encode_version((env)->x_out);                                                                                   \
        (void)ei_x_encode_tuple_header((env)->x_out, 2);                                                                           \
        (void)ei_x_encode_ref((env)->x_out, &(env)->from->tag);                                                                    \
        ei_x_buff *out = (env)->x_out;                                                                                             \
        do                                                                                                                         \
            reply_block while (0);                                                                                                 \
        c_node__x_reply(env);                                                                                                      \
    } while (0)
extern int c_node__x_reply(c_node_env_t *env);

/* Inline Functions */

static void c_node_linklist_init_anchor(c_node_linklist_t *anchor);
static int c_node_linklist_is_empty(c_node_linklist_t *anchor);
static int c_node_linklist_is_linked(c_node_linklist_t *node);
static void c_node_linklist_insert(c_node_linklist_t *pos, c_node_linklist_t *node);
static void c_node_linklist_insert_list(c_node_linklist_t *pos, c_node_linklist_t *list);
static void c_node_linklist_unlink(c_node_linklist_t *node);
#define c_node_log(level, label, format, ...)                                                                                      \
    c_node__log(level, label, __FILE__ ":" C_NODE_TO_STR(__LINE__) ":" format, ##__VA_ARGS__)
#define c_node_log_info(...) c_node_log(1, "INFO", __VA_ARGS__)
#define c_node_log_warning(...) c_node_log(2, "WARNING", __VA_ARGS__)
#define c_node_log_error(...) c_node_log(3, "ERROR", __VA_ARGS__)
#define c_node_log_trace(...) c_node_log(4, "TRACE", __VA_ARGS__)
#define c_node_log_debug(...) c_node_log(5, "DEBUG", __VA_ARGS__)
static void c_node__log(int level, const char *label, const char *format, ...);
static void *c_node_mem_alloc(size_t sz);
static void c_node_mem_free(void *p);
static void *c_node_mem_realloc(void *oldp, size_t sz);

inline void
c_node_linklist_init_anchor(c_node_linklist_t *anchor)
{
    anchor->next = anchor->prev = anchor;
}

inline int
c_node_linklist_is_empty(c_node_linklist_t *node)
{
    return node->next != NULL;
}

inline int
c_node_linklist_is_linked(c_node_linklist_t *anchor)
{
    return anchor->next == anchor;
}

inline void
c_node_linklist_insert(c_node_linklist_t *pos, c_node_linklist_t *node)
{
    assert(!c_node_linklist_is_linked(node));

    node->prev = pos->prev;
    node->next = pos;
    node->prev->next = node;
    node->next->prev = node;
}

inline void
c_node_linklist_insert_list(c_node_linklist_t *pos, c_node_linklist_t *list)
{
    if (c_node_linklist_is_empty(list))
        return;
    list->next->prev = pos->prev;
    list->prev->next = pos;
    pos->prev->next = list->next;
    pos->prev = list->prev;
    c_node_linklist_init_anchor(list);
}

inline void
c_node_linklist_unlink(c_node_linklist_t *node)
{
    node->next->prev = node->prev;
    node->prev->next = node->next;
    node->next = node->prev = NULL;
}

inline void
c_node__log(int level, const char *label, const char *format, ...)
{
    if (ei_get_tracelevel() >= level) {
        time_t now;
        char *timestr;
        char buf[2048];
        int len;
        va_list args;

        va_start(args, format);

        time(&now);
        timestr = (char *)ctime(&now);
        sprintf(buf, "%s: %.*s: ", label, (int)strlen(timestr) - 1, timestr);
        len = strnlen(buf, 2048);
        vsprintf(buf + len, format, args);
        fprintf(stderr, "%s\n", buf);
        va_end(args);
    }
}

inline void *
c_node_mem_alloc(size_t sz)
{
    void *p = malloc(sz);
    if (p == NULL) {
        c_node_fatal("no memory");
    }
    return p;
}

inline void
c_node_mem_free(void *p)
{
    (void)free(p);
}

inline void *
c_node_mem_realloc(void *oldp, size_t sz)
{
    void *newp = realloc(oldp, sz);
    if (newp == NULL) {
        c_node_fatal("no memory");
        return oldp;
    }
    return newp;
}

#endif

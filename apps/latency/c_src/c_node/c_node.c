// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "c_node.h"

/* Macros */

#define C_NODE_SERVER_SYNC(server, block)                                                                                          \
    do {                                                                                                                           \
        if (mtx_lock(&(server)->mutex) != thrd_success) {                                                                          \
            c_node_fatal("mutex lock error");                                                                                      \
        }                                                                                                                          \
        do                                                                                                                         \
            block while (0);                                                                                                       \
        if (mtx_unlock(&(server)->mutex) != thrd_success) {                                                                        \
            c_node_fatal("mutex unlock error");                                                                                    \
        }                                                                                                                          \
    } while (0)

#define C_NODE_STRUCT_FROM_MEMBER(s, m, p) ((s *)((char *)(p)-offsetof(s, m)))

/* Static Functions (Declarations) */

static int c_node_connect_init(c_node_server_t *server);
static int c_node_getfunc(c_node_server_t *server, const char *name, int arity, const c_node_func_t **funcp);
static int c_node_gethostname(c_node_config_t *config);
static int c_node_is_ready(int fd);
static int c_node_parse_opts(c_node_config_t *config, int argc, char *argv[]);
static int c_node_parse_opts_name(c_node_config_t *config, const char *subject, int is_short, const char *name);
static int c_node_publish(c_node_server_t *server);
static int c_node_tcp_listen(c_node_server_t *server);
static int c_node_tcp_open(void);
static void c_node_thread_link(c_node_server_t *server, c_node_thread_t *thread);
static int c_node_thread_loop(void *arg);
static void c_node_thread_unlink(c_node_thread_t *thread);

/* Public Functions */

int
c_node_init(c_node_server_t *server, int argc, char *argv[])
{
    (void)memset(server, 0, sizeof(*server));
    if (c_node_parse_opts(&server->config, argc, argv) == -1) {
        return -1;
    }
    if (server->config.needs_hostname == 1 && server->config.is_short == 0 && c_node_gethostname(&server->config) == -1) {
        return -1;
    }
    if (c_node_tcp_listen(server) == -1) {
        return -1;
    }
    (void)erl_init(NULL, 0);
    if (c_node_connect_init(server) == -1) {
        return -1;
    }
    if (c_node_publish(server) == -1) {
        return -1;
    }
    return 0;
}

int
c_node__enter_loop(c_node_server_t *server, int fc, const c_node_func_t *fv)
{
    if (mtx_init(&server->mutex, mtx_plain) != thrd_success) {
        return -1;
    }
    server->num_threads = 0;
    (void)c_node_linklist_init_anchor(&server->threads);
    server->num_funcs = fc;
    server->funcs = (void *)c_node_mem_alloc(sizeof(*fv) * fc);
    (void)memcpy(server->funcs, fv, sizeof(*fv) * fc);
    unsigned char *stdin_buf[1] = {0};

    for (;;) {
        if (c_node_is_ready(STDIN_FILENO) && read(STDIN_FILENO, stdin_buf, 1) <= 0) {
            return 0;
        }
        c_node_thread_t *thread = (void *)c_node_mem_alloc(sizeof(*thread));
        (void)memset(thread, 0, sizeof(*thread));
        thread->server = server;
        while ((thread->fd = ei_accept_tmo(&server->ec, server->fd, &thread->conn, 1000)) == ERL_ERROR) {
            c_node_log_trace("accept connection error\n");
            if (c_node_is_ready(STDIN_FILENO) && read(STDIN_FILENO, stdin_buf, 1) <= 0) {
                return 0;
            }
        }
        (void)c_node_thread_link(server, thread);
        if (thrd_create(&thread->tid, c_node_thread_loop, (void *)thread) != thrd_success) {
            c_node_log_error("unable to create thread\n");
            (void)close(thread->fd);
            (void)c_node_thread_unlink(thread);
            (void)c_node_mem_free(thread);
        }
    }

    return 0;
}

void
c_node__fatal(const char *format, ...)
{
    char buf[2048];
    va_list args;

    va_start(args, format);
    vsprintf(buf, format, args);
    fprintf(stderr, "fatal:%s\n", buf);
    va_end(args);
    abort();
}

void
c_node_env_clear(c_node_env_t *env)
{
    c_node_env_term_t *terms = env->terms;
    c_node_env_term_t *tmp = NULL;
    while (terms != NULL) {
        (void)erl_free_term(terms->term);
        tmp = terms->next;
        (void)c_node_mem_free(terms);
        terms = tmp;
    }
    (void)memset(env, 0, sizeof(*env));
}

void
c_node_env_free(c_node_env_t *env)
{
    (void)c_node_env_clear(env);
    (void)c_node_mem_free(env);
}

ETERM *
c_node_env_term_copy(c_node_env_t *env, const ETERM *term)
{
    c_node_env_term_t *tmp = (void *)c_node_mem_alloc(sizeof(*tmp));
    tmp->next = env->terms;
    tmp->term = erl_copy_term(term);
    env->terms = tmp;
    return tmp->term;
}

int
c_node_reply(c_node_env_t *env, const ETERM *reply)
{
    ei_x_buff *x_out = env->x_out;
    x_out->index = 0;
    (void)ei_x_encode_version(x_out);
    (void)ei_x_encode_tuple_header(x_out, 2);
    (void)ei_x_encode_ref(x_out, &env->from->tag);
    (void)ei_x_encode_term(x_out, (void *)reply);
    return c_node__x_reply(env);
}

int
c_node__x_reply(c_node_env_t *env)
{
    ei_x_buff *x_out = env->x_out;
    if (x_out->index > 0 && ei_send(env->thread->fd, &env->from->pid, x_out->buff, x_out->index) < 0) {
        c_node_fatal("ei_send to '%s'", env->from->pid.node);
    }
    x_out->index = 0;
    return 0;
}

/* Static Functions (Definitions) */

static int
c_node_connect_init(c_node_server_t *server)
{
    if (server->config.is_short == 1 && server->config.needs_hostname == 1) {
        if (ei_connect_init(&server->ec, server->config.name, server->config.cookie, 0) == -1) {
            return -1;
        }
        (void)memcpy(server->config.hostname, ei_thishostname(&server->ec),
                     strnlen(ei_thishostname(&server->ec), sizeof(server->config.hostname)));
        (void)memcpy(server->config.nodename, ei_thishostname(&server->ec),
                     strnlen(ei_thisnodename(&server->ec), sizeof(server->config.nodename)));
        return 0;
    }
    struct hostent *hp = NULL;
    if ((hp = ei_gethostbyname(server->config.hostname)) == NULL) {
        /* Looking up IP given hostname fails. We must be on a standalone
           host so let's use loopback for communication instead. */
        if ((hp = ei_gethostbyname("localhost")) == NULL) {
            return -1;
        }
    }
    return ei_connect_xinit(&server->ec, server->config.hostname, server->config.name, server->config.nodename,
                            (struct in_addr *)*hp->h_addr_list, server->config.cookie, 0);
}

static int
c_node_getfunc(c_node_server_t *server, const char *name, int arity, const c_node_func_t **funcp)
{
    size_t i;
    size_t len = strnlen(name, 255);
    for (i = 0; i < server->num_funcs; ++i) {
        if (server->funcs[i].arity == arity && strncmp(server->funcs[i].name, name, len) == 0) {
            *funcp = (const c_node_func_t *)&server->funcs[i];
            return 0;
        }
    }
    *funcp = NULL;
    return -1;
}

static int
c_node_gethostname(c_node_config_t *config)
{
    int error = 0;
    size_t pos = 0;

    /* get the host name */
    error = gethostname(config->hostname, sizeof(config->hostname));
    if (error) {
        fprintf(stderr, "Error: can't get hostname\n");
        return -1;
    }

    /* set the nodename */
    pos = strnlen(config->name, sizeof(config->name));
    (void)memcpy(config->nodename, config->name, pos);
    config->nodename[pos] = '@';
    (void)memcpy(config->nodename + pos + 1, config->hostname, strnlen(config->hostname, sizeof(config->hostname)));

    config->needs_hostname = 0;
    return 0;
}

static int
c_node_is_ready(int fd)
{
    fd_set fdset;
    struct timeval timeout;

    FD_ZERO(&fdset);
    FD_SET(fd, &fdset);
    timeout.tv_sec = 0;
    timeout.tv_usec = 1;
    return (select(fd + 1, &fdset, NULL, NULL, &timeout) == 1) ? 1 : 0;
}

static int
c_node_parse_opts(c_node_config_t *config, int argc, char *argv[])
{
    char *cookie = NULL;
    char *name = NULL;
    char *sname = NULL;
    int port = 0;
    int verbose = 0;
    int option = 0;

    while ((option = getopt(argc, argv, "c:n:s:p:v:")) != -1) {
        switch (option) {
        case 'c':
            cookie = optarg;
            break;
        case 'n':
            name = optarg;
            break;
        case 's':
            sname = optarg;
            break;
        case 'p':
            port = atoi(optarg);
            break;
        case 'v':
            verbose = atoi(optarg);
            break;
        default:
            goto usage;
        }
    }

    if (verbose < 0 || verbose > 5) {
        fprintf(stderr, "Error: VERBOSE must be within the range 0 to 5\n");
        goto usage;
    }

    if (cookie == NULL) {
        fprintf(stderr, "Error: COOKIE is required\n");
        goto usage;
    }

    if (strnlen(cookie, sizeof(config->cookie)) >= sizeof(config->cookie)) {
        fprintf(stderr, "Error: COOKIE must be less than %lu characters\n", sizeof(config->cookie));
        goto usage;
    }

    (void)memcpy(config->cookie, cookie, strnlen(cookie, sizeof(config->cookie)));

    if (name == NULL && sname == NULL) {
        fprintf(stderr, "Error: NAME or SNAME is required\n");
        goto usage;
    }

    if (name != NULL && sname != NULL) {
        fprintf(stderr, "Error: NAME and SNAME are mutually exclusive\n");
        goto usage;
    }

    if (port < 0 || port > 65535) {
        fprintf(stderr, "Error: PORT must be within the range 0 to 65535\n");
        goto usage;
    }

    if (name != NULL && c_node_parse_opts_name(config, "NAME", 0, name) == -1) {
        goto usage;
    }

    if (sname != NULL && c_node_parse_opts_name(config, "SNAME", 1, sname) == -1) {
        goto usage;
    }

    config->port = port;

    if (verbose > 0) {
        (void)ei_set_tracelevel(verbose);
    }

    return 0;

usage:
    fprintf(stderr, "Usage: %s -c COOKIE [-n NAME] [-s SNAME] [-p PORT] [-v VERBOSE]\n", argv[0]);
    return -1;
}

static int
c_node_parse_opts_name(c_node_config_t *config, const char *subject, int is_short, const char *name)
{
    size_t pos = 0;
    size_t len = 0;
    char *p = NULL;

    len = strnlen(name, sizeof(config->nodename));
    if (len >= sizeof(config->nodename)) {
        fprintf(stderr, "Error: %s must be less than %lu characters\n", subject, sizeof(config->nodename));
        return -1;
    }
    p = memchr(name, '@', len);
    if (p != NULL) {
        pos = p - name;
        if ((len - pos - 1) == 0) {
            len--;
            goto no_hostname;
        }
        if (memchr(p + 1, '@', len - pos - 1) != NULL) {
            fprintf(stderr, "Error: %s may only contain up to one '@' character\n", subject);
            return -1;
        }
        if (pos > sizeof(config->name)) {
            fprintf(stderr, "Error: %s must be less than %lu characters before the '@' character\n", subject, sizeof(config->name));
            return -1;
        }
        if ((len - pos) > sizeof(config->hostname)) {
            fprintf(stderr, "Error: %s must be less than %lu characters after the '@' character\n", subject,
                    sizeof(config->hostname));
            return -1;
        }
        (void)memcpy(config->nodename, name, len);
        (void)memcpy(config->hostname, p + 1, (len - pos - 1));
        (void)memcpy(config->name, name, pos);
        config->needs_hostname = 0;
    } else {
    no_hostname:
        if (len >= sizeof(config->name)) {
            fprintf(stderr, "Error: %s must be less than %lu characters without a '@' character\n", subject, sizeof(config->name));
            return -1;
        }
        (void)memset(config->nodename, 0, sizeof(config->nodename));
        (void)memset(config->hostname, 0, sizeof(config->hostname));
        (void)memcpy(config->name, name, len);
        config->needs_hostname = 1;
    }
    config->is_short = is_short;
    return 0;
}

static int
c_node_publish(c_node_server_t *server)
{
    int fd;

    fd = ei_publish(&server->ec, server->config.port);
    if (fd == -1) {
        return -1;
    }
    server->epmd_fd = fd;
    return 0;
}

static int
c_node_tcp_listen(c_node_server_t *server)
{
    int fd;
    struct sockaddr_in sa;
    socklen_t sa_len;

    if ((fd = c_node_tcp_open()) == -1) {
        goto error;
    }

    (void)memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_addr.s_addr = INADDR_ANY;
    sa.sin_port = htons(server->config.port);

    if (bind(fd, (struct sockaddr *)&sa, sizeof(sa)) != 0) {
        goto error;
    }
    if (listen(fd, 65535) != 0) {
        goto error;
    }

    (void)memset(&sa, 0, sizeof(sa));
    sa_len = sizeof(sa);
    if (getsockname(fd, (struct sockaddr *)&sa, &sa_len) != 0) {
        goto error;
    }

    server->fd = fd;
    server->config.port = ntohs(sa.sin_port);
    return 0;

error:
    if (fd != -1) {
        (void)close(fd);
    }
    return -1;
}

static int
c_node_tcp_open(void)
{
    int fd;
    int flag = 1;

    if ((fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1) {
        return -1;
    }

    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(flag)) != 0) {
        (void)close(fd);
        return -1;
    }

    return fd;
}

static void
c_node_msg_error(ei_x_buff *x_out, const char *reason)
{
    x_out->index = 0;
    (void)ei_x_encode_version(x_out);
    (void)ei_x_encode_tuple_header(x_out, 3);
    (void)ei_x_encode_atom(x_out, "error");
    (void)ei_x_encode_string(x_out, reason);
    c_node_thread_t *thread = C_NODE_STRUCT_FROM_MEMBER(c_node_thread_t, x_out, x_out);
    (void)ei_x_encode_pid(x_out, ei_self(&thread->server->ec));
}

static int
c_node_thread_dispatch(c_node_thread_t *thread, c_node_from_t *from)
{
    ei_x_buff *x_in = &thread->x_in;
    ei_x_buff *x_out = &thread->x_out;
    c_node_env_t env = {thread, from, x_out, NULL};

    int version;
    int arity;
    char msg_atom[MAXATOMLEN + 1] = {0};
    char fun_atom[MAXATOMLEN + 1] = {0};
    int type;
    int len;
    int argc;
    char *args = NULL;
    ETERM **argv = NULL;
    size_t i;
    const c_node_func_t *funcp = NULL;
    ETERM *result = NULL;

    if (ei_decode_version(x_in->buff, &x_in->index, &version) < 0) {
        c_node_log_warning("ignoring malformed message (bad version: %d)\n", version);
        return -1;
    }
    if (ei_decode_tuple_header(x_in->buff, &x_in->index, &arity) < 0) {
        c_node_log_warning("ignoring malformed message (not tuple)\n");
        return -1;
    }
    if (arity != 4) {
        c_node_log_warning("ignoring malformed message (must be a 4-arity tuple)\n");
        return -1;
    }
    if (ei_decode_atom(x_in->buff, &x_in->index, msg_atom) < 0) {
        c_node_log_warning("ignoring malformed message (first tuple element not atom)\n");
        return -1;
    }
    if (ei_decode_tuple_header(x_in->buff, &x_in->index, &arity) < 0 || arity != 2) {
        c_node_log_warning("ignoring malformed message (second tuple element not 2-arity tuple)\n");
        return -1;
    }
    if (ei_decode_pid(x_in->buff, &x_in->index, &from->pid) < 0) {
        c_node_log_warning("ignoring malformed message (first tuple element of second tuple element not pid)\n");
        return -1;
    }
    if (ei_decode_ref(x_in->buff, &x_in->index, &from->tag) < 0) {
        c_node_log_warning("ignoring malformed message (second tuple element of second tuple element not reference)\n");
        return -1;
    }

    if (strncmp(msg_atom, "stop", 4) == 0) {
        c_node_log_debug("node stopping normally\n");
        x_out->index = 0;
        (void)ei_x_encode_version(x_out);
        (void)ei_x_encode_atom(x_out, "ok");
        return 0;
    }

    if (strncmp(msg_atom, "call", 4) == 0) {
        if (ei_decode_atom(x_in->buff, &x_in->index, fun_atom) < 0) {
            c_node_log_warning("ignoring malformed message (third tuple element for 'call' not atom)\n");
            c_node_msg_error(x_out, "Third tuple element is not an atom.");
            return 1;
        }
        if (ei_get_type(x_in->buff, &x_in->index, &type, &len) < 0 ||
            (type != ERL_LIST_EXT && type != ERL_STRING_EXT && type != ERL_NIL_EXT)) {
            c_node_log_warning("ignoring malformed message (fourth tuple element for 'call' not list)\n");
            c_node_msg_error(x_out, "Fourth tuple element is not a list.");
            return 1;
        }
        switch (type) {
        case ERL_LIST_EXT: {
            if (ei_decode_list_header(x_in->buff, &x_in->index, &argc) < 0) {
                c_node_log_warning("ignoring malformed message (fourth tuple element for 'call' not list)\n");
                c_node_msg_error(x_out, "Fourth tuple element is not a list.");
                return 1;
            }
        } break;
        case ERL_STRING_EXT: {
            args = (void *)c_node_mem_alloc(sizeof(char) * (len + 1));
            if (ei_decode_string(x_in->buff, &x_in->index, args) < 0) {
                (void)c_node_mem_free(args);
                c_node_log_warning("ignoring malformed message (fourth tuple element for 'call' not list)\n");
                c_node_msg_error(x_out, "Fourth tuple element is not a list.");
                return 1;
            }
            argc = len;
        } break;
        case ERL_NIL_EXT:
        default: {
            argc = 0;
        } break;
        }
        if (c_node_getfunc(thread->server, fun_atom, argc, &funcp) < 0) {
            if (args != NULL) {
                (void)c_node_mem_free(args);
            }
            c_node_log_warning("ignoring malformed message (unknown function: '%s'/%d)\n", fun_atom, argc);
            c_node_msg_error(x_out, "Unknown function.");
            return 1;
        }
        if (argc > 0) {
            argv = (void *)c_node_mem_alloc(sizeof(ETERM *) * argc);
            (void)memset(argv, 0, sizeof(ETERM *) * argc);
            if (type == ERL_LIST_EXT) {
                for (i = 0; i < argc; ++i) {
                    if (ei_decode_term(x_in->buff, &x_in->index, (void *)&(argv[i])) < 0) {
                        (void)c_node_mem_free(argv);
                        c_node_log_warning("ignoring malformed message (%lu list element for 'call' not term)\n", i + 1);
                        c_node_msg_error(x_out, "Fourth tuple element contains invalid term in list.");
                        return 1;
                    }
                }
            } else if (type == ERL_STRING_EXT) {
                for (i = 0; i < argc; ++i) {
                    argv[i] = erl_mk_int((int)args[i]);
                }
                (void)c_node_mem_free(args);
            }
        }
        result = funcp->fptr(&env, argc, (const ETERM **)argv);
        if (result != NULL) {
            (void)c_node_reply(&env, result);
        }
        (void)erl_free_array(argv, argc);
        (void)c_node_mem_free(argv);
        (void)c_node_env_clear(&env);
        return 1;
    }

    c_node_log_warning("ignoring malformed message (first tuple element not atom 'stop' or 'call')\n");
    c_node_msg_error(x_out, "First tuple element is not the atom 'stop' or 'call'.");
    return 1;
}

static void
c_node_thread_link(c_node_server_t *server, c_node_thread_t *thread)
{
    C_NODE_SERVER_SYNC(server, {
        (void)c_node_linklist_insert(&server->threads, &thread->_link);
        thread->idx = server->num_threads++;
    });
}

static int
c_node_thread_loop(void *arg)
{
    c_node_thread_t *thread = (void *)arg;
    if (thread == NULL) {
        return 0;
    }
    if (thrd_detach(thread->tid) != thrd_success) {
        fprintf(stderr, "WARNING: thread detach error\n");
        (void)close(thread->fd);
        (void)c_node_thread_unlink(thread);
        (void)c_node_mem_free(thread);
        return 0;
    }

    if (ei_x_new(&thread->x_in) != 0) {
        c_node_fatal("no memory");
    }
    if (ei_x_new(&thread->x_out) != 0) {
        c_node_fatal("no memory");
    }

    erlang_msg msg;
    int running = 1;
    ei_x_buff *x_in = &thread->x_in;
    ei_x_buff *x_out = &thread->x_out;

    while (running) {
        x_in->index = 0;
        switch (ei_xreceive_msg(thread->fd, &msg, x_in)) {
        case ERL_ERROR:
        default:
            // c_node_log_error("[%d] %s error in receive: %d (%d)\n", thread->idx, thread->conn.nodename, erl_errno,
            //                  strerror(erl_errno));
            c_node_log_error("[%d] %s error in receive\n", thread->idx, thread->conn.nodename);
            running = 0;
            break;
        case ERL_TICK:
            c_node_log_debug("[%d] %s tick\n", thread->idx, thread->conn.nodename);
            break;
        case ERL_MSG:
            switch (msg.msgtype) {
            case ERL_LINK:
                c_node_log_debug("[%d] %s link\n", thread->idx, thread->conn.nodename);
                break;
            case ERL_UNLINK:
                c_node_log_debug("[%d] %s unlink\n", thread->idx, thread->conn.nodename);
                running = 0;
                break;
            case ERL_EXIT:
                c_node_log_debug("[%d] %s exit\n", thread->idx, thread->conn.nodename);
                running = 0;
                break;
            case ERL_SEND:
            case ERL_REG_SEND: {
                c_node_from_t from;
                (void)memset(&from, 0, sizeof(from));
                x_in->index = 0;
                running = c_node_thread_dispatch(thread, &from);
                if (running == -1) {
                    /* ignore messages without a return pid */
                    running = 1;
                } else {
                    if (x_out->index > 0 && ei_send(thread->fd, &from.pid, x_out->buff, x_out->index) < 0) {
                        c_node_fatal("ei_send to '%s'", from.pid.node);
                    }
                }
            } break;
            }
            break;
        }
    }

    (void)close(thread->fd);
    (void)ei_x_free(&thread->x_in);
    (void)ei_x_free(&thread->x_out);
    (void)c_node_thread_unlink(thread);

    return 0;
}

static void
c_node_thread_unlink(c_node_thread_t *thread)
{
    c_node_server_t *server = thread->server;
    C_NODE_SERVER_SYNC(server, {
        (void)c_node_linklist_unlink(&thread->_link);
        server->num_threads--;
    });
}

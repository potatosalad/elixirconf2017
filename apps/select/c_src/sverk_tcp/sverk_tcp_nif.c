/* sverk_tcp.c */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <fcntl.h>

#include "sverk_tcp_nif.h"

#define ASSERT(e) ((void)((e) ? 1 : (my_assert_error(#e, __func__, __FILE__, __LINE__), 0)))

static void
my_assert_error(const char *expr, const char *func, const char *file, int line)
{
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n", file, line, func, expr);
    fflush(stderr);
    abort();
}

#define SET_NONBLOCKING(fd) fcntl((fd), F_SETFL, fcntl((fd), F_GETFL, 0) | O_NONBLOCK)

typedef struct {
    int sock;
    ErlNifMutex *write_mtx;
    int is_writable;

    ErlNifMutex *read_mtx;
    int is_readable;
    ErlNifBinary read_bin;
    unsigned int read_capacity;

    int write_tries;
    int write_waits;
    int read_tries;
    int read_waits;
} Connection;

static void
conn_rt_dtor(ErlNifEnv *env, void *obj)
{
    Connection *conn = (Connection *)obj;
    enif_fprintf(stderr, "conn_rt_dtor called\n");

    enif_mutex_destroy(conn->read_mtx);
    enif_mutex_destroy(conn->write_mtx);
}

static void
conn_rt_stop(ErlNifEnv *env, void *obj, int fd, int is_direct_call)
{
    Connection *conn = (Connection *)obj;

    enif_fprintf(stderr, "conn_rt_stop called %s\n", (is_direct_call ? "DIRECT" : "LATER"));

    enif_mutex_lock(conn->read_mtx);
    enif_mutex_lock(conn->write_mtx);
    ASSERT(!conn->is_readable && !conn->is_writable);
    close(conn->sock);
    enif_mutex_unlock(conn->read_mtx);
    enif_mutex_unlock(conn->write_mtx);
}

// static void
// conn_rt_exit(ErlNifEnv *env, void *obj)
// {
//     enif_fprintf(stderr, "conn_rt_exit called\n");
// }

static ErlNifResourceTypeInit conn_rt_init = {conn_rt_dtor, conn_rt_stop};

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_closed;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_undefined;
static ErlNifResourceType *conn_rt;

static int
on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_true = enif_make_atom(env, "true");
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_closed = enif_make_atom(env, "closed");
    atom_eagain = enif_make_atom(env, "eagain");
    atom_undefined = enif_make_atom(env, "undefined");
    conn_rt = enif_open_resource_type_x(env, "connection", &conn_rt_init, ERL_NIF_RT_CREATE, NULL);
    return !conn_rt;
}

/* is_loaded() */
static ERL_NIF_TERM
is_loaded_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_true;
}

/* connect(Host,Port,Opts) */
static ERL_NIF_TERM
connect_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct addrinfo *ai;
    struct addrinfo hints;
    char hostname[30];
    char port_str[12];
    unsigned int port_no;
    Connection *conn;
    ERL_NIF_TERM res;
    int sock, cret;

    if (!enif_get_string(env, argv[0], hostname, sizeof(hostname), ERL_NIF_LATIN1) || !enif_get_uint(env, argv[1], &port_no)) {
        return enif_make_badarg(env);
    }

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = 0;
    hints.ai_protocol = 0; /* Any protocol */

    snprintf(port_str, sizeof(port_str), "%u", port_no);

    if (getaddrinfo(hostname, port_str, &hints, &ai) != 0) {
        return enif_make_string(env, "Unknown host", ERL_NIF_LATIN1);
    }

    sock = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (sock < 0) {
        return enif_make_string(env, "Socket create failed", ERL_NIF_LATIN1);
    }

    cret = connect(sock, ai->ai_addr, ai->ai_addrlen);
    freeaddrinfo(ai);
    if (cret != 0) {
        return enif_make_string(env, "Socket connect failed", ERL_NIF_LATIN1);
    }

    SET_NONBLOCKING(sock);

    conn = enif_alloc_resource(conn_rt, sizeof(Connection));
    conn->sock = sock;
    conn->read_capacity = 0;
    conn->write_mtx = enif_mutex_create("sverk_tcp.write");
    conn->read_mtx = enif_mutex_create("sverk_tcp.read");
    conn->is_writable = 1;
    conn->is_readable = 1;

    conn->write_tries = 0;
    conn->write_waits = 0;
    conn->read_tries = 0;
    conn->read_waits = 0;

    res = enif_make_resource(env, conn);
    enif_release_resource(conn);
    return enif_make_tuple2(env, atom_ok, res);
}

/* send_try(Sock, Data, Ref) */
static ERL_NIF_TERM
send_try_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    Connection *conn;
    ErlNifBinary data;
    int written;
    ERL_NIF_TERM res;
    int rv;

    if (!enif_get_resource(env, argv[0], conn_rt, (void **)&conn) || !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }
    enif_mutex_lock(conn->write_mtx);
    if (conn->is_writable) {
        conn->write_tries++;
        written = write(conn->sock, data.data, data.size);
        if (written == data.size) {
            res = atom_ok;
            goto done;
        } else if (written < 0) {
            if (errno != EAGAIN && errno != EINTR) {
                res = enif_make_tuple2(env, atom_error, enif_make_int(env, errno));
                goto done;
            }
            written = 0;
        }
        conn->write_waits++;
        rv = enif_select(env, conn->sock, ERL_NIF_SELECT_WRITE, conn, NULL, argv[2]);
        ASSERT(rv >= 0);
        res = enif_make_int(env, written);
    } else
        res = enif_make_badarg(env);

done:
    enif_mutex_unlock(conn->write_mtx);
    return res;
}

#define DEFAULT_RECV_BUF_SZ 2920

/* recv_try(Sock, Length, Ref) */
static ERL_NIF_TERM
recv_try_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    Connection *conn;
    int length;
    int got;
    ERL_NIF_TERM res;
    int rv;

    if (!enif_get_resource(env, argv[0], conn_rt, (void **)&conn) || !enif_get_int(env, argv[1], &length) || length < 0) {
        return enif_make_badarg(env);
    }

    enif_mutex_lock(conn->read_mtx);
    if (!conn->is_readable) {
        res = enif_make_tuple2(env, atom_error, atom_closed);
        goto done;
    }
    if (!conn->read_capacity) {
        conn->read_capacity = length ? length : DEFAULT_RECV_BUF_SZ;
        enif_alloc_binary(conn->read_capacity, &conn->read_bin);
    } else if (length > conn->read_capacity) {
        res = enif_make_badarg(env);
        goto done;
    }
    conn->read_tries++;
    got = read(conn->sock, conn->read_bin.data + conn->read_bin.size - conn->read_capacity, length ? length : conn->read_capacity);
    if (got >= length) {
        ASSERT(got == length || length == 0);

        res = enif_make_binary(env, &conn->read_bin);
        if (got != conn->read_capacity) {
            /* ToDo: Copy if less than ??? */
            res = enif_make_sub_binary(env, res, 0, got);
        }
        conn->read_capacity = 0;
        goto done;
    } else if (got == 0) {
        res = enif_make_tuple2(env, atom_error, atom_closed);
        goto done;
    } else if (got > 0) {
        conn->read_capacity -= got;
    } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
        res = enif_make_tuple2(env, atom_error, enif_make_int(env, errno));
        goto done;
    }
    conn->read_waits++;
    rv = enif_select(env, conn->sock, ERL_NIF_SELECT_READ, conn, NULL, argv[2]);
    ASSERT(rv >= 0);
    res = atom_eagain;

done:
    enif_mutex_unlock(conn->read_mtx);
    return res;
}

/* close(Sock) */
static ERL_NIF_TERM
close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    Connection *conn;
    int do_close;

    if (!enif_get_resource(env, argv[0], conn_rt, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    enif_mutex_lock(conn->read_mtx);
    enif_mutex_lock(conn->write_mtx);

    enif_fprintf(stderr, "Closing fd=%d\n", conn->sock);

    if (conn->read_capacity) {
        enif_release_binary(&conn->read_bin);
        conn->read_capacity = 0;
    }

    do_close = conn->is_readable || conn->is_writable;
    conn->is_readable = 0;
    conn->is_writable = 0;
    enif_mutex_unlock(conn->write_mtx);
    enif_mutex_unlock(conn->read_mtx);

    if (do_close) {
        int rv;
        enif_fprintf(stderr, "Schedule ERL_NIF_SELECT_STOP\n");
        rv = enif_select(env, conn->sock, ERL_NIF_SELECT_STOP, conn, NULL, atom_undefined);
        ASSERT(rv >= 0);
    }

    return atom_ok;
}

static ERL_NIF_TERM
stats_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    Connection *conn;
    ERL_NIF_TERM res;

    if (!enif_get_resource(env, argv[0], conn_rt, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    enif_mutex_lock(conn->read_mtx);
    enif_mutex_lock(conn->write_mtx);
    res = enif_make_tuple4(env, enif_make_int(env, conn->write_tries), enif_make_int(env, conn->write_waits),
                           enif_make_int(env, conn->read_tries), enif_make_int(env, conn->read_waits));
    enif_mutex_unlock(conn->write_mtx);
    enif_mutex_unlock(conn->read_mtx);
    return res;
}

static ErlNifFunc sverk_tcp_funcs[] = {{"is_loaded", 0, is_loaded_nif},   {"connect_nif", 3, connect_nif},
                                       {"send_try_nif", 3, send_try_nif}, {"recv_try_nif", 3, recv_try_nif},
                                       {"close_nif", 1, close_nif},       {"stats_nif", 1, stats_nif}};

ERL_NIF_INIT(sverk_tcp_nif, sverk_tcp_funcs, on_load, NULL, NULL, NULL)

// Copyright (c) 2016 The Luke Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "erl_nif.h"

#include "poly.h"
#include "newhope.h"
#include "params.h"

static ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, char *error)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error));
}

static ERL_NIF_TERM enif_luke_keypair(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    (void)argv;

    ErlNifBinary public;

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    if (! enif_alloc_binary(POLY_BYTES, &public)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    poly secret;

    newhope_keygen(public.data, &secret);

    ERL_NIF_TERM output_secret[PARAM_N];

    for (int i = 0; i < PARAM_N; ++i)
        output_secret[i] = enif_make_int(env, secret.v[i]);

    return enif_make_tuple2(env, enif_make_binary(env, &public), enif_make_list_from_array(env, output_secret, PARAM_N));
}

static ERL_NIF_TERM enif_luke_sharedb(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    ErlNifBinary public;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (! enif_inspect_iolist_as_binary(env, argv[0], &public)) {
        return enif_make_badarg(env);
    }

    if (public.size != POLY_BYTES) {
        return enif_make_badarg(env);
    }

    ErlNifBinary new_public;
    ErlNifBinary shared;

    if (! enif_alloc_binary(POLY_BYTES, &new_public)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    if (! enif_alloc_binary(32, &shared)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    newhope_sharedb(shared.data, new_public.data, public.data);

    return enif_make_tuple2(env, enif_make_binary(env, &shared), enif_make_binary(env, &new_public));
}

static ERL_NIF_TERM enif_luke_shareda(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    unsigned secret_length = 0;

    if (! enif_get_list_length(env, argv[0], &secret_length)) {
        return enif_make_badarg(env);
    }

    if (secret_length != PARAM_N) {
        return enif_make_badarg(env);
    }

    ErlNifBinary public;

    if (! enif_inspect_iolist_as_binary(env, argv[1], &public)) {
        return enif_make_badarg(env);
    }

    if (public.size != POLY_BYTES) {
        return enif_make_badarg(env);
    }

    ErlNifBinary shared;

    if (! enif_alloc_binary(32, &shared)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    int i = 0;
    poly secret;

    while (enif_get_list_cell(env, list, &head, &tail)) {
        int value;

        if (! enif_get_int(env, head, &value)) {
            return enif_make_badarg(env);
        }

        secret.v[i++] = value;
        list = tail;
    }

    newhope_shareda(shared.data, &secret, public.data);

    return enif_make_binary(env, &shared);
}

static ErlNifFunc nif_functions[] = {
    { "keypair", 0, enif_luke_keypair, ERL_NIF_DIRTY_JOB_CPU_BOUND },
    { "sharedb", 1, enif_luke_sharedb, ERL_NIF_DIRTY_JOB_CPU_BOUND },
    { "shareda", 2, enif_luke_shareda, ERL_NIF_DIRTY_JOB_CPU_BOUND },
};

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)load_info;

    return 0;
}

static int on_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)old_priv_data;
    (void)load_info;

    return 0;
}

ERL_NIF_INIT(luke_nif, nif_functions, on_load, /* reload */ NULL, on_upgrade, /* unload */ NULL);

// Copyright (c) 2016 The Luke Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "erl_nif.h"

#include "ref/poly.h"
#include "ref/newhope.h"
#include "ref/params.h"

static ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, char *error)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error));
}

static ERL_NIF_TERM enif_luke_keypair(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    (void)argv;

    ErlNifBinary public;
    ErlNifBinary secret;
    int tor = 0;

    poly newhope_secret;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (! enif_get_int(env, argv[0], &tor)) {
        return enif_make_badarg(env);
    }

    if (! enif_alloc_binary(NEWHOPE_SENDABYTES, &public)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    if (! enif_alloc_binary(POLY_BYTES, &secret)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    newhope_keygen(public.data, &newhope_secret, tor);

    poly_tobytes(secret.data, &newhope_secret);

    return enif_make_tuple2(env, enif_make_binary(env, &public), enif_make_binary(env, &secret));
}

static ERL_NIF_TERM enif_luke_sharedb(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    ErlNifBinary public;
    ErlNifBinary new_public;
    ErlNifBinary shared;
    int tor = 0;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if (! enif_inspect_iolist_as_binary(env, argv[0], &public)) {
        return enif_make_badarg(env);
    }

    if (! enif_get_int(env, argv[1], &tor)) {
        return enif_make_badarg(env);
    }

    if (public.size != NEWHOPE_SENDABYTES) {
        return enif_make_badarg(env);
    }

    if (! enif_alloc_binary(NEWHOPE_SENDBBYTES, &new_public)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    if (! enif_alloc_binary(32, &shared)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    newhope_sharedb(shared.data, new_public.data, public.data, tor);

    return enif_make_tuple2(env, enif_make_binary(env, &shared), enif_make_binary(env, &new_public));
}

static ERL_NIF_TERM enif_luke_shareda(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary secret;
    ErlNifBinary public;
    ErlNifBinary shared;

    poly newhope_secret;

    if (! enif_inspect_iolist_as_binary(env, argv[0], &secret)) {
        return enif_make_badarg(env);
    }

    if (secret.size != POLY_BYTES) {
        return enif_make_badarg(env);
    }

    if (! enif_inspect_iolist_as_binary(env, argv[1], &public)) {
        return enif_make_badarg(env);
    }

    if (public.size != NEWHOPE_SENDBBYTES) {
        return enif_make_badarg(env);
    }

    if (! enif_alloc_binary(32, &shared)) {
        return make_error_tuple(env, "alloc_binary_failed");
    }

    poly_frombytes(&newhope_secret, secret.data);

    newhope_shareda(shared.data, &newhope_secret, public.data);

    return enif_make_binary(env, &shared);
}

static ErlNifFunc nif_functions[] = {
    { "keypair", 1, enif_luke_keypair, ERL_NIF_DIRTY_JOB_CPU_BOUND },
    { "sharedb", 2, enif_luke_sharedb, ERL_NIF_DIRTY_JOB_CPU_BOUND },
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

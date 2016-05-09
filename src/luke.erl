%%%
%%% Copyright (c) 2016 The Luke Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc
%%% @end
%%% ----------------------------------------------------------------------------
-module(luke).

%% Public API.
-export([keypair/0,
         sharedb/1,
         shareda/2]).

%% Public Types
-export_type([public_key/0,
              secret_key/0,
              shared/0,
              keypair/0]).

-type public_key() :: binary().
-type secret_key() :: binary().
-type shared()     :: binary().

-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec keypair() -> keypair().
keypair() ->
    {Public, Secret} = luke_nif:keypair(),
    #{ public => Public, secret => Secret }.

-spec sharedb(PublicA :: public_key()) -> #{ shared => shared(), public => public_key() }.
sharedb(PublicA) ->
    {Shared, PublicB} = luke_nif:sharedb(PublicA),
    #{ shared => Shared, public => PublicB }.

-spec shareda(SecretA :: secret_key(), PublicB :: public_key()) -> shared().
shareda(SecretA, PublicB) ->
    luke_nif:shareda(SecretA, PublicB).

-ifdef(TEST).

keypair_test() ->
    #{ secret := Secret, public := Public } = keypair(),
    [
        ?assertEqual(1792, byte_size(Secret)),
        ?assertEqual(1792 + 32, byte_size(Public))
    ].

sharedb_test() ->
    #{ secret := SecretA, public := PublicA } = keypair(),
    #{ shared := SharedB, public := PublicB } = sharedb(PublicA),
    [
        ?assertEqual(1792, byte_size(SecretA)),
        ?assertEqual(1792 + 32, byte_size(PublicA)),

        ?assertEqual(1792 + 256, byte_size(PublicB)),
        ?assertEqual(32, byte_size(SharedB))
    ].

shareda_test() ->
    #{ secret := SecretA, public := PublicA } = keypair(),
    #{ shared := SharedB, public := PublicB } = sharedb(PublicA),
    SharedA = shareda(SecretA, PublicB),
    [
        ?assertEqual(1792, byte_size(SecretA)),
        ?assertEqual(1792 + 32, byte_size(PublicA)),

        ?assertEqual(1792 + 256, byte_size(PublicB)),
        ?assertEqual(32, byte_size(SharedB)),

        ?assertEqual(32, byte_size(SharedA)),

        ?assertEqual(SharedA, SharedB)
    ].

-endif.

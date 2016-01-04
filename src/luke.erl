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

-ifdef(TEST).
-include_lib("triq/include/triq.hrl").
-endif.

-type public_key() :: binary().
-type secret_key() :: [0 .. 65535].
-type shared()     :: binary().

-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

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
key() ->
    #{ secret := Secret, public := Public } = keypair(),
    {Public, Secret}.

prop_shared() ->
    %% Alice generates a keypair.
    ?FORALL({PublicA, SecretA}, key(),
        begin
            %% Bob derives a public key and a shared secret from Alice's
            %% public key.
            #{ shared := SharedB, public := PublicB } = sharedb(PublicA),

            %% Bob sends his public key to Alice who computes a shared
            %% secret between Bob's public key and her own secret key.
            SharedA = shareda(SecretA, PublicB),

            %% Verify that the shared secret is the same.
            SharedA =:= SharedB
        end).
-endif.

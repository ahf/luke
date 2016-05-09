%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for luke
%%% @end
%%% -----------------------------------------------------------
-module(prop_luke).

-include_lib("proper/include/proper.hrl").

prop_shared_secret() ->
    %% Alice generates a keypair.
    ?FORALL({PublicA, SecretA}, keypair(),
        begin
            %% Bob derives a public key and a shared secret from Alice's
            %% public key.
            #{ shared := SharedB, public := PublicB } = luke:sharedb(PublicA),

            %% Bob sends his public key to Alice who computes a shared
            %% secret between Bob's public key and her own secret key.
            SharedA = luke:shareda(SecretA, PublicB),

            %% Verify that the shared secret is the same.
            SharedA =:= SharedB
        end).

%% @private
keypair() ->
    #{ secret := Secret, public := Public } = luke:keypair(),
    {Public, Secret}.

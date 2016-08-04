%%%
%%% Copyright (c) 2016 The Luke Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @private
%%% ----------------------------------------------------------------------------
-module(luke_nif).

%% Private API.
-export([keypair/1,
         sharedb/2,
         shareda/2]).

%% Initializer.
-on_load(init/0).

%% NIF.
-define(nif_stub, nif_stub_error(?LINE)).

-spec init() -> ok | {error, any()}.
init() ->
    Module = "luke",
    File = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                DirectoryName when is_list(DirectoryName) ->
                    filename:join([filename:dirname(DirectoryName), "..", "priv", Module]);

                _Otherwise ->
                    filename:join(["..", "priv", Module])
            end;

        DirectoryName when is_list(DirectoryName) ->
            filename:join([DirectoryName, Module])
    end,
    erlang:load_nif(File, 0).

%% @private
keypair(_) ->
    ?nif_stub.

%% @private
sharedb(_, _) ->
    ?nif_stub.

%% @private
shareda(_, _) ->
    ?nif_stub.

%% @private
-spec nif_stub_error(Line :: non_neg_integer()) -> no_return().
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

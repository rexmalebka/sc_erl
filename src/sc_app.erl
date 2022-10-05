%%%-------------------------------------------------------------------
%% @doc sc public API
%% @end
%%%-------------------------------------------------------------------

-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

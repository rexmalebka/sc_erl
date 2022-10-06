%%%-------------------------------------------------------------------
%% @doc sc public API
%% @end
%%%-------------------------------------------------------------------

-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->ok.
	

stop(_State) ->
	ok.

%% internal functions

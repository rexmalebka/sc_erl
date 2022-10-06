-module(synthdef).

-author(rexmalebka).

-export([
	 load/2, load/1
	 , remove/1, remove/2
	]).

find_sc_client()->
	{ok, _Pid} = osc_client:start(),
	case osc_client:connect(localhost, 57110) of
		{error, {already_started, Pid}} -> Pid;
		{ok, Pid} -> Pid
	end.


%--------------------------------------------------------------------
%% @doc
%% add a synthdef, could be bytes form, directory path or file path.
%% @end
%--------------------------------------------------------------------

-spec load(OSC::pid(), Synthdef::iodata()|list()) -> ok | {error, timeout}.

load(OSC, SynthdefPath) when is_pid(OSC) and is_list(SynthdefPath) ->
	AbsPath = filename:absname(SynthdefPath),
	Res = case filelib:is_dir(AbsPath) of
		      true -> 
			      osc_client:call_msg(OSC, "/d_loadDir", [{s,SynthdefPath}]);
		      false ->
			      osc_client:call_msg(OSC, "/d_load", [{s,SynthdefPath}])
	      end,

	case Res of 
		{error, timeout} -> {error, timeout};
		{message, "/done",_} -> ok
	end;

load(OSC, Synthdef) when is_pid(OSC) and is_binary(Synthdef)->
	case osc_client:call_msg(OSC, "/d_recv", [{b,Synthdef}]) of
		{error, timeout} -> {error,timeout};
		{message, "/done", _ } -> ok
	end.

%--------------------------------------------------------------------
%% @doc
%% add a synthdef, could be bytes form, directory path or file path.
%% @end
%--------------------------------------------------------------------

-spec load(Synthdef::iodata()|list()) -> ok | {error, timeout}.

load(SynthdefPath) ->
	OSC = find_sc_client(),
	load(OSC, SynthdefPath).


%--------------------------------------------------------------------
%% @doc
%% remove a synthdef
%% @end
%--------------------------------------------------------------------

-spec remove(OSC::pid(), Synthdef::list()) -> ok.

remove(OSC, SynthdefName) when is_pid(OSC) and is_list(SynthdefName) ->
	osc_client:cast_msg(OSC, "/d_free", [{s,SynthdefName}]).


%--------------------------------------------------------------------
%% @doc
%% remove a synthdef
%% @end
%--------------------------------------------------------------------

-spec remove(Synthdef::list()) -> ok.

remove(SynthdefName) ->
	OSC = find_sc_client(),
	remove(OSC, SynthdefName).



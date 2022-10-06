-module(synth).

-author(rexmalebka).

-export([
	 load/5, load/4, load/3, load/2, load/1
	 , noid_synth/2, noid_synth/1
	 , get/3, get/2
	 , set/3, set/2
	 , remove/2, remove/1
	]).

%--------------------------------------------------------------------
%% @doc
%% add a synth specifying synthid, and actions 
%% @end
%--------------------------------------------------------------------

-type addAction() :: 0..4 | head | tail | before | 'after' | replace.
-type addActionTuple() :: { addAction(), integer() }.

-spec load(OSC::pid()
		, SynthdefName::list()
		, SynthId::integer()
		, SynthArgs::map( )
		, AddActionTuple::addActionTuple()
	       )-> ok.

load(OSC, SynthdefName, SynthId, SynthArgs, {AddAction, TargetAction } ) when
	  is_pid(OSC) and
	  is_list(SynthdefName) and
	  is_integer(SynthId) and 
	is_integer(AddAction) and
	( ( AddAction >= 0 ) and ( AddAction =< 4) ) and
	is_integer(TargetAction) and
	is_map(SynthArgs) -> 

	Args_ = lists:map(fun({Key, Value}) when is_number(Value) and is_atom(Key)->
					 [{s,atom_to_list(Key)}, Value];
			    ({Key, Value}) when is_number(Value) and is_integer(Key) ->
					 [Key, Value]
			 end, maps:to_list(SynthArgs) ),

	Args = Args_ ++ [{b,<<>>}],

	osc_client:cast_msg(
	  OSC
	  , "/s_new", [
		       {s, SynthdefName}, SynthId, AddAction, TargetAction | Args]
	 );

load(OSC, SynthdefName, SynthId, SynthArgs, {head, TargetAction } ) ->
	load(OSC, SynthdefName, SynthId, SynthArgs, {0, TargetAction } );

load(OSC, SynthdefName, SynthId, SynthArgs, {tail, TargetAction } ) ->
	load(OSC, SynthdefName, SynthId, SynthArgs, {1, TargetAction } );

load(OSC, SynthdefName, SynthId, SynthArgs, {before, TargetAction } ) ->
	load(OSC, SynthdefName, SynthId, SynthArgs, {2, TargetAction } );

load(OSC, SynthdefName, SynthId, SynthArgs, {'after', TargetAction } ) ->
	load(OSC, SynthdefName, SynthId, SynthArgs, {3, TargetAction } );

load(OSC, SynthdefName, SynthId, SynthArgs, {replace, TargetAction } ) ->
	load(OSC, SynthdefName, SynthId, SynthArgs, {4, TargetAction } ).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, the synth will be added to tail of group 1 
%% @end
%%--------------------------------------------------------------------

-spec load(OSC::pid()
		, SynthdefName::list()
		, SynthId::integer()
		, SynthArgs::map( )
	       )-> ok.

load(OSC, SynthdefName, SynthId, SynthArgs) when
	  is_pid(OSC) and
	  is_list(SynthdefName) and
	  is_integer(SynthId) and
	  is_map(SynthArgs) -> 
	load(OSC, SynthdefName, SynthId, SynthArgs, {1,1});
load(SynthdefName, SynthId, SynthArgs, ActionTuple) ->
	OSC = sc:get_client(),
	load(OSC, SynthdefName, SynthId, SynthArgs, ActionTuple).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, the synth will be added to tail of group 1 with no control arguments.
%% @end
%%--------------------------------------------------------------------

-spec load(OSC::pid(), SynthdefName::list(), SynthId::integer())-> ok;
	  (SynthdefName::list(), SynthId::integer(), SynthArgs::map())-> ok.

load(OSC, SynthdefName, SynthId ) when
	  is_pid(OSC) and
	  is_list(SynthdefName) and
	  is_integer(SynthId) ->
	load(OSC, SynthdefName, SynthId,#{}, {1,1});

load(SynthdefName, SynthId, SynthArgs ) ->
	OSC = sc:get_client(),
	load(OSC, SynthdefName, SynthId, SynthArgs, {1,1}).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, by default the synth will be added to tail of group 1, random id and no control arguments.
%% @end
%%--------------------------------------------------------------------

-spec load(OSC::pid(), SynthdefName::list() ) -> ok;
	  (SynthdefName::list(), SynthId::integer() ) -> ok.


load(OSC, SynthdefName) when
	  is_pid(OSC) and
	  is_list(SynthdefName) ->
	load(OSC, SynthdefName, -1, #{}, {1,1});

load(SynthdefName, SynthId) ->
	OSC = sc:get_client(),
	load(OSC, SynthdefName, SynthId, #{}, {1,1}).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, by default the synth will be added to tail of group 1, random id and no control arguments.
%% @end
%%--------------------------------------------------------------------

-spec load(SynthdefName::list())-> ok.

load(SynthdefName) ->
	OSC = sc:get_client(),
	load(OSC, SynthdefName, -1, #{}, {1,1}).

%%--------------------------------------------------------------------
%% @doc
%% get synth control value, in case the control does not exists will receivie a 0.0.
%% @end
%%--------------------------------------------------------------------

-spec get(OSC::pid()
		, SynthId::non_neg_integer()
		, ControlName::integer()|string()
	       ) -> {ok, float()} | {error, timeout} | {error, notfound}.


get(OSC, SynthId, ControlName) when is_pid(OSC) and
					  is_integer(SynthId) and (SynthId > 0) and
					  (is_list(ControlName) or is_integer(ControlName))-> 
	case osc_client:call_msg(OSC, "/s_get", [SynthId, ControlName]) of
		{error,timeout} -> {error,timeout};
		{message, "/n_set", [_, _, Value ]} ->
			{ok, Value};
		{message, "/fail", _ } -> {error,  notfound}
	end.

%%--------------------------------------------------------------------
%% @doc
%% get synth control value, in case the control does not exists will receivie a 0.0.
%% @end
%%--------------------------------------------------------------------

-spec get(SynthId::non_neg_integer()
		, ControlName::integer()|string()
	       ) -> {ok, float()} | {error, timeout} | {error, notfound}.

get(SynthId, ControlName) ->
	OSC = sc:get_client(),
	get(OSC, SynthId, ControlName).

%%--------------------------------------------------------------------
%% @doc
%% noid a synth, the synth could no longer be communicated 
%% @end
%%--------------------------------------------------------------------

-spec noid_synth(OSC::pid()
		, SynthId::non_neg_integer()
	       ) -> ok.
			
noid_synth(OSC,SynthId)  when is_pid(OSC) and is_integer(SynthId) and (SynthId > 0) ->
	osc_client:cast_msg(OSC, "/s_noid", [{i,SynthId}]).

%%--------------------------------------------------------------------
%% @doc
%% noid a synth, the synth could no longer be communicated 
%% @end
%%--------------------------------------------------------------------

-spec noid_synth(SynthId::non_neg_integer() ) -> ok.
			
noid_synth(SynthId)  ->
	OSC = sc:get_client(),
	noid_synth(OSC, SynthId).

%%--------------------------------------------------------------------
%% @doc
%% set control value for a synth
%% @end
%%--------------------------------------------------------------------

-spec set(OSC::pid()
	       , SynthId::non_neg_integer()
	       , ControlMap:: #{ atom()|integer() => float()|integer()}
	       ) -> ok.

set(OSC, SynthId, ControlMap) ->
	sc_node:set(OSC, SynthId, ControlMap).

%%--------------------------------------------------------------------
%% @doc
%% set control value for a synth
%% @end
%%--------------------------------------------------------------------

-spec set(SynthId::non_neg_integer()
	       , ControlMap:: #{ atom()|integer() => float()|integer()}
	       ) -> ok.

set(SynthId, ControlMap) ->
	OSC = sc:get_client(),
	sc_node:set(OSC, SynthId, ControlMap).

%%--------------------------------------------------------------------
%% @doc
%% removes a synth
%% @end
%%--------------------------------------------------------------------

-spec remove(OSC::pid()
		   , SynthId::non_neg_integer()
		  ) -> ok.

remove(OSC, SynthId) -> sc_node:remove(OSC, SynthId).

%%--------------------------------------------------------------------
%% @doc
%% removes a synth
%% @end
%%--------------------------------------------------------------------

-spec remove(SynthId::non_neg_integer() ) -> ok.

remove(SynthId) ->
	OSC = sc:get_client(),
	sc_node:remove(OSC, SynthId).

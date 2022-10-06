-module(synth).

-author(rexmalebka).

-export([
	 load/5, load/4, load/3, load/2, load/1
	 , noid_synth/2, noid_synth/1
	 , get/3, get/2
	 , set/3, set/2
	 , stop/2, stop/1
	]).

find_sc_client()->
	{ok, _Pid} = osc_client:start(),
	case osc_client:connect(localhost, 57110) of
		{error, {already_started, Pid}} -> Pid;
		{ok, Pid} -> Pid
	end.

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

	Args_ = case maps:size(SynthArgs) rem 2 of
			0 -> maps:to_list(SynthArgs);
			1 -> [ {dummyfix, 0} | maps:to_list(SynthArgs)]
		end,

	Args = lists:map(fun({Key, Value}) when is_number(Value) and is_atom(Key)->
					 [{s,atom_to_list(Key)}, Value];
			    ({Key, Value}) when is_number(Value) and is_integer(Key) ->
					 [Key, Value]
			 end,Args_),

	osc_client:cast_msg(
	  OSC
	  , "/s_new", [
		       {s, SynthdefName}, SynthId, AddAction, TargetAction | lists:append(Args)]
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
	OSC = find_sc_client(),
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
	OSC = find_sc_client(),
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
	OSC = find_sc_client(),
	load(OSC, SynthdefName, SynthId, #{}, {1,1}).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, by default the synth will be added to tail of group 1, random id and no control arguments.
%% @end
%%--------------------------------------------------------------------

-spec load(SynthdefName::list())-> ok.

load(SynthdefName) ->
	OSC = find_sc_client(),
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
	OSC = find_sc_client(),
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
	OSC = find_sc_client(),
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
	set_node(OSC, SynthId, ControlMap).

%%--------------------------------------------------------------------
%% @doc
%% set control value for a synth
%% @end
%%--------------------------------------------------------------------

-spec set(SynthId::non_neg_integer()
	       , ControlMap:: #{ atom()|integer() => float()|integer()}
	       ) -> ok.

set(SynthId, ControlMap) ->
	OSC = find_sc_client(),
	set_node(OSC, SynthId, ControlMap).

%%--------------------------------------------------------------------
%% @doc
%% removes a synth
%% @end
%%--------------------------------------------------------------------

-spec stop(OSC::pid()
		   , SynthId::non_neg_integer()
		  ) -> ok.

stop(OSC, SynthId) -> remove_node(OSC, SynthId).

%%--------------------------------------------------------------------
%% @doc
%% removes a synth
%% @end
%%--------------------------------------------------------------------

-spec stop(SynthId::non_neg_integer() ) -> ok.

stop(SynthId) ->
	OSC = find_sc_client(),
	remove_node(OSC, SynthId).

%%--------------------------------------------------------------------
%% @doc
%% sets value for a control of a node
%% @end
%%--------------------------------------------------------------------

-spec set_node(OSC::pid()
	       , NodeId::non_neg_integer()
	       , ControlMap:: #{ atom()|integer() => float()|integer()}
	       ) -> ok.

set_node(OSC, NodeId, ControlMap) when is_pid(OSC) and
			     is_integer(NodeId) and
			     is_map(ControlMap) ->

	Args_ = case maps:size(ControlMap) rem 2 of
			0 -> maps:to_list(ControlMap);
			1 -> [ {dummyfix, 0} | maps:to_list(ControlMap)]
		end,

	Args = lists:map(fun({Key, Value}) when is_number(Value) and is_atom(Key)->
					 [{s,atom_to_list(Key)}, Value];
			    ({Key, Value}) when is_number(Value) and is_integer(Key) ->
					 [Key, Value]
			 end,Args_),

	osc_client:cast_msg(OSC, "/n_set", [NodeId | Args ]).

%%--------------------------------------------------------------------
%% @doc
%% remove a node
%% @end
%%--------------------------------------------------------------------

-spec remove_node(OSC::pid()
		   , NodeId::non_neg_integer()
		  ) -> ok.

remove_node(OSC, NodeId) when is_pid(OSC) and
			     is_integer(NodeId) ->
	osc_client:cast_msg(OSC, "/n_free", [NodeId]).

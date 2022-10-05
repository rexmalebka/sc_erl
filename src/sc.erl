-module(sc).

-author(rexmalebka).

-export([
	 add_synthdef/2
	 , remove_synthdef/2

	 , add_synth/5, add_synth/4, add_synth/3, add_synth/2
	 , noid_synth/2
	 , get_synth/3, set_synth/3
	 , remove_synth/2
	 
	 , add_buffer/5, add_buffer/3
	 , remove_buffer/2
	 , get_buffer/2

	]).


%--------------------------------------------------------------------
%% @doc
%% add a synthdef, could be bytes form, directory path or file path.
%% @end
%--------------------------------------------------------------------

-spec add_synthdef(OSC::pid(), Synthdef::iodata()|list()) -> ok | {error, timeout}.

add_synthdef(OSC, SynthdefPath) when is_pid(OSC) and is_list(SynthdefPath) ->
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

add_synthdef(OSC, Synthdef) when is_pid(OSC) and is_binary(Synthdef)->
	case osc_client:call_msg(OSC, "/d_recv", [{b,Synthdef}]) of
		{error, timeout} -> {error,timeout};
		{message, "/done", _ } -> ok
	end.

%--------------------------------------------------------------------
%% @doc
%% remove a synthdef
%% @end
%--------------------------------------------------------------------

-spec remove_synthdef(OSC::pid(), Synthdef::list()) -> ok.

remove_synthdef(OSC, SynthdefName) when is_pid(OSC) and is_list(SynthdefName) ->
	osc_client:cast_msg(OSC, "/d_free", [{s,SynthdefName}]).



%--------------------------------------------------------------------
%--------------------------------------------------------------------


%--------------------------------------------------------------------
%% @doc
%% add a synth specifying synthid, and actions 
%% @end
%--------------------------------------------------------------------
-type addAction() :: 0..4 | head | tail | before | 'after' | replace.
-type addActionTuple() :: { addAction(), integer() }.

-spec add_synth(OSC::pid()
		, SynthdefName::list()
		, SynthId::integer()
		, SynthArgs::map( )
		, AddActionTuple::addActionTuple()
	       )-> ok.

add_synth(OSC, SynthdefName, SynthId, SynthArgs, {AddAction, TargetAction } ) when
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

add_synth(OSC, SynthdefName, SynthId, SynthArgs, {head, TargetAction } ) ->
	add_synth(OSC, SynthdefName, SynthId, SynthArgs, {0, TargetAction } );

add_synth(OSC, SynthdefName, SynthId, SynthArgs, {tail, TargetAction } ) ->
	add_synth(OSC, SynthdefName, SynthId, SynthArgs, {1, TargetAction } );

add_synth(OSC, SynthdefName, SynthId, SynthArgs, {before, TargetAction } ) ->
	add_synth(OSC, SynthdefName, SynthId, SynthArgs, {2, TargetAction } );

add_synth(OSC, SynthdefName, SynthId, SynthArgs, {'after', TargetAction } ) ->
	add_synth(OSC, SynthdefName, SynthId, SynthArgs, {3, TargetAction } );

add_synth(OSC, SynthdefName, SynthId, SynthArgs, {replace, TargetAction } ) ->
	add_synth(OSC, SynthdefName, SynthId, SynthArgs, {4, TargetAction } ).



%%--------------------------------------------------------------------
%% @doc
%% add a synth, the synth will be added to tail of group 1 
%% @end
%%--------------------------------------------------------------------

-spec add_synth(OSC::pid()
		, SynthdefName::list()
		, SynthId::integer()
		, SynthArgs::map( )
	       )-> ok.

add_synth(OSC, SynthdefName, SynthId, SynthArgs) ->
	add_synth(OSC, SynthdefName, SynthId, SynthArgs, {1,1}).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, the synth will be added to tail of group 1 with no control arguments.
%% @end
%%--------------------------------------------------------------------

-spec add_synth(OSC::pid()
		, SynthdefName::list()
		, SynthId::integer()
	       )-> ok.

add_synth(OSC, SynthdefName, SynthId ) ->
	add_synth(OSC, SynthdefName, SynthId,#{}, {1,1}).

%%--------------------------------------------------------------------
%% @doc
%% add a synth, by default the synth will be added to tail of group 1, random id and no control arguments.
%% @end
%%--------------------------------------------------------------------

-spec add_synth(OSC::pid()
		, SynthdefName::list()
	       )-> ok.

add_synth(OSC, SynthdefName) when
	  is_pid(OSC) and
	  is_list(SynthdefName) ->
	add_synth(OSC, SynthdefName, -1, #{}, {1,1}).


%%--------------------------------------------------------------------
%% @doc
%% get synth control value, in case the control does not exists will receivie a 0.0.
%% @end
%%--------------------------------------------------------------------

-spec get_synth(OSC::pid()
		, SynthId::non_neg_integer()
		, ControlName::integer()|string()
	       ) -> {ok, float()} | {error, timeout} | {error, notfound}.


get_synth(OSC, SynthId, ControlName) when is_pid(OSC) and
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
%% set control value for a synth
%% @end
%%--------------------------------------------------------------------


-spec set_synth(OSC::pid()
	       , SynthId::non_neg_integer()
	       , ControlMap:: #{ atom()|integer() => float()|integer()}
	       ) -> ok.

set_synth(OSC, SynthId, ControlMap) ->
	set_node(OSC, SynthId, ControlMap).


%%--------------------------------------------------------------------
%% @doc
%% removes a synth
%% @end
%%--------------------------------------------------------------------

-spec remove_synth(OSC::pid()
		   , SynthId::non_neg_integer()
		  ) -> ok.

remove_synth(OSC, SynthId) -> remove_node(OSC, SynthId).




%--------------------------------------------------------------------
%--------------------------------------------------------------------




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




%--------------------------------------------------------------------
%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @doc
%% add a buffer
%% @end
%%--------------------------------------------------------------------


-spec add_buffer(OSC::pid()
		 , Path:: list()
		 , BufferId::non_neg_integer()
		 , StartFrame::non_neg_integer()
		 , TotalFrames::non_neg_integer()
		  ) -> ok | {error, not_found}.

add_buffer(OSC, Path, BufferId, StartFrame , TotalFrames )  when 
	  is_pid(OSC) and 
	  is_list(Path) and
	  is_integer(BufferId) and
	  is_integer(StartFrame) and (StartFrame >= 0) and
	  is_integer(TotalFrames)  
	  -> 
	case osc_client:call_msg(OSC, "/b_allocRead", [BufferId, {s,Path}, StartFrame, TotalFrames]) of
		{error, timeout} -> {error, timeout};
		{message, "/done", _} -> ok;
		{message, "/fail",_} -> {error, not_found}
	end.
					 

%%--------------------------------------------------------------------
%% @doc
%% add a buffer
%% @end
%%--------------------------------------------------------------------


-spec add_buffer(OSC::pid()
		 , Path:: list()
		 , BufferId::non_neg_integer()
		  ) -> ok | {error, not_found}.

add_buffer(OSC, Path, BufferId )  when 
	  is_pid(OSC) and 
	  is_list(Path) and
	  is_integer(BufferId) and (BufferId > 0) 
	  -> 
	add_buffer(OSC, Path, BufferId, 0, -1).


%%--------------------------------------------------------------------
%% @doc
%% removes a buffer
%% @end
%%--------------------------------------------------------------------


-spec remove_buffer(OSC::pid()
		 , BufferId::non_neg_integer()
		  ) -> ok | {error, not_found}.

remove_buffer(OSC, BufferId )  when 
	  is_pid(OSC) and 
	  is_integer(BufferId) and (BufferId > 0) 
	  -> 
	case osc_client:call_msg(OSC, "/b_free",[BufferId]) of
		{error, timeout} -> {error, timeout};
		{message, "/done", _} -> ok;
		{message, "/fail",_} -> {error, not_found}
	end.


%%--------------------------------------------------------------------
%% @doc
%% gets data from a buffer
%% @end
%%--------------------------------------------------------------------

-type bufferOutput() :: {
			 BufferNumber :: integer()
			 , NumFrames::integer()
			 , NumChannels::integer()
			 , SampleRate::float()}.

-spec get_buffer(OSC::pid()
		 , BufferId::non_neg_integer()
		  ) -> {ok, bufferOutput()} | {error, not_found}.

get_buffer(OSC, BufferId )  when 
	  is_pid(OSC) and 
	  is_integer(BufferId) and (BufferId > 0) 
	  -> 
	case osc_client:call_msg(OSC, "/b_query",[BufferId]) of
		{error, timeout} -> {error, timeout};
		{message, "/b_info", BufferOutput} -> {ok, BufferOutput};
		{message, "/fail",_} -> {error, not_found}
	end.







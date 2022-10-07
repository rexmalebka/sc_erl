-module(sc_group).

-author(rexmalebka).

-export([
	 add/2, add/3
	 , get/1
	 , remove/2, remove/1	
	]).

-type addAction() :: 0..4 | head | tail | before | 'after' | replace.
-type addActionTuple() :: { addAction(), integer() }.

-spec add(OSC::pid(), GroupId::integer(), AddAction::addActionTuple()) -> ok.

add(OSC, GroupId, {AddAction, TargetAction}) when
	  is_pid(OSC) and
	  is_integer(GroupId) and
	  ( ( AddAction >= 0 ) and ( AddAction =< 4) ) and
	  is_integer(TargetAction) ->
	osc_client:cast_msg(
	  OSC
	  , "/g_new", [{i,GroupId}, {i,AddAction}, {i,TargetAction}, {b,<<>>}]
	 );

add(OSC, GroupId, {head, TargetAction}) ->  add(OSC, GroupId, {0, TargetAction});

add(OSC, GroupId, {tail, TargetAction}) ->  add(OSC, GroupId, {1, TargetAction});

add(OSC, GroupId, {before, TargetAction}) ->  add(OSC, GroupId, {2, TargetAction});

add(OSC, GroupId, {'after', TargetAction}) ->  add(OSC, GroupId, {3, TargetAction});

add(OSC, GroupId, {replace, TargetAction}) ->  add(OSC, GroupId, {4, TargetAction}).

-spec add( GroupId::integer(), AddAction::addActionTuple()) -> ok.

add(GroupId, {AddAction, TargetAction}) ->
	  OSC = sc:get_client(),
	  add(OSC, GroupId, {AddAction, TargetAction}).

-type querySynthArgs() :: { string() | integer(), float() | string() }.

-type  querySynth() :: {synth, SynthId::integer(), SynthDefName::string(), [querySynthArgs()] }.

-type  queryTree() :: {group, GroupId::integer(), [querySynth()] }.

-spec get(GroupIp::integer() ) -> queryTree().

get(GroupId) when is_integer(GroupId)->
	OSC = sc:get_client(),
	case  osc_client:call_msg(
	  OSC
	  , "/g_queryTree",
	  [GroupId, 1] ) of
		{message, "/fail",_} -> {error, {group, not_found}};
		{message, "/g_queryTree.reply", QueryTree} -> 
		       parse_queryTree(QueryTree)
	end.

parse_queryTree([HasSynthControl, GroupId, NumChildNodes | ChildNodes ]) when 
	  HasSynthControl == 1
	  ->
	QuerySynths = parse_querySynth(ChildNodes, [], NumChildNodes),
	{
	 group,
	 GroupId,
	 QuerySynths
	}.

parse_querySynth([NodeId, ChildNodesNumber  |ChildNodes], QueryList, NumNodes) when ( NumNodes > 0 ) ->
	{Output, RestChildNodes}  = case  <<ChildNodesNumber>> == <<-1>> of
			 true ->
				 [ SynthDefName, NumControls| RestData ] = ChildNodes,
				 {ControlArgsList, Rest} = lists:split(NumControls*2, RestData),
				 

				 {_, ControlArgsPList} = lists:foldl(fun(Elem, {odd, Acc} ) ->
							     {even, Acc, Elem};
						(Elem, {even, Acc, Prop}) ->
							     {odd, Acc ++ [ {Prop, Elem} ]}
					     end
					    , {odd, []}
					    , ControlArgsList
					    ),

				 ControlArgs = maps:from_list(ControlArgsPList),
				 {
				  {
				   synth,
				   NodeId,
 				   SynthDefName,
 				   ControlArgs
 				  },
				  Rest
				 };

			 false ->
						    {
				 {
				 group,
				 NodeId,
				 ChildNodesNumber
				 },
				 ChildNodes}
		 end,
	parse_querySynth(RestChildNodes, QueryList ++ [Output], NumNodes -1 );

parse_querySynth(_ChildNodes, QueryList, NumNodes) when ( NumNodes == 0 ) ->
	QueryList.










-spec remove(OSC::pid(), GroupId::integer()) -> ok.

remove(OSC, GroupId)->
	sc_node:remove(OSC, GroupId).

-spec remove(GroupId::integer()) -> ok.

remove(GroupId)->
	OSC = sc:get_client(),
	sc_node:remove(OSC, GroupId).

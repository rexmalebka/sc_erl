-module(sc_group).

-author(rexmalebka).

-export([
	 add/2, add/3
	 , remove/2, remove/1	
	]).

find_sc_client() ->
	{ok, _Pid} = osc_client:start(),
	case osc_client:connect(localhost, 57110) of
		{error, {already_started, Pid}} -> Pid;
		{ok, Pid} -> Pid
	end.

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
	  , "/g_new", [GroupId, AddAction, TargetAction]
	 ).

-spec add( GroupId::integer(), AddAction::addActionTuple()) -> ok.

add(GroupId, {AddAction, TargetAction}) ->
	  OSC = find_sc_client(),
	  add(OSC, GroupId, {AddAction, TargetAction}).

-spec remove(OSC::pid(), GroupId::integer()) -> ok.

remove(OSC, GroupId)->
	sc_node:remove(OSC, GroupId).

-spec remove(GroupId::integer()) -> ok.

remove(GroupId)->
	OSC = find_sc_client(),
	sc_node:remove(OSC, GroupId).
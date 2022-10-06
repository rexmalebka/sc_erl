-module(sc_node).

-author(rexmalebka).

-export([
	 remove/2
	 , set/3
	]).

-spec remove(OSC::pid()
		  , NodeId::non_neg_integer()
		 ) -> ok.

remove(OSC, NodeId) when is_pid(OSC) and
			 is_integer(NodeId) ->
	osc_client:cast_msg(OSC, "/n_free", [NodeId]).


-spec set(OSC::pid()
	  , NodeId::non_neg_integer()
	  , ControlMap:: #{ atom()|integer() => float()|integer()}
	 ) -> ok.

set(OSC, NodeId, ControlMap) when is_pid(OSC) and
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


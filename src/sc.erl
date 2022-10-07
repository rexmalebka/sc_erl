-module(sc).

-author(rexmalebka).

-export([
	 get_client/0
	]).


get_client()->
	{ok, _Pid} = osc_client:start(),
	case osc_client:connect(localhost, 57110) of
		{error, {already_started, Pid}} -> Pid;
		{ok, Pid} -> Pid
	end.

version()->
	OSC = get_client(),
	{
	 message
	 , "/verion.reply"
	 , Version } = osc_client:call_msg(
	  OSC,
	  "/version"
	 ),
	Version.



-module(sc).

-author(rexmalebka).

-export([
	  add_buffer/5, add_buffer/3
	 , remove_buffer/2
	 , get_buffer/1, get_buffer/2

	]).

find_sc_client()->
	{ok, _Pid} = osc_client:start(),
	case osc_client:connect(localhost, 57110) of
		{error, {already_started, Pid}} -> Pid;
		{ok, Pid} -> Pid
	end.
		

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


%%--------------------------------------------------------------------
%% @doc
%% gets data from a buffer
%% @end
%%--------------------------------------------------------------------

-spec get_buffer(BufferId::non_neg_integer() ) -> {ok, bufferOutput()} | {error, not_found}.

get_buffer(BufferId )  when 
	  is_integer(BufferId) and (BufferId > 0) 
	  -> 
	Pid = find_sc_client(),
	get_buffer(Pid, BufferId).




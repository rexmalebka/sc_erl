-module(buffer).

-author(rexmalebka).

-export([
	  load/5, load/4, load/3, load/2
	 , remove/2, remove/1
	 , get/1, get/2

	]).

%%--------------------------------------------------------------------
%% @doc
%% add a buffer
%% @end
%%--------------------------------------------------------------------

-spec load(OSC::pid()
		 , Path:: list()
		 , BufferId::non_neg_integer()
		 , StartFrame::non_neg_integer()
		 , TotalFrames::non_neg_integer()
		  ) -> ok | {error, not_found}.

load(OSC, Path, BufferId, StartFrame , TotalFrames )  when 
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

-spec load(Path:: list()
	   , BufferId::non_neg_integer()
	   , StartFrame::non_neg_integer()
	   , TotalFrames::non_neg_integer()) -> ok | {error, not_found}.

load(Path, BufferId, StartFrame, TotalFrames) ->
	OSC = sc:get_client(),
	load(OSC, Path, BufferId, StartFrame , TotalFrames).

%%--------------------------------------------------------------------
%% @doc
%% add a buffer
%% @end
%%--------------------------------------------------------------------

-spec load(OSC::pid()
		 , Path:: list()
		 , BufferId::non_neg_integer()
		  ) -> ok | {error, not_found}.

load(OSC, Path, BufferId )  when 
	  is_pid(OSC) and 
	  is_list(Path) and
	  is_integer(BufferId) and (BufferId > 0) 
	  -> 
	load(OSC, Path, BufferId, 0, -1).

%%--------------------------------------------------------------------
%% @doc
%% add a buffer
%% @end
%%--------------------------------------------------------------------

-spec load(Path:: list(), BufferId::non_neg_integer() ) -> ok | {error, not_found}.

load(Path, BufferId ) -> 
	OSC = sc:get_client(),
	load(OSC, Path, BufferId, 0, -1).

%%--------------------------------------------------------------------
%% @doc
%% removes a buffer
%% @end
%%--------------------------------------------------------------------

-spec remove(OSC::pid()
		 , BufferId::non_neg_integer()
		  ) -> ok | {error, not_found}.

remove(OSC, BufferId )  when 
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
%% removes a buffer
%% @end
%%--------------------------------------------------------------------

-spec remove(BufferId::non_neg_integer() ) -> ok | {error, not_found}.

remove(BufferId ) ->
	OSC = sc:get_client(),
	remove(OSC, BufferId).

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

-spec get(OSC::pid()
		 , BufferId::non_neg_integer()
		  ) -> {ok, bufferOutput()} | {error, not_found}.

get(OSC, BufferId )  when 
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

-spec get(BufferId::non_neg_integer() ) -> {ok, bufferOutput()} | {error, not_found}.

get(BufferId )  when 
	  is_integer(BufferId) and (BufferId > 0) 
	  -> 
	Pid = sc:get_client(),
	get(Pid, BufferId).

-module(cloud_app).

-behaviour(application).

-include_lib("alog/include/alog.hrl").

%% Application callbacks
-export([start/0, start/2, stop/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
	application:start(cloud).    

start(_StartType, _StartArgs) ->	
	application:start(sasl),
	application:start(alog),
	application:start(gproc),

	Nodes = case application:get_env(cloud,nodes) of 
		{ok, N} -> N;
		undefined -> []
	end,

	cloud_sup:start_link([{cloud_nodes,Nodes},{cloud_pid,self()}]).

stop(_State) ->
    ok.
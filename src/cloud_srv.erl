-module(cloud_srv).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("alog/include/alog.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	?INFO("~nCloud srv started with args: ~p",[Args]),
    {ok, Args}.

handle_call(_Request, _From, State) ->
	io:format("~ncloud_srv_Request=~p~n",[_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
	io:format("~ncloud_srv_Msg=~p~n",[_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
	io:format("~ncloud_srv_Info=~p~n",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("~ncloud_srv_Reason=~p~n",[_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


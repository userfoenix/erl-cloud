-module(cloud_ping).
-behaviour(gen_server).

-define(SERVER, ?MODULE).


-include_lib("alog/include/alog.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, info/0, get/1, get_pid/1, ping_nodes/0, ping_node/1, register/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Worker state
-record(cloud_ping_state, {pid, name, nodes}).

%% Node record
-record(cloud_node, {name, status, pid, monitor}).

-define(NAME_INDEX, 2).
-define(PID_INDEX, 4).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

%% Get worker state (sync)
info() ->
	gen_server:call(?MODULE, info).

%% Get node data by hostname, return as proplist (sync)
get(Name) ->
	gen_server:call(?MODULE, {get_node,Name}).

%% Get pid by hostname
get_pid(Name) ->
	gen_server:call(?MODULE, {get_node_pid,Name}).

%% Register external node (from list) by pid and name
register(Node,Pid) when is_pid(Pid) ->
	gen_server:call(?MODULE, {register,Node,Pid});
register(_Node,_Pid) ->
	{error, bad_pid}.

%% Ping nodes list (async)
ping_nodes() ->
	gen_server:cast(?MODULE, ping_nodes).

%% Ping node by name (async)
ping_node(Name) ->
	gen_server:cast(?MODULE, {ping_node,Name}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Opts]) ->
	Pid = proplists:get_value(cloud_pid,Opts,undefined),
	Name = proplists:get_value(cloud_name,Opts,node()),
	Nodes = [begin
		#cloud_node{
			name = N,
			status = -1,
			pid = undefined,
			monitor = undefined
		}
	end || N <- proplists:get_value(cloud_nodes,Opts,[]), N=/=Name],

	State = #cloud_ping_state{
		pid = Pid,
		name = Name,
		nodes = Nodes
	},

	erlang:send_after(10, self(), ping_all),

	?DBG("Init.State=~p",[State]),

	{ok, State}.

%% ------------------------------------------------------------------
%% gen_server Sync callbacks Definitions
%% ------------------------------------------------------------------

%% Returns proces state
handle_call(info, _From, State) ->
	{reply, State, State};

%% Returns node data
handle_call({get_node,Name}, _From, State = #cloud_ping_state{nodes = Nodes}) ->
	Res = case internal_get_node(Name,Nodes) of
		{ok, NodeRec} ->
			Props = lists:zip(record_info(fields, cloud_node), tl(tuple_to_list(NodeRec))),
			{ok, Props};
		{error, Reason} ->
			{error, Reason}
	end,
	{reply, Res, State};

%% Returns node pid
handle_call({get_node_pid,Name}, _From, State = #cloud_ping_state{nodes = Nodes}) ->
	Res = case internal_get_node(Name,Nodes) of
		{ok, NodeRec} -> NodeRec#cloud_node.pid;
		{error, _} -> undefined
	end,
	{reply, Res, State};

%% Register node
handle_call({register,ExtNode,ExtPid}, _From, State = #cloud_ping_state{nodes=Nodes,pid=Pid,name=Name}) ->
	case internal_subscribe_node({ExtNode,ExtPid},Nodes) of
		{ok, Nodes1} ->
			 { reply, {ok, {Name,Pid}}, State#cloud_ping_state{nodes = Nodes1} };
		{error, Reason} ->
			{ reply, {error, Reason}, State }
	end;

handle_call(_Request, _From, State) ->
	% ?DBG("handle_call default=~p",[_Request]),
	{reply, ok, State}.

%% ------------------------------------------------------------------
%% gen_server Async callbacks Definitions
%% ------------------------------------------------------------------

%% Ping nodes list
handle_cast(ping_nodes, State = #cloud_ping_state{nodes = Nodes}) ->
	internal_ping_nodes(Nodes),
	{noreply, State};

%% Ping node by name
handle_cast({ping_node,Node}, State = #cloud_ping_state{nodes=Nodes, name=IntNode, pid=IntPid}) ->
	State1 = case net_adm:ping(Node) of
		pong ->
			case rpc:call(Node,cloud_ping,register,[IntNode,IntPid]) of
				{badrpc, Reason} ->
					?DBG("RPC Send Error: ~p",[{badrpc, Reason}]),
					State;
				{error, Reason} ->
					?DBG("RPC Recv Error: ~p",[{error, Reason}]),
					State;
				{ok, {_ExtNode,ExtPid}} ->
					% ?DBG("Ext data received: ~p",[{_ExtNode,ExtPid}]),
					case internal_subscribe_node({Node,ExtPid},Nodes) of
						{ok, Nodes1} ->
							State#cloud_ping_state{nodes = Nodes1};
						_ ->
							State
					end
			end;
		pang ->
			% ?DBG("Pang for ~p",[Node]),
			State
	end,
	{noreply, State1};

handle_cast(_Msg, State) ->
	% ?DBG("handle_cast default=~p",[_Msg]),
	{noreply, State}.

%% ------------------------------------------------------------------
%% gen_server Ingo callbacks Definitions
%% ------------------------------------------------------------------

%% Ping nodes from init timeout
handle_info(ping_all, State = #cloud_ping_state{nodes=Nodes}) ->
	internal_ping_nodes(Nodes),
	{noreply, State};

%% Unreg node if node process died
handle_info({'DOWN',_Ref,process,Pid,_Reason}, State = #cloud_ping_state{nodes=Nodes}) ->
	State1 = case lists:keyfind(Pid,?PID_INDEX,Nodes) of
		false ->
			State;
		NodeRec ->
			?DBG("Cluster node `~p` down. Reason=~p",[NodeRec#cloud_node.name,_Reason]),
			NodeRec1 = NodeRec#cloud_node{status=0, pid=undefined, monitor=undefined},
			Nodes1 = lists:keyreplace(Pid,?PID_INDEX, Nodes, NodeRec1),
			State#cloud_ping_state{nodes = Nodes1}
	end,
	{noreply, State1};

handle_info(_Info, State) ->
	% ?DBG("handle_info default=~p",[_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	?DBG("Terminate=~p",[_Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% Get node from nodes list by name
internal_get_node(Name, Nodes)->
	case lists:keyfind(Name,2,Nodes) of
		false -> {error, notfound};
		Rec -> {ok, Rec}
	end.

%% Ping nodes
internal_ping_nodes(Nodes)->
	?DBG("Ping Nodes=~p",[Nodes]),
	[ ping_node(NodeRec#cloud_node.name) || NodeRec <- Nodes].

%% Register new active node in nodes list
internal_subscribe_node({Node,Pid},Nodes) ->
	% ?DBG("internal_subscribe_node=~p",[{Node,Pid}]),
	case lists:keyfind(Node,?NAME_INDEX,Nodes) of
		false ->
			{error,node_notfound};
		NodeRec ->
			Ref = erlang:monitor(process, Pid),
			NodeRec1 = NodeRec#cloud_node{status=1, pid=Pid, monitor=Ref},
			Nodes1 = lists:keyreplace(Node, ?NAME_INDEX, Nodes, NodeRec1),
			{ok, Nodes1}
	end.


-module(cloud_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

make_child(Mod, Type) ->
    make_child(Mod, Mod, Type, []).

make_child(Mod, Type, Opts) ->
    make_child(Mod, Mod, Type, Opts).

make_child(Mod, Identifier, Type, Opts) ->
	{Identifier, {Mod, start_link, Opts}, permanent, 5000, Type, [Mod]}.
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	start_link([]).	   

start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).
    
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Opts) ->
    {ok, { {one_for_one, 5, 10}, [
	    make_child(cloud_srv, worker)
	    , make_child(cloud_ping, worker, _Opts)
	]} }.
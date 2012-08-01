
-module(theNetwork_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	io:format("In sup start_link~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	io:format("Sup init callback~n"),
	TheNetworkChildApp = ?CHILD('theNetwork_app', worker),
	FlightSup = ?CHILD('flight_supervisor', supervisor),
    {ok, { {one_for_one, 5, 10}, [TheNetworkChildApp, FlightSup]} }.


-module(theNetwork_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_link/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start_link() ->
	io:format("Application start link...~n"),
	network_events_manager:start_link().
	
	
start() ->
	io:format("Application start/0...~n"),
	application:start(theNetwork).
	

start(_StartType, _StartArgs) ->
   io:format("Application start/2...~n"),
   theNetwork_sup:start_link().

stop(_State) ->
    ok.

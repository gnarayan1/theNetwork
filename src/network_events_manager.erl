%% Author: geeth
%% Created: Jul 28, 2012
%% Description: TODO: Add description to network_events_manager
-module(network_events_manager).

-define( SERVER, ?MODULE ).

%% External exports
-export([delete_handler/2, add_handler/2, start_link/0]).

-export( [publish/1, publish/2] ).

-export([publish_after/2]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	io:format("In network_events_manager start link~n"),
	try
		gen_event:start_link( {local, ?SERVER } )
	catch
		E:R ->
			io:format("Error Occured: ~p\n Reason:~p",[E,R])
	end.

add_handler( Handler, Args ) ->
	gen_event:add_handler( ?SERVER, Handler, Args).

delete_handler( Handler, Args ) ->
	gen_event:delete_handler( ?SERVER, Handler, Args).

publish( Event ) ->
	try
		gen_event:notify( ?SERVER, Event )
	catch
		E:R ->
			io:format("Error Occured: ~p\n Reason:~p",[E,R])
	end.

publish( EPID, Event ) ->
	gen_event:notify( EPID, Event ).

publish_after( Time_In_Millis, Event ) ->
	Fun = fun() ->
				  receive
				  after Time_In_Millis ->
						  publish(Event)
				  end
		  end,
	
	erlang:spawn(Fun).



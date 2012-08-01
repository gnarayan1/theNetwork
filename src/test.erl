%% Author: geeth
%% Created: Jul 31, 2012
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([create/0]).

%%
%% API Functions
%%
create() ->
	flight_fsm:start('flight_supervisor', '123', 60*60*1000),
	flight_fsm:start('flight_supervisor', '234', 60*60*1000),
	
	
	pnr_server:start(pnr_supervisor, {'XY3DQ', ['123', '234'], ['pax1', 'pax2']}),
	pnr_server:start(pnr_supervisor, {'RDY8O', ['123', '234'], ['pax3', 'pax4']}).
	

%%
%% Local Functions
%%


%%% -------------------------------------------------------------------
%%% Author  : geeth
%%% Description :
%%%
%%% Created : Jul 29, 2012
%%% -------------------------------------------------------------------
-module(flight_fsm).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/3]).


%% gen_fsm callbacks
-export([open_flight/1, get_seats/1, update_seat/3, start_link/2, stop/1, echo/1, init/1, flight_initiated/2, flight_initiated/3, flight_opened/2, flight_opened/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-define( MILLI_SECS_TO_STOP, 1 ).

-record(state, {id, timeout=0, seats=[], flt_num, dep_date, origin, time}).



%% ====================================================================
%% External functions
%% ====================================================================

start(Sup, Id, Time_Out) ->
	 supervisor:start_child (Sup, [Id, Time_Out]).

start_link( ID, Time_Out ) ->
    gen_fsm:start_link({local, ID}, ?MODULE, [ID, Time_Out], []).

stop( FSM_ID ) ->
	gen_fsm:sync_send_event( FSM_ID, stop, infinity ).

echo( FSM_ID ) ->
	gen_fsm:sync_send_event( FSM_ID, echo_state, infinity).

open_flight(FSM_ID) ->
	gen_fsm:sync_send_event( FSM_ID, open_flight, infinity).


get_seats(FSM_ID) ->
	gen_fsm:sync_send_event( FSM_ID, get_seats, infinity).


update_seat(FSM_ID, Seat, Pax) ->
	gen_fsm:sync_send_event( FSM_ID, {update_seat, Seat, Pax}, infinity).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([ID, Time_Out]) ->
	% Time_Out = 60*60*100,
	% [Flt_Num, Flt_Date, Flt_Origin, Flt_Time] = string:tokens(atom_to_list(ID), "_"),
    {ok, flight_initiated, #state{id=ID, timeout=Time_Out}, Time_Out }.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
flight_initiated(timeout, StateData) ->
    {stop, timeout, StateData};
flight_initiated(_Event, StateData) ->
    {next_state, flight_initiated, StateData#state.timeout}.

flight_opened(timeout, StateData) ->
    {stop, timeout, StateData};
flight_opened(_Event, StateData) ->
    {next_state, flight_opened, StateData#state.timeout}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
flight_initiated(open_flight, _From, StateData) ->
	Seats =get_flt_seats(StateData#state.id),
	spawn(flightmanage_events,publish,[{StateData#state.id, open} ] ),
    {reply, {ok, flight_opened}, flight_opened, StateData#state{seats=Seats}, StateData#state.timeout};
flight_initiated(stop, _From, StateData) ->
	Reply = {ok, stopping},
	spawn(flightmanage_events,publish,[{StateData#state.id, stop} ] ),
    {stop,flight_closed,Reply,StateData};
flight_initiated(echo_state, _From, StateData) ->
	 {reply, {flight_initiated, StateData}, flight_initiated, StateData, StateData#state.timeout };
flight_initiated(_Event, _From, StateData) ->
	Reply = {error, invalid_message},
	{reply, Reply, flight_initiated, StateData, StateData#state.timeout}.



flight_opened(get_seats, _From, StateData) ->
    {reply, StateData#state.seats, flight_opened, StateData};
flight_opened({update_seat, Seat, Pax}, _From, StateData) ->
    Curr_Seats = StateData#state.seats,
	{Updated_Seat, Seats} = update_seat_tup(Curr_Seats, Seat, Pax),
	% spawn(flightmanage_events,publish,[[StateData#state.id, Updated_Seat]] ),
    {reply, Seats, flight_opened, StateData#state{seats=Seats}};
flight_opened(stop, _From, StateData) ->
	Reply = {ok, stopping},
	% spawn(flightmanage_events,publish,[{StateData#state.id, stop} ] ),
    {stop,flight_closed,Reply,StateData};
flight_opened(echo_state, _From, StateData) ->
	 {reply, {flight_opened, StateData}, flight_opened, StateData, StateData#state.timeout };
flight_opened(_Event, _From, StateData) ->
	Reply = {error, invalid_message},
	{reply, Reply, flight_opened, StateData, StateData#state.timeout}.




%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
	% spawn(flightmanage_events,publish,[{StateData#state.id, stop} ] ),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_flt_seats(_Id) ->
	 [[{paxid, "pax1"}, {name, "John Doe"}, {seat, "Unassigned"}],
	 [{paxid, "pax2"}, {name, "Jane Roe"},  {seat, "Unassigned"}],
	 [{paxid, "pax3"}, {name, "Sammy Soe"}, {seat, "Unassigned"}],
	 [{paxid, "pax4"}, {name, "Karren Koe"}, {seat, "Unassigned"}],
	 [{paxid, "pax5"}, {name, "William Woe"}, {seat, "Unassigned"}]].
	 %[[{seat, "1A"},{status, "Available"}], [{seat, "1B"},{status, "Booked"}], [{seat, "1C"},{status, "Available"}]].


update_seat_tup(Seats, Seat, Pax) ->
	update_seat_tup(Seats, Seat, Pax, {[],[]}).

update_seat_tup([], _Seat, _Pax, {Seats, Acc}) ->
	{Seats, lists:reverse(Acc)};
update_seat_tup([First|Rest], Seat, Pax, {Seats,Acc}) ->
	[{paxid, CurrPaxId}, {name, CurrPaxName},{seat, _CurrSeat}] = First,
	io:format("CurrPaxId=~p~nPax=~p~n",[CurrPaxId, Pax]),
	New_Acc = case (CurrPaxId == Pax) of
				true ->
					New_Tup = lists:keyreplace(seat, 1, First, {seat, Seat}),
					{New_Tup, [New_Tup|Acc]};
				false ->
					{Seats, [First|Acc]}
	end,
	update_seat_tup(Rest, Seat, Pax, New_Acc).

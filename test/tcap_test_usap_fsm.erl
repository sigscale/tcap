%% tcap_test_usap_fsm.erl
%%
-module(tcap_test_usap_fsm).

-behaviour(gen_fsm).

-export([init/1, idle/2, handle_event/3, handle_sync_event/4]).

-record(statedata, {ct :: pid()}).

init([CT]) ->
erlang:display({?MODULE, ?LINE, CT, self()}),
	{ok, idle, #statedata{ct = CT}}.

idle(Event, #statedata{ct = CT} = StateData) ->
erlang:display({?MODULE, ?LINE, Event, CT}),
	CT ! Event,
	{next_state, idle, StateData}.

handle_event(_Event, StateName, StateData) ->
erlang:display({?MODULE, ?LINE, _Event}),
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
erlang:display({?MODULE, ?LINE, _Event}),
	{next_state, StateName, StateData}.


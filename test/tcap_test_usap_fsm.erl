%% tcap_test_usap_fsm.erl
%%
-module(tcap_test_usap_fsm).

-behaviour(gen_fsm).

-export([init/1, idle/2, handle_event/3, handle_sync_event/4]).

-record(statedata, {ct :: pid()}).

init([CT]) ->
	{ok, idle, #statedata{ct = CT}}.

idle(Event, #statedata{ct = CT} = StateData) ->
	CT ! Event,
	{next_state, idle, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.


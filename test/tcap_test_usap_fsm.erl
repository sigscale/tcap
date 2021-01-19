%% tcap_test_usap_fsm.erl
%%
-module(tcap_test_usap_fsm).
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_statem).

-export([callback_mode/0, init/1, idle/3, handle_event/4]).

-record(statedata, {ct :: pid()}).

callback_mode() ->
	[state_functions].

init([CT]) ->
	{ok, idle, #statedata{ct = CT}}.

idle(cast, Event, #statedata{ct = CT} = Data) ->
	CT ! Event,
	keep_state_and_data;
idle(info, _, _Data) ->
	keep_state_and_data.

handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.


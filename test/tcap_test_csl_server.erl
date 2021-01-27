%% tcap_test_csl_server.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This module implements a tcap_tco_server behaviour callback for
%%% use in the csl test group.
%%%
%%% The TC-User process is implemented in tcap_test_csl_fsm.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
-module(tcap_test_csl_server).
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(tcap_tco_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		send_primitive/2, start_aei/2]).

-record(state,
		{ct :: pid(),
		dha :: pid(),
		cco :: pid(),
		tcu :: pid()}).

init([CT]) ->
erlang:display({?MODULE, ?LINE, CT}),
	{ok, #state{ct = CT}}.

handle_call(_Request, _From, State) ->
erlang:display({?MODULE, ?LINE, _Request, _From, State}),
	{noreply, State}.

handle_cast(_Request, State) ->
erlang:display({?MODULE, ?LINE, _Request, State}),
	{noreply, State}.

handle_info({'N', _, indication, _} = Primitive, State) ->
erlang:display({?MODULE, ?LINE, Primitive, State}),
	{primitive, Primitive, State};
handle_info(_Info, State) ->
erlang:display({?MODULE, ?LINE, _Info, State}),
	{noreply, State}.

send_primitive(Primitive, #state{ct = CT} = _State) ->
erlang:display({?MODULE, ?LINE, Primitive, _State}),
	CT ! Primitive.

start_aei(_DialoguePortion, #state{ct = CT} = State) ->
	{ok, TCU} = gen_statem:start_link(tcap_test_csl_fsm, [CT], [{debug, [trace]}]),
erlang:display({?MODULE, ?LINE, TCU}),
	{ok, DHA, CCO} = tcap:open(self(), TCU),
	ok = gen_statem:call(TCU, {csl_open, DHA, CCO}),
	NewState = State#state{dha = DHA, cco = CCO, tcu = TCU},
	{ok, DHA, CCO, TCU, NewState}.


%%% tcap_test_csl_fsm.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2021 SigScale Global Inc.
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%    - Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    - Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in
%%%      the documentation and/or other materials provided with the
%%%      distribution.
%%%    - Neither the name of Motivity Telecom nor the names of its
%%%      contributors may be used to endorse or promote products derived
%%%      from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This module implements a TC-User process for use in the csl
%%% 	 test group of the `tcap_q787_SUITE' test suite.
%%%
%%% 	The TCO process is implemented in `tcap_test_csl_server'.
%%%
-module(tcap_test_csl_fsm).
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_statem).

-export([callback_mode/0, init/1, handle_event/4]).

-record(statedata,
		{dha :: pid(),
		cco :: pid(),
		tcu :: pid(),
		ct :: pid()}).

callback_mode() ->
	[handle_event_function].

init([CT]) ->
	{ok, idle, #statedata{ct = CT}}.

handle_event({call, From}, {csl_open, DHA, CCO}, _State, Data) ->
	NewData = Data#statedata{dha = DHA, cco = CCO},
	Actions = [{reply, From, ok}],
	{keep_state, NewData, Actions};
handle_event(cast,
		{tc_send, {'TC', Name, request, _} = Primitive}, _State,
		#statedata{dha = DHA} = _Data) when Name == 'UNI';
		Name == 'BEGIN'; Name == 'CONTINUE'; Name == 'END';
		Name == 'U-ABORT' ->
	gen_server:cast(DHA, Primitive),
	keep_state_and_data;
handle_event(cast,
		{tc_send, {'TC', Name, request, _} = Primitive}, _State,
		#statedata{cco = CCO} = _Data) when Name == 'INVOKE';
		Name == 'RESULT-L'; Name == 'U-ERROR'; Name == 'U-CANCEL';
		Name == 'U-REJECT' ->
	gen_server:cast(CCO, Primitive),
	keep_state_and_data;
handle_event(cast, {'TC', _, indication, _} = Primitive, _State,
		#statedata{ct = CT} = _Data) ->
	CT ! Primitive,
	keep_state_and_data;
handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.


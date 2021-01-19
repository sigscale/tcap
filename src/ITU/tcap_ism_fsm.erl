%%% tcap_ism_fsm.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2010-2013 Harald Welte,
%%% 	 	2021 SigScale Global Inc.
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @author Vance Shipley <vances@sigscale.org>
%%% @end
%%%
%%% Copyright (c) 2010-2013, Harald Welte <laforge@gnumonks.org>
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module implements an Invocation State Machine (ISM) functional
%%% 	block within the component sub-layer of ITU TCAP.
%%%
%%% @reference ANSI T1.114.4 Transaction Capabilities Procedures 
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_ism_fsm).
-copyright('Copyright (c) 2010-2013 Harald Welte').
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Harald Welte <laforge@gnumonks.org>').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, handle_event/4, callback_mode/0,
			terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([idle/3, sent_class_1/3, sent_class_2/3, sent_class_3/3,
	 sent_class_4/3, wait_for_reject/3]).

%% record definitions for TC-User primitives
-include("tcap.hrl").

-type state() :: idle | sent_class_1 | sent_class_2
		| sent_class_3 | sent_class_4 | wait_for_reject.

%% the invocation_fsm state data
-record(statedata,
	{usap :: pid(),
	dialogueId :: 0..4294967295,
	invokeId :: 0..255,
	cco :: pid(),
	op_class :: 1..4,
	invoke_timeout :: non_neg_integer(),
	reject_timeout = 1000 :: non_neg_integer()}).
-type statedata() :: #statedata{}.

%%----------------------------------------------------------------------
%%  The tcap_tsm_fsm gen_statem callbacks
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions].

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State, Data} | {ok, State, Data, Actions}
				| ignore | {stop, Reason},
		State :: state(),
		Data :: statedata(),
		Actions :: Action | [Action],
		Action :: gen_statem:action(),
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%%
%% 	Initialize a Invocation State Machine (TSM) process.
%%
%% 	Reference: Figure A.7/Q.774 (sheet 1 of 6)
%%
%% @see //stdlib/gen_statem:init/1
%% @private
init([Usap, DialogueId, InvokeId, CcoPid, OpClass, InvTimeout]) ->
	process_flag(trap_exit, true),
	Data = #statedata{usap = Usap, dialogueId = DialogueId,
			invokeId = InvokeId, cco = CcoPid,
			op_class = OpClass, invoke_timeout = InvTimeout},
	{ok, idle, Data}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @private
% CCO -> ISM: Operation sent
idle(cast, 'operation-sent', #statedata{op_class = 1,
		invoke_timeout = Time} = Data) ->
	Actions = [{timeout, Time, invoke}],
	{next_state, sent_class_1, Data, Actions};
idle(cast, 'operation-sent', #statedata{op_class = 2,
		invoke_timeout = Time} = Data) ->
	Actions = [{timeout, Time, invoke}],
	{next_state, sent_class_2, Data, Actions};
idle(cast, 'operation-sent', #statedata{op_class = 3,
		invoke_timeout = Time} = Data) ->
	Actions = [{timeout, Time, invoke}],
	{next_state, sent_class_3, Data, Actions};
idle(cast, 'operation-sent', #statedata{op_class = 4,
		invoke_timeout = Time} = Data) ->
	Actions = [{timeout, Time, invoke}],
	{next_state, sent_class_4, Data, Actions};
idle(info, _, _Data) ->
	keep_state_and_data.

-spec sent_class_1(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>sent_class_1</em> state.
%% @private
sent_class_1(cast, #'TC-RESULT-L'{} = TcParms,
		#statedata{usap = USAP, reject_timeout = Time} = Data) ->
	% Figure A.7/Q.774 (2 of 6)
	% TC-RESULT-L.ind to user
	gen_statem:cast(USAP, {'TC', 'RESULT-L', indication, TcParms}),
	% start reject timer
	Actions = [{timeout, Time, reject}],
	{next_state, wait_for_reject, Data, Actions};
sent_class_1(cast, #'TC-U-ERROR'{} = TcParms,
		#statedata{usap = USAP, reject_timeout = Time} = Data) ->
	% Figure A.7/Q.774 (2 of 6)
	% TC-U-ERROR.ind to user
	gen_statem:cast(USAP, {'TC', 'U-ERROR', indication, TcParms}),
	% start reject timer
	Actions = [{timeout, Time, reject}],
	{next_state, wait_for_reject, Data, Actions};
sent_class_1(cast, #'TC-RESULT-NL'{} = TcParms,
		#statedata{usap = USAP} = Data) ->
	% Figure A.7/Q.774 (2 of 6)
	% TC-RESULT-NL.ind to user
	gen_statem:cast(USAP, {'TC', 'RESULT-NL', indication, TcParms}),
	{next_state, sent_class_1, Data};
sent_class_1(timeout, invoke, #statedata{usap = USAP,
		dialogueId = DID, invokeId = IID} = Data) ->
	% invocation timer expiry
	% TC-L-CANCEL.ind to user
	TcParms = #'TC-L-CANCEL'{dialogueID = DID, invokeID = IID},
	gen_statem:cast(USAP, {'TC', 'L-CANCEL', indication, TcParms}),
	{stop, normal, Data};
sent_class_1(cast, 'terminate', Data) ->
	{stop, normal, Data};
sent_class_1(info, _, _Data) ->
	keep_state_and_data.

-spec sent_class_2(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>sent_class_2</em> state.
%% @private
sent_class_2(cast, #'TC-U-ERROR'{} = TcParms,
		#statedata{usap = USAP, reject_timeout = Time} = Data) ->
	% TC-U-ERROR.ind to user
	gen_statem:cast(USAP, {'TC', 'U-ERROR', indication, TcParms}),
	% start reject timer
	Actions = [{timeout, Time, reject}],
	{next_state, wait_for_reject, Data, Actions};
sent_class_2(cast, Op, #statedata{cco = CCO} = Data)
		when is_record(Op, 'TC-RESULT-L');
		is_record(Op, 'TC-RESULT-NL') ->
	% Generate REJ component to CCO
	Problem = {'ReturnResultProblem', resultResponseUnexpected},
	Reject = #'TC-R-REJECT'{dialogueID = Data#statedata.dialogueId,
				invokeID = Data#statedata.invokeId,
				problemCode = Problem},
	gen_server:cast(CCO, {reject_component, Reject}),
	% terminate
	{stop, normal, Data};
sent_class_2(timeout, invoke, #statedata{usap = USAP,
		dialogueId = DID, invokeId = IID} = Data) ->
	% invocation timer expiry
	% TC-L-CANCEL.ind to user
	TcParms = #'TC-L-CANCEL'{dialogueID = DID, invokeID = IID},
	gen_statem:cast(USAP, {'TC', 'L-CANCEL', indication, TcParms}),
	{stop, normal, Data};
sent_class_2(cast, 'terminate', Data) ->
	% terminate
	{stop, normal, Data};
sent_class_2(info, _, _Data) ->
	keep_state_and_data.

-spec sent_class_3(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>sent_class_3</em> state.
%% @private
sent_class_3(cast, #'TC-RESULT-L'{} = TcParms,
		#statedata{usap = USAP, reject_timeout = Time} = Data) ->
	% Figure A.7/Q.774 (5 of 6)
	% TC-RESULT-L.ind to user
	gen_statem:cast(USAP, {'TC', 'RESULT-L', indication, TcParms}),
	% start reject timer
	Actions = [{timeout, Time, reject}],
	{next_state, wait_for_reject, Data, Actions};
sent_class_3(cast, #'TC-RESULT-NL'{} = TcParms,
		#statedata{usap = USAP} = Data) ->
	% TC-RESULT-NL.ind to user
	gen_statem:cast(USAP, {'TC', 'RESULT-NL', indication, TcParms}),
	{next_state, sent_class_3, Data};
sent_class_3(timeout, invoke, #statedata{usap = USAP,
		dialogueId = DID, invokeId = IID} = Data) ->
	% invocation timer expiry
	% TC-L-CANCEL.ind to user
	TcParms = #'TC-L-CANCEL'{dialogueID = DID, invokeID = IID},
	gen_statem:cast(USAP, {'TC', 'L-CANCEL', indication, TcParms}),
	{stop, normal, Data};
sent_class_3(cast, 'terminate', Data) ->
	% terminate
	{stop, normal, Data};
sent_class_3(info, _, _Data) ->
	keep_state_and_data.

-spec sent_class_4(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>sent_class_4</em> state.
%% @private
sent_class_4(cast, 'terminate', Data) ->
	% terminate
	{stop, normal, Data};
sent_class_4(cast, _Op, #statedata{cco = CCO} = Data) ->
	% Figure A.7/Q.774 (6 of 6)
	% generate REJ component to CCO
	Problem = {'ReturnResultProblem', resultResponseUnexpected},
	Reject = #'TC-R-REJECT'{dialogueID = Data#statedata.dialogueId,
				invokeID = Data#statedata.invokeId,
				problemCode = Problem},
	gen_server:cast(CCO, {reject_component, Reject}),
	% terminate
	{stop, normal, Data};
sent_class_4(timeout, invoke, #statedata{usap = USAP,
		dialogueId = DID, invokeId = IID} = Data) ->
	% invocation timer expiry
	% TC-L-CANCEL.ind to user
	TcParms = #'TC-L-CANCEL'{dialogueID = DID, invokeID = IID},
	gen_statem:cast(USAP, {'TC', 'L-CANCEL', indication, TcParms}),
	{stop, normal, Data};
sent_class_4(info, _, _Data) ->
	keep_state_and_data.

-spec wait_for_reject(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_for_reject</em> state.
%% @private
wait_for_reject(cast, 'terminate', Data) ->
	{stop, normal, Data};
wait_for_reject(timeout, reject, Data) ->
	% reject timer expiry
	% terminate
	{stop, normal, Data};
wait_for_reject(info, _, _Data) ->
	keep_state_and_data.

-spec handle_event(EventType, EventContent, State, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(State).
%% @doc Handles events received in any state.
%% @private
%%
handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, _Data) ->
	ok.

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.


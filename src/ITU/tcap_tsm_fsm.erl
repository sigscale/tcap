%%% tcap_tsm_fsm.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2004-2005 Motivity Telecom,
%%% 		2010-2011 Harald Welte,
%%% 	 	2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org>
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
%%% Copyright (c) 2010-2011, Harald Welte
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
%%% 	module implements a Transaction State Machine (TCM) functional
%%% 	block within the transaction sub-layer of ITU TCAP.
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_tsm_fsm).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-copyright('Copyright (c) 2010-2011 Harald Welte').
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').
-author('Harald Welte <laforge@gnumonks.org>').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, handle_event/4, callback_mode/0,
			terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([idle/3, initiation_sent/3, initiation_received/3, active/3]).

-include_lib("kernel/include/logger.hrl").

-type state() :: idle | initiation_sent | initiation_received | active.

-include("DialoguePDUs.hrl").
-include("tcap.hrl").
-include_lib("sccp/include/sccp_primitive.hrl").
-include("TCAPMessages.hrl").

%% the transaction_fsm state data
-record(statedata,
		{tco :: pid(),
		dha :: pid(),
		localTID :: tcap_tco_server:tid(),
		remoteTID :: tcap_tco_server:tid(),
		local_address,
		remote_address}).
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
%% 	Initialize a Transaction State Machine (TSM) process.
%%
%% @see //stdlib/gen_statem:init/1
%% @private
init([TCO, DHA, TID]) ->
	link(TCO),
	process_flag(trap_exit, true),
	Data = #statedata{localTID = TID, tco = TCO, dha = DHA},
	{ok, idle, Data}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @private
% started by remote
% reference: Figure A.4/Q.774 (sheet 1 of 5)
idle(cast, {'BEGIN', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		callingAddress = CallingAddress, calledAddress = CalledAddress,
		userData = #'Begin'{otid = OTID, dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#statedata{localTID = LocalTID, dha = DHA} = Data) ->
	%% Store remote address and remote TID
	NewData = Data#statedata{remote_address = CallingAddress,
			local_address = CalledAddress, remoteTID = OTID},
	QOS = {SequenceControl, ReturnOption},
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-BEGIN'{qos = QOS,
			destAddress = CalledAddress, origAddress = CallingAddress,
			transactionID = LocalTID, userData = UserData},
	%% TR-BEGIN CSL <- TSL
	gen_statem:cast(DHA, {'TR', 'BEGIN', indication, TrParms}),
	{next_state, initiation_received, NewData};
% started by TR-User
% reference: Figure A.4/Q.774 (sheet 1 of 5)
idle(cast, {'BEGIN', transaction,
		#'TR-BEGIN'{origAddress = OrigAddress,
		userData = UserData} = BeginParms},
		#statedata{localTID = Otid, tco = TCO} = Data) ->
	%% Store local address
	%% NOTE - This may be provided by TC-user or be implicitly associated with
	%%        the access point at which the N-UNITDATA primitive is issued. 
	NewData = Data#statedata{local_address = OrigAddress},
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	Begin = #'Begin'{otid = <<Otid:32/big>>, dialoguePortion = DialoguePortion,
			components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {'begin', Begin}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(BeginParms),
	SccpParms = #'N-UNITDATA'{calledAddress = BeginParms#'TR-BEGIN'.destAddress,
			callingAddress = BeginParms#'TR-BEGIN'.origAddress,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, initiation_sent, NewData};
idle(info, _, _Data) ->
	keep_state_and_data.

-spec initiation_received(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>initiation_received</em> state.
%% @private
%
% reference: Figure A.4/Q.774 (sheet 2 of 5)
%
% Continue from TR-User
initiation_received(cast, {'CONTINUE', transaction,
		#'TR-CONTINUE'{origAddress = OrigAddress,
		userData = UserData} = ContParms},
		#statedata{localTID = Otid, remoteTID = Dtid, tco = TCO} = Data) ->
	%% Store new local address if it is provided by User
	NewData = case OrigAddress of
		undefined ->
			Data;
		NewAddress ->
			Data#statedata{local_address = NewAddress}
	end,
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	Continue = #'Continue'{otid = <<Otid:32/big>>, dtid = <<Dtid:32/big>>,
				dialoguePortion = DialoguePortion, components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {continue, Continue}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(ContParms),
	SccpParms = #'N-UNITDATA'{calledAddress = NewData#statedata.remote_address,
			callingAddress = NewData#statedata.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, active, NewData};
% End from TR-User (prearranged)
initiation_received(cast, {'END', transaction,
		#'TR-END'{termination = prearranged} = _EndParms},
		#statedata{localTID = TID} = Data) ->
	{stop, {shutdown, {?MODULE, TID}}, Data};
% End from TR-User (not prearranged)
initiation_received(cast, {'END', transaction,
		#'TR-END'{userData = UserData} = EndParms},
		#statedata{localTID = TID, remoteTID = Dtid, tco = TCO} = Data) ->
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	End = #'End'{dtid = <<Dtid:32/big>>, dialoguePortion = DialoguePortion,
		     components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {'end', End}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(EndParms),
	SccpParms = #'N-UNITDATA'{calledAddress = Data#statedata.remote_address,
			callingAddress = Data#statedata.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, {?MODULE, TID}}, Data};
% Abort from TR-User
initiation_received(cast, {'ABORT', transaction,
		#'TR-U-ABORT'{userData = UserData} = AbortParms},
		#statedata{localTID = TID, tco = TCO} = Data) ->
	Cause = UserData#'TR-user-data'.dialoguePortion,
	Abort = #'Abort'{reason = {'u-abortCause', Cause}},
	{ok, TPDU} = 'TR':encode('TCMessage', {abort, Abort}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(AbortParms),
	SccpParms = #'N-UNITDATA'{calledAddress = Data#statedata.remote_address,
			callingAddress = Data#statedata.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, {?MODULE, TID}}, Data};
initiation_received(info, _, _Data) ->
	keep_state_and_data.
	
-spec initiation_sent(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>initiation_sent</em> state.
%% @private
%
% reference: Figure A.4/Q.774 (sheet 2 of 5)
%
% Continue from remote
initiation_sent(cast, {'CONTINUE', received,
		#'N-UNITDATA'{callingAddress = CallingAddress,
		sequenceControl = SequenceControl, returnOption = ReturnOption,
		userData = #'Continue'{otid = OTID,
		dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#statedata{localTID = LocalTID, dha = DHA} = Data) ->
	%% Store remote address and remote TID
	NewData = Data#statedata{remote_address = CallingAddress,
			remoteTID = OTID},
	QOS = {SequenceControl, ReturnOption},
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
				   componentPortion = Components},
	TrParms = #'TR-CONTINUE'{qos = QOS,
			transactionID = LocalTID,
			userData = UserData},
	gen_statem:cast(DHA, {'TR', 'CONTINUE', indication, TrParms}),
	{next_state, active, NewData};
% End from remote
initiation_sent(cast, {'END', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		userData = #'End'{dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#statedata{localTID = TID, dha = DHA} = Data) ->
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-END'{qos = {SequenceControl, ReturnOption},
			transactionID = TID, userData = UserData},
	gen_statem:cast(DHA, {'TR', 'END', indication, TrParms}),
	{stop, {shutdown, {?MODULE, TID}}, Data};
% Abort from remote
initiation_sent(cast, {'ABORT', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption, userData = Abort} = _SccpParms},
		#statedata{localTID = TID, dha = DHA} = Data) ->
	%% TR-U-ABORT?
	case Abort#'Abort'.reason of
		{'p-abortCause', Cause} ->
			TrParms = #'TR-P-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, pAbort = Cause},
			gen_statem:cast(DHA, {'TR', 'P-ABORT', indication, TrParms});
		{'u-abortCause', Cause} ->
			UserData = #'TR-user-data'{dialoguePortion = Cause},
			TrParms = #'TR-U-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, userData = UserData},
			gen_statem:cast(DHA, {'TR', 'U-ABORT', indication, TrParms})
	end,
	{stop, {shutdown, {?MODULE, TID}}, Data};
% End from TR-User
initiation_sent(cast, {'END', transaction,
		#'TR-END'{} = _EndParms},
		#statedata{localTID = TID} = Data) ->
	{stop, {shutdown, {?MODULE, TID}}, Data};
%% Abort from TR-User
initiation_sent(cast, {'ABORT', transaction,
		#'TR-U-ABORT'{} = _AbortParms},
		#statedata{localTID = TID} = Data) ->
	{stop, {shutdown, {?MODULE, TID}}, Data};
initiation_sent(info, _, _Data) ->
	keep_state_and_data.
	
-spec active(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>active</em> state.
%% @private
%
% active state handler
%
% reference: Figure A.4/Q.774 (sheet 2 of 5)
%
% Continue received from remote
active(cast, {'CONTINUE', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		userData = #'Continue'{dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#statedata{localTID = LocalTID, dha = DHA} = _Data) ->
	QOS = {SequenceControl, ReturnOption},
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-CONTINUE'{qos = QOS,
			transactionID = LocalTID, userData = UserData},
	%% TR-CONTINUE indication CSL <- TSL
	gen_statem:cast(DHA, {'TR', 'CONTINUE', indication, TrParms}),
	keep_state_and_data;
% Continue from TR-User
active(cast, {'CONTINUE', transaction,
		#'TR-CONTINUE'{userData = UserData} = ContParms},
		#statedata{localTID = Otid, remoteTID = Dtid, tco = TCO} = Data) ->
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	Continue = #'Continue'{otid = <<Otid:32/big>>, dtid = <<Dtid:32/big>>,
				dialoguePortion = DialoguePortion, components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {continue, Continue}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(ContParms),
	SccpParms = #'N-UNITDATA'{calledAddress = Data#statedata.remote_address,
			callingAddress = Data#statedata.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	keep_state_and_data;
% End from remote
active(cast, {'END', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		userData = #'End'{dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#statedata{localTID = TID, dha = DHA} = Data) ->
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-END'{qos = {SequenceControl, ReturnOption},
			transactionID = TID, userData = UserData},
	%% TR-END indication CSL <- TSL
	gen_statem:cast(DHA, {'TR', 'END', indication, TrParms}),
	{stop, {shutdown, {?MODULE, TID}}, Data};
% End from TR-User (prearranged)
active(cast, {'END', transaction,
		#'TR-END'{termination = prearranged} = _EndParms},
		#statedata{localTID = TID} = Data) ->
	{stop, {shutdown, {?MODULE, TID}}, Data};
% End from TR-User (not prearranged)
active(cast, {'END', transaction,
		#'TR-END'{userData = UserData} = EndParms},
		#statedata{localTID = TID, remoteTID = Dtid, tco = TCO} = Data) ->
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	End = #'End'{dtid = <<Dtid:32/big>>, dialoguePortion = DialoguePortion,
		     components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {'end', End}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(EndParms),
	SccpParms = #'N-UNITDATA'{calledAddress = Data#statedata.remote_address,
			callingAddress = Data#statedata.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, {?MODULE, TID}}, Data};
% Abort received from remote
active(cast, {'ABORT', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption, userData = Abort} = _SccpParms},
		#statedata{localTID = TID, dha = DHA} = Data) ->
	%% TR-U-ABORT?
	case Abort#'Abort'.reason of
		{'p-abortCause', Cause} ->  % No
			TrParms = #'TR-P-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, pAbort = Cause},
			%% TR-P-ABORT indication CSL <- TSL
			gen_statem:cast(DHA, {'TR', 'P-ABORT', indication, TrParms});
		{'u-abortCause', Cause} ->  % Yes
			UserData = #'TR-user-data'{dialoguePortion = Cause},
			TrParms = #'TR-U-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, userData = UserData},
			%% TR-U-ABORT indication CSL <- TSL
			gen_statem:cast(DHA, {'TR', 'U-ABORT', indication, TrParms})
	end,
	{stop, {shutdown, {?MODULE, TID}}, Data};
% Abort from TR-User
active(cast, {'ABORT', transaction,
		#'TR-U-ABORT'{userData = UserData} = AbortParms},
		#statedata{localTID = TID, tco = TCO} = Data) 
		when is_record(AbortParms, 'TR-U-ABORT') ->
	Cause = UserData#'TR-user-data'.dialoguePortion,
	Abort = #'Abort'{reason = {'u-abortCause', Cause}},
	%% Assemble TR-portion of ABORT message
	{ok, TPDU} = 'TR':encode('TCMessage', {abort, Abort}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(AbortParms),
	SccpParms = #'N-UNITDATA'{calledAddress = Data#statedata.remote_address,
			callingAddress = Data#statedata.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, {?MODULE, TID}}, Data};
active(info, _, _Data) ->
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

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

% QoS from TR-* primitive to {SequenceControl, ReturnOpt}
qos_from_tr_qos(undefined) ->
	{false, true};
qos_from_tr_qos({Seq, Ret}) ->
	{Seq, Ret}.

qos_from_tr_prim(#'TR-CONTINUE'{qos=Qos}) ->
	qos_from_tr_qos(Qos);
qos_from_tr_prim(#'TR-BEGIN'{qos=Qos}) ->
	qos_from_tr_qos(Qos);
qos_from_tr_prim(#'TR-END'{qos=Qos}) ->
	qos_from_tr_qos(Qos);
qos_from_tr_prim(#'TR-U-ABORT'{qos=Qos}) ->
	qos_from_tr_qos(Qos).


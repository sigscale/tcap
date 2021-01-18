%%% tcap_tsm_fsm.erl
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom,
%%% 		2010-2011 Harald Welte,
%%% 		2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
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
%%%---------------------------------------------------------------------
%%%
%%% @doc Transaction State Machine (TCM) functional block within the
%%% 		transaction sub-layer of ITU TCAP.
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_tsm_fsm).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-copyright('Copyright (c) 2010-2011 Harald Welte').
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').
-author('Harald Welte <laforge@gnumonks.org>').

-behaviour(gen_fsm).

%% call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		terminate/3, code_change/4]).

%% transaction_fsm state callbacks 
-export([idle/2, initiation_sent/2, initiation_received/2, active/2]).

%% record definitions for TR-User primitives
-include("tcap.hrl").
%% record definitions for N-User primitives
-include("sccp_primitive.hrl").
%% record definitions for TCAP messages
-include("TCAPMessages.hrl").

%% the transaction_fsm state data
-record(state,
		{tco :: pid(),
		dha :: pid(),
		localTID :: tcap_tco_server:tid(),
		remoteTID :: tcap_tco_server:tid(),
		local_address,
		remote_address}).

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% initialize the server
init([TCO, DHA, TID]) ->
	link(TCO),
	process_flag(trap_exit, true),
	{ok, idle, #state{localTID = TID, tco = TCO, dha = DHA}}.

%%%
%%% idle state handler
%%% 

%% started by remote
%% reference: Figure A.4/Q.774 (sheet 1 of 5)
idle({'BEGIN', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		callingAddress = CallingAddress, calledAddress = CalledAddress,
		userData = #'Begin'{otid = OTID, dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#state{localTID = LocalTID, dha = DHA} = State) ->
	%% Store remote address and remote TID
	NewState = State#state{remote_address = CallingAddress,
			local_address = CalledAddress, remoteTID = OTID},
	QOS = {SequenceControl, ReturnOption},
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-BEGIN'{qos = QOS,
			destAddress = CalledAddress, origAddress = CallingAddress,
			transactionID = LocalTID, userData = UserData},
	%% TR-BEGIN CSL <- TSL
	gen_fsm:send_event(DHA, {'TR', 'BEGIN', indication, TrParms}),
	{next_state, initiation_received, NewState};

%% started by TR-User
%% reference: Figure A.4/Q.774 (sheet 1 of 5)
idle({'BEGIN', transaction,
		#'TR-BEGIN'{origAddress = OrigAddress,
		userData = UserData} = BeginParms},
		#state{localTID = Otid, tco = TCO} = State) ->
	%% Store local address
	%% NOTE - This may be provided by TC-user or be implicitly associated with
	%%        the access point at which the N-UNITDATA primitive is issued. 
	NewState = State#state{local_address = OrigAddress},
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
	{next_state, initiation_sent, NewState}.


%%%
%%% initiation_received state handler
%%%
%%% reference: Figure A.4/Q.774 (sheet 2 of 5)

%% Continue from TR-User
initiation_received({'CONTINUE', transaction,
		#'TR-CONTINUE'{origAddress = OrigAddress,
		userData = UserData} = ContParms},
		#state{localTID = Otid, remoteTID = Dtid, tco = TCO} = State) ->
	%% Store new local address if it is provided by User
	case OrigAddress of
		undefined ->
			NewState = State;
		NewAddress ->
			NewState = State#state{local_address = NewAddress}
	end,
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	Continue = #'Continue'{otid = <<Otid:32/big>>, dtid = <<Dtid:32/big>>,
				dialoguePortion = DialoguePortion, components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {continue, Continue}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(ContParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = NewState#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, active, NewState};

%% End from TR-User (prearranged)
initiation_received({'END', transaction,
		#'TR-END'{termination = prearranged} = _EndParms},
		#state{localTID = TID} = State) ->
	{stop, {shutdown, TID}, State};
%% End from TR-User (not prearranged)
initiation_received({'END', transaction,
		#'TR-END'{userData = UserData} = EndParms},
		#state{localTID = TID, remoteTID = Dtid, tco = TCO} = State) ->
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	End = #'End'{dtid = <<Dtid:32/big>>, dialoguePortion = DialoguePortion,
		     components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {'end', End}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(EndParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, TID}, State};

%% Abort from TR-User
initiation_received({'ABORT', transaction,
		#'TR-U-ABORT'{userData = UserData} = AbortParms},
		#state{localTID = TID, tco = TCO} = State) ->
	Cause = UserData#'TR-user-data'.dialoguePortion,
	Abort = #'Abort'{reason = {'u-abortCause', Cause}},
	{ok, TPDU} = 'TR':encode('TCMessage', {abort, Abort}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(AbortParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, TID}, State}.
	

%%%
%%% initiation_sent state handler
%%%
%%% reference: Figure A.4/Q.774 (sheet 2 of 5)

%% Continue from remote
initiation_sent({'CONTINUE', received,
		#'N-UNITDATA'{callingAddress = CallingAddress,
		sequenceControl = SequenceControl, returnOption = ReturnOption,
		userData = #'Continue'{otid = OTID,
		dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#state{localTID = LocalTID, dha = DHA} = State) ->
	%% Store remote address and remote TID
	NewState = State#state{remote_address = CallingAddress,
			remoteTID = OTID},
	QOS = {SequenceControl, ReturnOption},
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
				   componentPortion = Components},
	TrParms = #'TR-CONTINUE'{qos = QOS,
			transactionID = LocalTID,
			userData = UserData},
	gen_fsm:send_event(DHA, {'TR', 'CONTINUE', indication, TrParms}),
	{next_state, active, NewState};

%% End from remote
initiation_sent({'END', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		userData = #'End'{dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#state{localTID = TID, dha = DHA} = State) ->
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-END'{qos = {SequenceControl, ReturnOption},
			transactionID = TID, userData = UserData},
	gen_fsm:send_event(DHA, {'TR', 'END', indication, TrParms}),
	{stop, {shutdown, TID}, State};

%% Abort from remote
initiation_sent({'ABORT', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption, userData = Abort} = _SccpParms},
		#state{localTID = TID, dha = DHA} = State) ->
	%% TR-U-ABORT?
	case Abort#'Abort'.reason of
		{'p-abortCause', Cause} ->
			TrParms = #'TR-P-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, pAbort = Cause},
			gen_fsm:send_event(DHA, {'TR', 'P-ABORT', indication, TrParms});
		{'u-abortCause', Cause} ->
			UserData = #'TR-user-data'{dialoguePortion = Cause},
			TrParms = #'TR-U-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, userData = UserData},
			gen_fsm:send_event(DHA, {'TR', 'U-ABORT', indication, TrParms})
	end,
	{stop, {shutdown, TID}, State};

%% End from TR-User
initiation_sent({'END', transaction,
		#'TR-END'{} = _EndParms},
		#state{localTID = TID} = State) ->
	{stop, {shutdown, TID}, State};

%% Abort from TR-User
initiation_sent({'ABORT', transaction,
		#'TR-U-ABORT'{} = _AbortParms},
		#state{localTID = TID} = State) ->
	{stop, {shutdown, TID}, State}.
	

%%%
%%% active state handler
%%%
%%% reference: Figure A.4/Q.774 (sheet 2 of 5)

%% Continue received from remote
active({'CONTINUE', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		userData = #'Continue'{dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#state{localTID = LocalTID, dha = DHA} = State) ->
	QOS = {SequenceControl, ReturnOption},
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-CONTINUE'{qos = QOS,
			transactionID = LocalTID, userData = UserData},
	%% TR-CONTINUE indication CSL <- TSL
	gen_fsm:send_event(DHA, {'TR', 'CONTINUE', indication, TrParms}),
	{next_state, active, State};


%% Continue from TR-User
active({'CONTINUE', transaction,
		#'TR-CONTINUE'{userData = UserData} = ContParms},
		#state{localTID = Otid, remoteTID = Dtid, tco = TCO} = State) ->
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	Continue = #'Continue'{otid = <<Otid:32/big>>, dtid = <<Dtid:32/big>>,
				dialoguePortion = DialoguePortion, components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {continue, Continue}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(ContParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, active, State};

%% End from remote
active({'END', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption,
		userData = #'End'{dialoguePortion = DialoguePortion,
		components = Components}} = _SccpParms},
		#state{localTID = TID, dha = DHA} = State) ->
	UserData = #'TR-user-data'{dialoguePortion = DialoguePortion,
			componentPortion = Components},
	TrParms = #'TR-END'{qos = {SequenceControl, ReturnOption},
			transactionID = TID, userData = UserData},
	%% TR-END indication CSL <- TSL
	gen_fsm:send_event(DHA, {'TR', 'END', indication, TrParms}),
	{stop, {shutdown, TID}, State};

%% End from TR-User (prearranged)
active({'END', transaction,
		#'TR-END'{termination = prearranged} = _EndParms},
		#state{localTID = TID} = State) ->
	{stop, {shutdown, TID}, State};
%% End from TR-User (not prearranged)
active({'END', transaction,
		#'TR-END'{userData = UserData} = EndParms},
		#state{localTID = TID, remoteTID = Dtid, tco = TCO} = State) ->
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	End = #'End'{dtid = <<Dtid:32/big>>, dialoguePortion = DialoguePortion,
		     components = ComponentPortion},
	{ok, TPDU} = 'TR':encode('TCMessage', {'end', End}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(EndParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, TID}, State};

%% Abort received from remote
active({'ABORT', received,
		#'N-UNITDATA'{sequenceControl = SequenceControl,
		returnOption = ReturnOption, userData = Abort} = _SccpParms},
		#state{localTID = TID, dha = DHA} = State) ->
	%% TR-U-ABORT?
	case Abort#'Abort'.reason of
		{'p-abortCause', Cause} ->  % No
			TrParms = #'TR-P-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, pAbort = Cause},
			%% TR-P-ABORT indication CSL <- TSL
			gen_fsm:send_event(DHA, {'TR', 'P-ABORT', indication, TrParms});
		{'u-abortCause', Cause} ->  % Yes
			UserData = #'TR-user-data'{dialoguePortion = Cause},
			TrParms = #'TR-U-ABORT'{qos = {SequenceControl, ReturnOption},
					transactionID = TID, userData = UserData},
			%% TR-U-ABORT indication CSL <- TSL
			gen_fsm:send_event(DHA, {'TR', 'U-ABORT', indication, TrParms})
	end,
	{stop, {shutdown, TID}, State};

%% Abort from TR-User
active({'ABORT', transaction,
		#'TR-U-ABORT'{userData = UserData} = AbortParms},
		#state{localTID = TID, tco = TCO} = State) 
		when is_record(AbortParms, 'TR-U-ABORT') ->
	Cause = UserData#'TR-user-data'.dialoguePortion,
	Abort = #'Abort'{reason = {'u-abortCause', Cause}},
	%% Assemble TR-portion of ABORT message
	{ok, TPDU} = 'TR':encode('TCMessage', {abort, Abort}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(AbortParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	gen_server:cast(TCO, {'N', 'UNITDATA', request, SccpParms}),
	{stop, {shutdown, TID}, State}.

%% handle an event sent using gen_fsm:send_all_state_event/2
handle_event(_Event, StateName, State) -> 
	{stop, StateName, State}.

%% handle an event sent using gen_fsm:sync_send_all_state_event/2,3
handle_sync_event(_Event, _From, StateName, State) ->
	{stop, StateName, State}.

%% handle any other message
handle_info(Info, _StateName, State) ->
	{stop, Info, State}.

%% handle a shutdown request
terminate(_Reason, _StateName, _State) ->
	ok.

%% handle updating state data due to a code replacement
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

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

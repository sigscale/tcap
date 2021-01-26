%%% tcap_dha_fsm.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2004-2005 Motivity Telecom,
%%% 		2010-2012 Harald Welte,
%%% 		2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
%%% Copyright (c) 2010-2012, Harald Welte <laforge@gnumonks.org>
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
%%% 	module implements a Dialogue Handler (DHA) functional block within
%%% 	the component sub-layer of ITU TCAP.
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_dha_fsm).
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
-export([idle/3, initiation_sent/3, active/3,
		wait_for_uni_components/3, wait_for_begin_components/3,
		initiation_received/3, wait_cont_components_ir/3,
		wait_cont_components_active/3, wait_for_end_components/3]).

-type state() :: idle | initiation_sent | initiation_received | active.

%% record definitions for TR-User primitives
-include("tcap.hrl").
%% record definitions for N-User primitives
-include("sccp_primitive.hrl").
%% record definitions for TCAP messages
%-include("TCAPMessages.hrl").
-include("UnidialoguePDUs.hrl").
-include("DialoguePDUs.hrl").

%% the dialogue_fsm state data
-record(statedata,
		{sup :: pid(),
		usap :: pid(),
		tco :: pid(),
		cco :: pid(),
		did :: 0..4294967295,
		parms :: #'TR-UNI'{} | #'TR-BEGIN'{} | #'TR-CONTINUE'{}
				| #'TR-END'{} | #'TR-U-ABORT'{},
		appContextMode :: tuple()}).
-type statedata() :: #statedata{}.

%%----------------------------------------------------------------------
%%  The tcap_dha_fsm gen_statem call backs
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions, state_enter].

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
%% 	Initialize a Dialogue Handler (DHA) process.
%%
%% 	Reference: Figure A.5/Q.774 (sheet 1 of 11)
%%
%% @see //stdlib/gen_statem:init/1
%% @private
init([Sup, TCO, TCU]) ->
	process_flag(trap_exit, true),
	{ok, idle, #statedata{sup = Sup, tco = TCO, usap = TCU}}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @private
%
% reference: Figure A.5/Q.774 (sheet 1 of 11)
% TC-UNI request from TCU
idle(enter, idle, #statedata{sup = Sup1} = Data) ->
	Children1 = supervisor:which_children(Sup1),
	{_, Sup2, _, _} = lists:keyfind(tcap_components_sup, 1, Children1),
	Children2 = supervisor:which_children(Sup2),
	{_, CCO, _, _} = lists:keyfind(tcap_cco_server, 1, Children2),
	{keep_state, Data#statedata{cco = CCO}};
idle(cast, {'TC', 'UNI', request,
		#'TC-UNI'{qos = QoS,
		destAddress = DestAddress, origAddress = OrigAddress,
		userInfo = UserInfo, appContextName = AppContextName} = UniParms},
		#statedata{cco = CCO} = Data) 
		when is_record(UniParms, 'TC-UNI') ->
	%% Dialogue info included?
	DialoguePortion = case UserInfo of
		undefined ->
			undefined;
		UserInfo when is_binary(UserInfo) ->
			%% Build AUDT apdu
			'UnidialoguePDUs':encode('AUDT-apdu',
					#'AUDT-apdu'{'application-context-name' = AppContextName,
					'user-information' = UserInfo})
	end,
	TrUserData = #'TR-user-data'{dialoguePortion = dialogue_ext(DialoguePortion)},
	TrParms = #'TR-UNI'{qos = QoS,
			destAddress = DestAddress, origAddress = OrigAddress,
			userData = TrUserData},
	NewData = Data#statedata{parms = TrParms},
	%% Request components to CHA
	gen_server:cast(CCO, 'request-components'),
	%% Process components
	{next_state, wait_for_uni_components, NewData};
% reference: Figure A.5/Q.774 (sheet 1 of 11)
% TC-BEGIN request from TCU
idle(cast, {'TC', 'BEGIN', request,
		#'TC-BEGIN'{dialogueID = DID,
		qos = QoS, appContextName = AppContextName,
		destAddress = DestAddress, origAddress = OrigAddress,
		userInfo = UserInfo} = _BeginParms},
		#statedata{cco = CCO} = Data) -> 
	%% Dialogue info included?
	DialoguePortion = case AppContextName of
		undefined ->
			undefined;
		AC ->
			%% Set protocol version = 1
			%% Build AARQ apdu
			{ok, DP} = 'DialoguePDUs':encode('AARQ-apdu',
					#'AARQ-apdu'{'protocol-version' = [version1],
					'application-context-name' = AC,
					'user-information' = UserInfo}),
			DP	
	end,
	TrUserData = #'TR-user-data'{dialoguePortion = dialogue_ext(DialoguePortion)},
	TrParms = #'TR-BEGIN'{qos = QoS,
			destAddress = DestAddress, origAddress = OrigAddress,
			transactionID = DID, userData = TrUserData},
	NewData = Data#statedata{parms = TrParms, did = DID,
			%% Set application context mode
			appContextMode = AppContextName},
	%% Request components to CHA
	gen_server:cast(CCO, 'request-components'),
	%% Process components
	{next_state, wait_for_begin_components, NewData};
% reference: Figure A.5/Q.774 (sheet 2 of 11)
% TR-UNI indication from TSL
idle(cast, {'TR', 'UNI', indication,
		#'TR-UNI'{qos = QoS, destAddress = DestAddress,
		origAddress = OrigAddress, userData = UserData} = UniParms},
		#statedata{usap = USAP, cco = CCO} = Data) ->
	%% Extract dialogue portion
	case extract_uni_dialogue_portion(UserData) of
		incorrect_dialogue_portion ->   %% Dialogue portion correct? (no)
			%% Discard components
			{stop, normal, Data};
		no_version1 ->                  %% Is version 1 supported? (no)
			%% Discard components
			{stop, normal, Data};
		#'TC-UNI'{} = TcParms ->
			{ComponentsPresent, Components} = get_components(UserData),
			%% Assign dialogue ID
			DID = tcap_tco_server:new_tid(),
			NewTcParms = TcParms#'TC-UNI'{qos = QoS,
					destAddress = DestAddress, origAddress = OrigAddress,
					dialogueID = DID, componentsPresent = ComponentsPresent},
			NewData = Data#statedata{did = DID, parms = UniParms},
			%% Components to CHA
			case ComponentsPresent of
				true ->
					gen_server:cast(CCO, {components, Components});
				false ->
					ok       % should never happen
			end,
			%% TC-UNI indication to TCU
			gen_statem:cast(USAP, {'TC', 'UNI', indication, NewTcParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewData}
	end;
% reference: Figure A.5/Q.774 (sheet 3 of 11)
% TR-BEGIN indication from TSL
idle(cast, {'TR', 'BEGIN', indication,
		#'TR-BEGIN'{transactionID = DID, qos = QoS,
		destAddress = DestAddress, origAddress = OrigAddress,
		userData = UserData} = BeginParms},
		#statedata{usap = USAP, tco = TCO, cco = CCO} = Data) ->
	%% Extract dialogue portion
	case extract_begin_dialogue_portion(UserData) of
		incorrect_dialogue_portion ->    %% Dialogue portion correct? (no)
			%% Build ABORT apdu
			ABRT = 'DialoguePDUs':encode('ABRT-apdu',
					#'ABRT-apdu'{'abort-source' = 'dialogue-service-provider'}),
			%% Discard components
			%% TR-U-ABORT request to TSL
			TrUserData = #'TR-user-data'{dialoguePortion = dialogue_ext(ABRT)},
			TrParms = BeginParms#'TR-BEGIN'{userData = TrUserData},
			NewData = Data#statedata{did = DID, parms = TrParms},
			gen_server:cast(TCO, {'TR', 'U-ABORT', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			{stop, normal, NewData};
		no_version1 ->                  %% Is version 1 supported? (no)
			DP = UserData#'TR-user-data'.dialoguePortion,
			%% Build AARE apdu
			AARE = 'DialoguePDUs':encode('AARE-apdu',
					#'AARE-apdu'{'protocol-version' = [version1],
					'application-context-name' = DP#'AARQ-apdu'.'application-context-name',
					result = 'reject-permanent',
					'result-source-diagnostic' = {'dialogue-service-provider', 'no-common-dialogue-portion'}}),
			%% Discard components
			%% TR-P-ABORT request to TSL
			TrParms = #'TR-P-ABORT'{transactionID = DID, pAbort = AARE},
			NewData = Data#statedata{did = DID,
					appContextMode = DP#'AARQ-apdu'.'application-context-name',
					parms = TrParms},
			gen_server:cast(TCO, {'TR', 'P-ABORT', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			{stop, normal, NewData};
		#'TC-BEGIN'{} = TcParms ->
			{ComponentsPresent, Components} = get_components(UserData),
			%% Assign dialogue ID
			NewTcParms = TcParms#'TC-BEGIN'{qos = QoS,
					destAddress = DestAddress, origAddress = OrigAddress,
					dialogueID = DID, componentsPresent = ComponentsPresent},
			NewData = Data#statedata{did = DID,
					parms = BeginParms, appContextMode = TcParms#'TC-BEGIN'.appContextName},
			%% TC-BEGIN indication to TCU
			gen_statem:cast(USAP, {'TC', 'BEGIN', indication, NewTcParms}),
			%% Any components?
			case ComponentsPresent of
				true ->
					%% Components to CHA
					gen_server:cast(CCO, {components, Components});
				false ->
					ok
			end,
			{next_state, initiation_received, NewData}
	end;
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
% reference: Figure A.5/Q.774 (sheet 5 of 11)
% TC-CONTINUE request from TCU
initiation_received(enter, _, _Data) ->
	keep_state_and_data;
initiation_received(cast, {'TC', 'CONTINUE', request,
		#'TC-CONTINUE'{qos = QoS, origAddress = OrigAddress,
		appContextName = AC, userInfo = UserInfo} = _ContParms},
		#statedata{did = DID, cco = CCO} = Data) ->
	%% Dialogue info included?
	AARE = #'AARE-apdu'{'protocol-version' = [version1],
			'application-context-name' = AC,
			result = accepted,
			'result-source-diagnostic' = {'dialogue-service-user', null},
			'user-information' = UserInfo},
	{ok, DP} = 'DialoguePDUs':encode('AARE-apdu', AARE),
	TrParms = #'TR-CONTINUE'{qos = QoS,
				origAddress = OrigAddress, transactionID = DID,
				userData = #'TR-user-data'{dialoguePortion = dialogue_ext(DP)}},
	NewData = Data#statedata{parms = TrParms},
	%% Request components to CHA
	gen_server:cast(CCO, 'request-components'),
	{next_state, wait_cont_components_ir, NewData};
% reference: Figure A.5/Q.774 (sheet 5 of 11)
% TC-END request from TCU
initiation_received(cast, {'TC', 'END', request,
		#'TC-END'{qos = QoS, appContextName = AC, userInfo = UserInfo,
		termination = Termination} = EndParms},
		#statedata{did = DID, tco = TCO, cco = CCO} = Data) ->
	%% Prearranged end?
	case Termination of
		prearranged ->
			%% TR-END request to TSL
			TrParms = #'TR-END'{qos = QoS,
					transactionID = DID, termination = Termination},
			NewData = Data#statedata{parms = TrParms},
			gen_server:cast(TCO, {'TR', 'END', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewData};
		basic ->
			AARE = #'AARE-apdu'{'protocol-version' = [version1],
					'application-context-name' = AC, result = accepted,
					'result-source-diagnostic' = {'dialogue-service-user', null},
					'user-information' = UserInfo},
			{ok, DP} = 'DialoguePDUs':encode('AARE-apdu', AARE),
			TrParms = #'TR-END'{qos = QoS, transactionID = DID,
					termination = EndParms#'TC-END'.termination,
					userData = #'TR-user-data'{dialoguePortion = dialogue_ext(DP)}},
			NewData = Data#statedata{parms = TrParms},
			%% Request components to CHA
			gen_server:cast(CCO, 'request-components'),
			%% Process components
			{next_state, wait_for_end_components, NewData}
	end;
% reference: Figure A.5/Q.774 (sheet 6 of 11)
% TC-U-ABORT request from TCU
initiation_received(cast, {'TC', 'U-ABORT', request,
		#'TC-U-ABORT'{abortReason = AbortReason, qos = QoS,
		appContextName = AC, userInfo = UserInfo} = _AbortParms},
		#statedata{did = DID, tco = TCO, cco = CCO,
		appContextMode = AppContextMode} = Data)
		when AbortReason  == applicationContextNotSupported;
		AbortReason == dialogueRefused; AbortReason == userSpecified ->
	UserData = case AppContextMode of
		%% Is application context mode set? (no)
		undefined ->
			#'TR-user-data'{};
		%% Abort reason present and = AC-name not supported OR dialogue refused?
		_ when AbortReason == applicationContextNotSupported ->
			%% Set protocol version = 1
			%% Build AARE-pdu (rejected)
			AARE = 'DialoguePDUs':encode('AARE-apdu',
					#'AARE-apdu'{'protocol-version' = [version1],
					'application-context-name' = AC,
					result = 'reject-permanent',
					'result-source-diagnostic' = {'dialogue-service-user',
					'application-context-name-not-supported'}}),
			#'TR-user-data'{dialoguePortion = dialogue_ext(AARE),
						   componentPortion = asn1_NOVALUE};
		_ when AbortReason == dialogueRefused ->
			%% Set protocol version = 1
			%% Build AARE-pdu (rejected)
			AARE = 'DialoguePDUs':encode('AARE-apdu',
					#'AARE-apdu'{'protocol-version' = [version1],
					'application-context-name' = AC,
					result = 'reject-permanent',
					'result-source-diagnostic' = {'dialogue-service-user', null}}),
			#'TR-user-data'{dialoguePortion = dialogue_ext(AARE)};
		_ when AbortReason == userSpecified ->
			%% Build ABRT-apdu (abort source = dialogue-service-user)
			ABRT = 'DialoguePDUs':encode('ABRT-apdu',
					#'ABRT-apdu'{'abort-source' = 'dialogue-service-user',
					'user-information' = UserInfo}),
			#'TR-user-data'{dialoguePortion = dialogue_ext(ABRT),
						   componentPortion = asn1_NOVALUE}
	end,
	%% TR-U-ABORT request to TSL
	TrParms = #'TR-U-ABORT'{qos = QoS,
			transactionID = DID, userData = UserData},
	NewData = Data#statedata{parms = TrParms},
	gen_server:cast(TCO, {'TR', 'U-ABORT', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewData};
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
% reference: Figure A.5/Q.774 (sheet 7 of 11)
%% TC-END request from TCU
initiation_sent(enter, _, _Data) ->
	keep_state_and_data;
initiation_sent(cast, {'TC', 'END', request,
		#'TC-END'{termination = prearranged, qos = QoS} = _EndParms},
		#statedata{did = DID, tco = TCO, cco = CCO} = Data) ->
	% termination must be prearranged
	%% TR-END request to TSL
	TrParms = #'TR-END'{qos = QoS,
			transactionID = DID, termination = prearranged},
	NewData = Data#statedata{parms = TrParms},
	gen_server:cast(TCO, {'TR', 'END', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	{stop, normal, NewData};
% reference: Figure A.5/Q.774 (sheet 7 of 11)
%% TC-U-ABORT request from TCU (local action)
initiation_sent(cast, {'TC', 'U-ABORT', request,
		#'TC-U-ABORT'{qos = QoS} = _AbortParms},
		#statedata{did = DID, tco = TCO, cco = CCO} = Data) ->
	%% TR-U-ABORT request to TSL
	TrParms = #'TR-U-ABORT'{qos = QoS, transactionID = DID},
	NewData = Data#statedata{parms = TrParms},
	gen_server:cast(TCO, {'TR', 'U-ABORT', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	{stop, normal, NewData};
% reference: Figure A.5/Q.774 (sheet 7 of 11)
%%% TR-END indication from TSL
initiation_sent(cast, {'TR', 'END', indication, _EndParms} = P, Data) ->
	is_or_active(initiation_sent, P, Data);
%% reference: Figure A.5/Q.774 (sheet 7 of 11)
%% NOTE:  currently the TCO short circuits this function and sends directly to TCU
initiation_sent(cast, {'TR', 'NOTICE', indication,
		#'TR-NOTICE'{origAddress = OrigAddress, destAddress = DestAddress,
		reportCause = ReportCause} = 	NoticeParms},
		#statedata{usap = USAP, did = DID} = Data) ->
	%% TC-NOTICE indication to TCU
	TcParms = #'TC-NOTICE'{dialogueID = DID,
			origAddress = OrigAddress, destAddress = DestAddress,
			reportCause = ReportCause},
	NewData = Data#statedata{parms = NoticeParms},
	gen_statem:cast(USAP, {'TC', 'NOTICE', indication, TcParms}),
	{next_state, initiation_sent, NewData};
%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% TR-CONTINUE indication from TSL
initiation_sent(cast, {'TR', 'CONTINUE', indication, _ContParms} = P, Data) ->
	is_or_active(initiation_sent, P, Data);
%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% TR-U-ABORT indication from TSL
initiation_sent(cast, {'TR', 'U-ABORT', indication, _} = P, Data) ->
	handle_abort(P, Data);
initiation_sent(cast, {'TR', 'P-ABORT', indication, _} = P, Data) ->
	handle_abort(P, Data);
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
%% reference: Figure A.5/Q.774 (sheet 9 of 11)
%% TC-CONTINUE request from TCU
active(enter, _, _Data) ->
	keep_state_and_data;
active(cast, {'TC', 'CONTINUE', request,
		#'TC-CONTINUE'{dialogueID = DID, qos = QoS,
		origAddress = OrigAddress, userInfo = UserInfo} = _ContParms},
		#statedata{cco = CCO, did = DID} = Data) ->
	TrParms = #'TR-CONTINUE'{qos = QoS,
				origAddress = OrigAddress, transactionID = DID,
				userData = #'TR-user-data'{dialoguePortion = UserInfo}},
	NewData = Data#statedata{parms = TrParms},
	%% Request component to CHA
	gen_server:cast(CCO, 'request-components'),
	%% Process components
	{next_state, wait_cont_components_active, NewData};
%% reference: Figure A.5/Q.774 (sheet 9 of 11)
%% TC-END request from TCU
active(cast, {'TC', 'END', request,
		#'TC-END'{qos = QoS, userInfo = UserInfo,
		termination = Termination} = _EndParms},
		#statedata{tco = TCO, cco = CCO, did = DID} = Data) ->
	TrParms = #'TR-END'{qos = QoS, transactionID = DID,
			userData = #'TR-user-data'{dialoguePortion = UserInfo},
			termination = Termination},
	NewData = Data#statedata{parms = TrParms},
	%% Prearranged end?
	case Termination of
		prearranged ->
			%% TR-END request to TSL
			gen_server:cast(TCO, {'TR', 'END', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewData};
		basic ->
			%% Request component to CHA
			gen_server:cast(CCO, 'request-components'),
			%% Process components
			{next_state, wait_for_end_components, NewData}
	end;
%% reference: Figuer A.5/Q774 (sheet 10 of 11)
%% TR-END indication from TSL
active(cast, {'TR', 'END', indication, _EndParms} = P, Data) ->
	is_or_active(active, P, Data);
%% reference: Figure A.5/Q.774 (sheet 11 of 11)
%% TR-CONTINUE indication from TSL
active(cast, {'TR', 'CONTINUE', indication, _ContParms} = P, Data) ->
	is_or_active(active, P, Data);
active(cast, {'TR', 'U-ABORT', indication, _} = P, Data) ->
	handle_abort(P, Data);
active(cast, {'TR', 'P-ABORT', indication, _} = P, Data) ->
	handle_abort(P, Data);
active(info, _, _Data) ->
	keep_state_and_data.

-spec wait_for_uni_components(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_for_uni_components</em> state.
%% @private
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 2 of 11)
wait_for_uni_components(enter, _, _Data) ->
	keep_state_and_data;
wait_for_uni_components(cast, {'requested-components', []}, Data) ->
	wait_for_uni_components1(Data);
wait_for_uni_components(cast, {'requested-components', Components},
		#statedata{parms = #'TR-UNI'{userData = UserData} = TrParms} = Data) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	NewTrParms = TrParms#'TR-UNI'{userData = NewUserData},
	wait_for_uni_components1(Data#statedata{parms = NewTrParms});
wait_for_uni_components(info, _, _Data) ->
	keep_state_and_data.
%% @hidden
wait_for_uni_components1(#statedata{tco = TCO,
		cco = CCO, parms = TrParms} = Data) ->
	%% TR-UNI request to TSL
	gen_server:cast(TCO, {'TR', 'UNI', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, Data}.
	
-spec wait_for_begin_components(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_for_begin_components</em> state.
%% @private
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 2 of 11)
wait_for_begin_components(enter, _, _Data) ->
	keep_state_and_data;
wait_for_begin_components(cast, {'requested-components', []}, Data) ->
	wait_for_begin_components1(Data);
wait_for_begin_components(cast, {'requested-components', Components},
		#statedata{parms = #'TR-BEGIN'{userData = UserData} = TrParms} = Data) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	NewTrParms = TrParms#'TR-BEGIN'{userData = NewUserData},
	wait_for_begin_components1(Data#statedata{parms = NewTrParms});
wait_for_begin_components(info, _, _Data) ->
	keep_state_and_data.
%% @hidden
wait_for_begin_components1(#statedata{tco = TCO, parms = TrParms} = Data) ->
	%% We don't Assign local transaction ID, as we simply re-use the DialougeID!
	%% TR-BEGIN request to TSL
	case gen_server:call(TCO, {'TR', 'BEGIN', request, TrParms}) of
		ok ->
			{next_state, initiation_sent, Data};
		{error, Reason} ->
			{stop, Reason, Data}
	end.
	
-spec wait_cont_components_ir(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_cont_components_ir</em> state.
%% @private
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 5 of 11)
wait_cont_components_ir(enter, _, _Data) ->
	keep_state_and_data;
wait_cont_components_ir(cast, {'requested-components', []}, Data) ->
	wait_cont_components_ir1(Data);
wait_cont_components_ir(cast, {'requested-components', Components},
		#statedata{parms = #'TR-CONTINUE'{userData = UserData} = TrParms} = Data) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	NewTrParms = TrParms#'TR-CONTINUE'{userData = NewUserData},
	wait_cont_components_ir1(Data#statedata{parms = NewTrParms});
wait_cont_components_ir(info, _, _Data) ->
	keep_state_and_data.
%% @hidden
wait_cont_components_ir1(#statedata{tco = TCO, parms = TrParms} = Data) ->
	%% TR-CONTINUE request to TSL
	gen_server:cast(TCO, {'TR', 'CONTINUE', request, TrParms}),
	{next_state, active, Data}.
	
-spec wait_cont_components_active(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_cont_components_active</em> state.
%% @private
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 9 of 11)
wait_cont_components_active(enter, _, _Data) ->
	keep_state_and_data;
wait_cont_components_active(cast, {'requested-components', []}, Data) ->
	wait_cont_components_active1(Data);
wait_cont_components_active(cast, {'requested-components', Components},
		#statedata{parms = #'TR-CONTINUE'{userData = UserData} = TrParms} = Data) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	NewTrParms = TrParms#'TR-CONTINUE'{userData = NewUserData},
	wait_cont_components_active1(Data#statedata{parms = NewTrParms});
wait_cont_components_active(info, _, _Data) ->
	keep_state_and_data.
%% @hidden
wait_cont_components_active1(#statedata{tco = TCO, parms = TrParms} = Data) ->
	%% TR-CONTINUE request to TSL
	gen_server:cast(TCO, {'TR', 'CONTINUE', request, TrParms}),
	{next_state, active, Data}.
	
-spec wait_for_end_components(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_for_end_components</em> state.
%% @private
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 5 of 11)
%% reference: Figure A.5/Q.774 (sheet 9 of 11)
wait_for_end_components(enter, _, _Data) ->
	keep_state_and_data;
wait_for_end_components(cast, {'requested-components', []}, Data) ->
	wait_for_end_components1(Data);
wait_for_end_components(cast, {'requested-components', Components},
		#statedata{parms = #'TR-END'{userData = UserData} = TrParms} = Data) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	NewTrParms = TrParms#'TR-END'{userData = NewUserData},
	wait_for_end_components1(Data#statedata{parms = NewTrParms});
wait_for_end_components(info, _, _Data) ->
	keep_state_and_data.
%% @hidden
wait_for_end_components1(#statedata{tco = TCO,
		cco = CCO, parms = TrParms} = Data) ->
	%% TR-END request to TSL
	gen_server:cast(TCO, {'TR', 'END', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, Data}.

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
		Result :: {ok, NewData, NewData} |  Reason,
		NewData :: state(),
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

%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% reference: Figure A.5/Q.774 (sheet 11 of 11)
%% TR-CONTINUE indication from TSL
is_or_active(StateName, {'TR', 'CONTINUE', indication,
		#'TR-CONTINUE'{transactionID = TID, origAddress = OrigAddress,
		userData = UserData, qos = QoS} = ContParms},
		#statedata{appContextMode = AppContextMode,
		did = DID, usap = USAP, tco = TCO, cco = CCO} = Data) ->
	{ComponentsPresent, Components} = get_components(UserData),
	%% Dialogue portion included?
	%% AC Mode set?
	%% Extract dialogue portion
	%% Dialogue portion correct?
	case extract_dialogue_portion(UserData, AppContextMode, StateName) of
		abort ->
			%% Discard components
			%% TC-P-ABORT indication to TCU
			TcAParms = #'TC-P-ABORT'{qos = QoS,
					dialogueID = DID, pAbort = abnormalDialogue},
			gen_statem:cast(USAP, {'TC', 'P-ABORT', indication, TcAParms}),
			%% Build ABRT apdu
			ABRT = 'DialoguePDUs':encode('ABRT-apdu',
					#'ABRT-apdu'{'abort-source' = 'dialogue-service-provider'}),
			UserData = #'TR-user-data'{dialoguePortion = dialogue_ext(ABRT),
						   componentPortion = asn1_NOVALUE},
			%% TR-U-ABORT request to TSL
			TrParms = #'TR-U-ABORT'{qos = QoS,
						transactionID = TID, userData = UserData},
			NewData = Data#statedata{parms = ContParms},
			gen_server:cast(TCO, {'TR', 'U-ABORT', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewData};
		AARE ->
			%% TC-CONTINUE indication to TCU
			TcParms = #'TC-CONTINUE'{qos = QoS, origAddress = OrigAddress,
						appContextName = AppContextMode, dialogueID = DID,
      				userInfo = AARE, componentsPresent = ComponentsPresent},
			NewData = Data#statedata{parms = ContParms},
			gen_statem:cast(USAP, {'TC', 'CONTINUE', indication, TcParms}),
			%% Any components?
			case ComponentsPresent of
				true ->
					%% Components to CHA
					gen_server:cast(CCO, {components, Components}),
					{next_state, active, NewData};
				false ->
					{next_state, active, NewData}
			end
	end;
%% reference: Figure A.5/Q.774 (sheet 7 of 11)
%%% TR-END indication from TSL
is_or_active(StateName, {'TR', 'END', indication,
		#'TR-END'{qos = QoS, userData = UserData,
		termination = Termination} = EndParms},
		#statedata{appContextMode = AppContextMode,
		did = DID, usap = USAP, cco = CCO} = Data) ->
	{ComponentsPresent, Components} = get_components(UserData),
	%% Dialogue portion included?
	%% AC Mode set?
	%% Extract dialogue portion
	%% Dialogue portion correct?
	case extract_dialogue_portion(UserData, AppContextMode, StateName) of
		abort ->
			%% Discard components
			%% TC-P-ABORT indication to TCU
			TcParms = #'TC-P-ABORT'{qos = QoS,
					dialogueID = DID, pAbort = abnormalDialogue},
			NewData = Data#statedata{parms = EndParms},
			gen_statem:cast(USAP, {'TC', 'P-ABORT', indication, TcParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewData};
		AARE ->
			%% TC-END indication to TCU
			TcParms = #'TC-END'{qos = QoS, dialogueID = DID,
					appContextName = AppContextMode,
					componentsPresent = ComponentsPresent,
					userInfo = AARE, termination = Termination},
			NewData = Data#statedata{parms = EndParms},
			gen_statem:cast(USAP, {'TC', 'END', indication, TcParms}),
			%% Any components?
			case ComponentsPresent of
				true ->
					%% Components to CHA
					gen_server:cast(CCO, {components, Components});
				false ->
					ok
			end,
			%% Dialogue terminated to CHA
			gen_server:cast(CCO, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewData}
	end.

%% TR-U-ABORT indication from TSL
%% @private
handle_abort({'TR', 'U-ABORT', indication,
		#'TR-U-ABORT'{qos = QoS, userData = UserData} = AbortParms},
		#statedata{usap = USAP, cco = CCO, did = DID,
		appContextMode = undefined} = Data)
		when (not is_record(UserData, 'TR-user-data')
		or UserData#'TR-user-data'.dialoguePortion == asn1_NOVALUE) ->
	%% Is AC mode set? (no) Is Dialogue portion present? (no)
	TcParms = #'TC-U-ABORT'{qos = QoS, dialogueID = DID},
	NewData = Data#statedata{parms = AbortParms},
	gen_statem:cast(USAP, {'TC', 'U-ABORT', indication, TcParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewData};
handle_abort({'TR', 'U-ABORT', indication,
		#'TR-U-ABORT'{qos = QoS,
		userData = #'TR-user-data'{dialoguePortion = DP}} = AbortParms},
		#statedata{usap = USAP, cco = CCO, did = DID,
		appContextMode = undefined} = Data) when DP /= asn1_NOVALUE ->
	%% Is AC mode set? (no) Is Dialogue portion present? (yes)
	TcParms = #'TC-P-ABORT'{qos = QoS, dialogueID = DID,
			pAbort = abnormalDialogue},
	NewData = Data#statedata{parms = AbortParms},
	gen_statem:cast(USAP, {'TC', 'P-ABORT', indication, TcParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewData};
handle_abort({'TR', 'U-ABORT', indication,
		#'TR-U-ABORT'{qos = QoS, userData = UserData} = AbortParms},
		#statedata{usap = USAP, cco = CCO, did = DID} = Data)
		when not is_record(UserData, 'TR-user-data') ->
	%% Is User Data included in primitive? (no)
	TcParms = #'TC-P-ABORT'{qos = QoS, dialogueID = DID,
			pAbort = abnormalDialogue},
	NewData = Data#statedata{parms = AbortParms},
	gen_statem:cast(USAP, {'TC', 'P-ABORT', indication, TcParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewData};
handle_abort({'TR', 'U-ABORT', indication,
		#'TR-U-ABORT'{qos = QoS,
		userData = #'TR-user-data'{dialoguePortion = DP}} = AbortParms},
		#statedata{usap = USAP, cco = CCO, did = DID,
		appContextMode = AppContextMode} = Data) ->
		%% Is PDU type = ABRT or AARE (rejected)?
	TcParms = case 'DialoguePDUs':decode('DialoguePDU', DP) of
		{dialoguePDU, #'AARE-apdu'{'application-context-name' = AC}}
				when AC /= AppContextMode ->
			%% Is abstract syntax = dialogue-PDU AS? (no)
			#'TC-P-ABORT'{qos = QoS, dialogueID = DID, pAbort = abnormalDialogue};
		{dialoguePDU, #'ABRT-apdu'{'abort-source' = AbortSource,
				'user-information' = UserInfo}}
				when element(1, AbortSource) == 'dialogue-service-user' ->
			%% Is Abort source = user? (yes)
			#'TC-U-ABORT'{qos = QoS, dialogueID = DID,
					abortReason = userSpecific, userInfo = UserInfo};
		{dialoguePDU, #'AARE-apdu'{result = 'reject-permanent',
				'result-source-diagnostic' = {'dialogue-service-user',
				'application-context-name-not-supported'},
				'application-context-name' = AC,
				'user-information' = UserInfo}} ->
			%% Is Associate source = user? (yes)
			#'TC-U-ABORT'{qos = QoS, dialogueID = DID,
					abortReason = applicationContextNotSupported,
					appContextName = AC, userInfo = UserInfo};
		{dialoguePDU, #'AARE-apdu'{result = 'reject-permanent',
				'result-source-diagnostic' = RSD,
				'application-context-name' = AC},
				'user-information' = UserInfo} when
				element(1, RSD) == 'dialogue-service-user' ->
			#'TC-U-ABORT'{qos = QoS, dialogueID = DID,
					abortReason = dialogueRefused,
					appContextName = AC, userInfo = UserInfo};
		{dialoguePDU,
				#'AARE-apdu'{'result-source-diagnostic'
				= {'dialogue-service-provider', 'no-common-dialogue-portion'}}} ->
			%% Is AARE (no common dialogue portion)?
			#'TC-P-ABORT'{qos = QoS, dialogueID = DID,
					pAbort = noCommonDialoguePortion};
		_ ->
			#'TC-P-ABORT'{qos = QoS, dialogueID = DID,
					pAbort = abnormalDialogue}
	end,
	case TcParms of
		#'TC-U-ABORT'{} ->
			gen_statem:cast(USAP, {'TC', 'U-ABORT', indication, TcParms});
		#'TC-P-ABORT'{} ->
			gen_statem:cast(USAP, {'TC', 'P-ABORT', indication, TcParms})
	end,
	NewData = Data#statedata{parms = AbortParms},
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewData};
%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% TR-P-ABORT indication from TSL
handle_abort({'TR', 'P-ABORT', indication,
		#'TR-P-ABORT'{qos = QoS, pAbort = PAbort} = AbortParms},
		#statedata{usap = USAP, cco = CCO, did = DID} = Data) ->
	TcParms = #'TC-P-ABORT'{qos = QoS, dialogueID = DID, pAbort = PAbort},
	NewData = Data#statedata{parms = AbortParms},
	%% TC-P-ABORT indication to TCU
	gen_statem:cast(USAP, {'TC', 'P-ABORT', indication, TcParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(CCO, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewData}.

-spec extract_uni_dialogue_portion(UserData) -> Result
	when
		UserData :: #'TR-user-data'{} | undefined,
		Result :: #'TC-UNI'{} | no_version1 | incorrect_dialogue_portion.
%% @hidden
%% Dialogue portion included? (yes)
extract_uni_dialogue_portion(#'TR-user-data'{dialoguePortion = DP})
		when DP /= asn1_NOVALUE ->
	%% Dialogue portion correct?
	case 'UnidialoguePDUs':decode('UnidialoguePDU', DP) of
		{unidialoguePDU, #'AUDT-apdu'{'protocol-version' = Version,
				'application-context-name' = AC,
				'user-information' = UserInfo}} ->
			%% Is version 1 supported?
			case lists:member(version1, Version) of
				true ->
					#'TC-UNI'{appContextName = AC, userInfo = UserInfo};
				false ->
					no_version1
			end;
		_ ->
			incorrect_dialogue_portion
	end;
%% Dialogue portion included? (no)
extract_uni_dialogue_portion(_DialoguePortion) ->
	#'TC-UNI'{}.
	
%% @hidden
%% Dialogue portion included? (yes)
extract_begin_dialogue_portion(#'TR-user-data'{dialoguePortion = DP})
		when DP /= asn1_NOVALUE ->
	%% Extract dialogue portion
	%{'EXTERNAL', {syntax,{0,0,17,773,1,1,1}}, _, PDU} = DP,
	% some implementations seem to be broken and not send the 'syntax' part?!?
	{'EXTERNAL', _, _, PDU} = DP,
	case 'DialoguePDUs':decode('DialoguePDU', PDU) of
		{ok, {dialogueRequest, #'AARQ-apdu'{'protocol-version' = Version,
				'application-context-name' = AC,
				'user-information' = UserInfo}}} ->
			%% Is version 1 supported?
			case lists:member(version1, Version) of
				true ->
					%% Set application context mode
					#'TC-BEGIN'{appContextName = AC, userInfo = UserInfo};
				false ->
					no_version1
			end;
		_ ->
			incorrect_dialogue_portion
	end;
%% Dialogue portion included? (no)
extract_begin_dialogue_portion(_DialoguePortion) ->
	#'TC-BEGIN'{}.

-spec extract_dialogue_portion(UserData, AppContextName, StateName) -> Result
	when
		UserData :: #'TR-user-data'{} | undefined,
		AppContextName :: tuple() | undefined,
		StateName :: initiation_sent | active,
		Result :: #'AARE-apdu'{} | abort | undefined.
%% @hidden
% ANY: if AC is undefined and dialogue portion present -> abort
extract_dialogue_portion(#'TR-user-data'{dialoguePortion = DP},
		undefined, _) when DP /= asn1_NOVALUE ->
	%% Dialogue portion included? (yes)  AC mode set? (no)
	abort;
% IS: if dialogue portion is not present but App context name is set -> abort
extract_dialogue_portion(#'TR-user-data'{dialoguePortion = asn1_NOVALUE},
		AppContextName, initiation_sent) when AppContextName /= undefined ->
	%% Dialogue portion included? (no)  AC mode set? (yes)
	abort;
extract_dialogue_portion(UserData, AppContextName, initiation_sent)
		when not is_record(UserData, 'TR-user-data'),
		AppContextName /= undefined ->
	%% Dialogue portion included? (no)  AC mode set? (yes)
	abort;
% ACTIVE: if dialogue portion is not present but App context name is set -> empty
extract_dialogue_portion(#'TR-user-data'{dialoguePortion = asn1_NOVALUE},
		AppContextName, active) when AppContextName /= undefined ->
	%% Dialogue portion included? (no)  AC mode set? (yes)
	undefined;
extract_dialogue_portion(UserData, AppContextName, active)
		when not is_record(UserData, 'TR-user-data'),
		AppContextName /= undefined ->
	%% Dialogue portion included? (no)  AC mode set? (yes)
	undefined;
% ANY: if dialogue portion is present and AppContext name is set -> decode dialogue and proceed
extract_dialogue_portion(#'TR-user-data'{dialoguePortion = DP},
		AppContextName, _) when DP /= asn1_NOVALUE,
		AppContextName /= undefined ->
	%% Extract dialogue portion
	%{'EXTERNAL', {syntax,{0,0,17,773,1,1,1}}, _, PDU} = DP
	% some implementations seem to be broken and not send the 'syntax' part?!?
	{'EXTERNAL', _, _, PDU} = DP,
	case 'DialoguePDUs':decode('DialoguePDU', PDU) of
		{ok, {dialogueResponse, #'AARE-apdu'{} = AARE}} ->
			AARE;	%% Dialogue portion correct? (yes)
		_ ->
			abort	%% Dialogue portion correct? (no)
	end.

-spec dialogue_ext(DialoguePortion) -> Result
	when
		DialoguePortion :: binary() | undefined | asn1_NOVALUE,
		Result :: #'EXTERNAL'{} | asn1_NOVALUE.
%% @doc Wrap encoded DialoguePortion in EXTERNAL ASN.1 data type.
%% @hidden
dialogue_ext(undefined) ->
	asn1_NOVALUE;
dialogue_ext(asn1_NOVALUE) ->
	asn1_NOVALUE;
dialogue_ext(DialoguePortion) ->
	#'EXTERNAL'{'direct-reference' = {0,0,17,773,1,1,1},
		    'indirect-reference' = asn1_NOVALUE,
		    'encoding' = {'single-ASN1-type', DialoguePortion}}.

-spec get_components(UserData) -> Result
	when
		UserData :: #'TR-user-data'{} | undefined,
		Result :: {ComponentsPresent, Components},
		ComponentsPresent :: boolean(),
		Components :: list() | undefined.
%% @hidden
get_components(#'TR-user-data'{componentPortion = asn1_NOVALUE} = _UserData) ->
	{false, undefined};
get_components(#'TR-user-data'{componentPortion = CP}) ->
	case 'TC':decode('Components', CP) of
		{ok, []} ->
			{false, []};
		{ok, Components} ->
			{true, Components}
	end;
get_components(_UserData) ->
	{false, undefined}.


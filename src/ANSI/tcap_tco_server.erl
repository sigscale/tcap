%%% tcap_tco_server.erl
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
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
%%% @doc Transaction Coordinator (TCO) functional block within the
%%% 		transaction sub-layer of ANSI TCAP.
%%%
%%% @reference ANSI T1.114.4 Transaction Capabilities Procedures 
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_tco_server).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% our published API functions
-export([new_tid/0]).

-include("TCAPMessages.hrl").
-include("tcap.hrl").
-include("sccp_primitive.hrl").

-record(state,
		{sup :: pid(),
		module :: atom(),
		ext_state :: any(),
		usap :: pid()}).
-type state() :: #state{}.

-type tid() :: 0..4294967295.

-callback send_primitive(Primitive, State) -> Result
	when
		Primitive :: {'N', 'UNITDATA', request, UdataParams},
		UdataParams :: #'N-UNITDATA'{},
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
-callback start_user(CSL, DialogueID, State) -> pid()
	when
		CSL :: {DHA, CCO},
		DHA :: pid(),
		CCO :: pid(),
		DialogueID :: tid(),
		State :: state().
-callback start_dialogue(DialogueID, State) -> StartFunc
	when
		DialogueID :: tid(),
		State :: term(),
		StartFunc :: {Module, Function, Arguments},
		Module :: atom(),
		Function :: atom(),
		Arguments :: [term()].
-callback init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: state()}
				| {ok, State :: state(), Timeout :: timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term()} | ignore.
-callback handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: {pid(), Tag :: any()},
		State :: state(),
		Result :: {reply, Reply :: term(), NewState :: state()}
				| {reply, Reply :: term(), NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), Reply :: term(), NewState :: state()}
				| {stop, Reason :: term(), NewState :: state()}.
-callback handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}
				| {primitive, Primitive, NewState :: state()},
		Primitive :: {'N', 'UNITDATA', indication, #'N-UNITDATA'{}}
				| {'N', 'NOTICE', indication, #'N-NOTICE'{}}.
-callback handle_continue(Info, State) -> Result
	when
		Info :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
-callback handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}
				| {primitive, Primitive, NewState :: state()},
		Primitive :: {'N', 'UNITDATA', indication, #'N-UNITDATA'{}}
				| {'N', 'NOTICE', indication, #'N-NOTICE'{}}.
-callback terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: state().
-callback code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status
	when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().
-optional_callbacks([handle_info/2, handle_continue/2,
		terminate/2, code_change/3, format_status/2,
		start_dialogue/2]).

%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: state()}
				| {ok, State :: state(), Timeout :: timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term()} | ignore.
%% @see //stdlib/gen_server:init/1
%% @private
init([Sup, Module, Args]) when is_list(Args) ->
	process_flag(trap_exit, true),
	case Module:init(Args) of
		{ok, ExtState} ->
			NewState = #state{sup = Sup, module = Module, ext_state = ExtState},
			{ok, NewState};
		{ok, ExtState, Timeout} ->
			NewState = #state{sup = Sup, module = Module, ext_state = ExtState},
			{ok, NewState, Timeout};
		{stop, Reason} ->
			{stop, Reason};
		ignore ->
			ignore
	end.


-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: {pid(), Tag :: any()},
		State :: state(),
		Result :: {reply, Reply :: term(), NewState :: state()}
				| {reply, Reply :: term(), NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), Reply :: term(), NewState :: state()}
				| {stop, Reason :: term(), NewState :: state()}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
handle_call(dialogueID = _Request, _From, State) ->
	{reply, new_tid(), State};
handle_call(set_usap, {From, _Tag}, State) ->
	{reply, ok, State#state{usap = From}};
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};
handle_call(Request, From, State) ->
	Module = State#state.module,
	case Module:handle_call(Request, From, State#state.ext_state) of
		{reply, Reply, ExtState} ->
			{reply, Reply, State#state{ext_state = ExtState}};
		{reply, Reply, ExtState, Timeout} ->
			{reply, Reply, State#state{ext_state = ExtState}, Timeout};
		{noreply, ExtState} ->
			{noreply, State#state{ext_state = ExtState}};
		{noreply, ExtState, Timeout} ->
			{noreply, State#state{ext_state = ExtState}, Timeout};
		{stop, Reason, Reply, ExtState} ->
			{stop, Reason, Reply, State#state{ext_state = ExtState}};
		{stop, Reason, ExtState} ->
			{stop, Reason, State#state{ext_state = ExtState}}
	end.

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}
				| {primitive, Primitive, NewState :: state()},
		Primitive :: {'N', 'UNITDATA', indication, #'N-UNITDATA'{}}
				| {'N', 'NOTICE', indication, #'N-NOTICE'{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%%
%% 	A user callback module may return an SCCP service primitive
%% 	to TCO for processing with the return value 
%% 	`{primitive, Primitive, NewState}'.
%% @@see //stdlib/gen_server:handle_cast/2
%% @private
%% @end
% service primitive indications from the network layer
handle_cast({'N', 'UNITDATA', indication, UdataParms}, State) 
		when is_record(UdataParms, 'N-UNITDATA') ->
	case 'TR':decode('TCMessage', UdataParms#'N-UNITDATA'.userData) of
		{ok, {unidirectional, TPDU}} ->
			case 'TR':decode('Unidirectional', TPDU) of
				{ok, Unidirectional} ->
					%% Create a Dialogue Handler (DHA) 
					DialogueID = new_tid(),
					SupId = list_to_atom("dha_" ++ integer_to_list(DialogueID)),
					{ok, {M, F, A, Mods} = application:get_env(start_dha),
					StartFunc = {M, F, A ++ [{State#state.nsap, State#state.usap, DialogueID, self(), SupId}]},
					ChildSpec = {SupId, StartFunc, temporary, 10000, worker, Mods},
					{ok, DHA} = supervisor:start_child(State#state.sup, ChildSpec),
					%% TR-UNI indication CSL <- TSL
					UserData = #'TR-user-data'{dialoguePortion = Unidirectional#'Unidirectional'.dialoguePortion,
							componentPortion = Unidirectional#'Unidirectional'.components},
					TrParms = #'TR-UNI'{qos = {UdataParams#'N-UNITDATA'.sequenceControl,
							UdataParams#'N-UNITDATA'.returnOption },
							destAddress = UdataParms#'N-UNITDATA'.calledAddress,
							origAddress = UdataParms#'N-UNITDATA'.callingAddress,
							userData = UserData},
					gen_fsm:send_event(DHA, {'TR', 'UNI', indication, TrParms}),
					{noreply, State};
				{error, Reason} ->
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					error_logger:error_report(["Syntax error in received N-UNI",
							{nsap, State#state.nsap}, {error, Reason},
							{caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, Begin} ->
					%% Assign local transaction ID
					TID = new_tid(),
					ChildName = list_to_atom("tsm_sup_" ++ integer_to_list(TID)),
					{ok, {M, F, A, Mods} = application:get_env(start_tsm),
					StartFunc = {M, F, A ++ [{State#state.nsap, State#state.usap, TID}]},
					ChildSpec = {ChildName, StartFunc, temporary, infinity, supervisor, Mods},
					%% Is TID = no TID?
					%% Note:  The assignment of the ID above just gets the next available
					%%        value and doesn't ensure that it is not in use (unlikely)
					%%        or that there are enough resources available.  The real
					%%        test is in whether the start succeeds.
					case supervisor:start_child(State#state.sup, ChildSpec) of
						{ok, TSM} ->
							%% Created a Transaction State Machine (TSM)
							TsmParms = UdataParms#'N-UNITDATA'{userData = Begin},
							%% BEGIN received TSM <- TCO
							gen_fsm:send_event(TSM, {'BEGIN', received, TsmParms});
						{error, Reason} ->
							%% TID = no TID
							%% Build ABORT message (P-Abort Cause = Resource Limitation)
							Abort = {abort, #'Abort'{dtid = TPDU#'Begin'.otid,
									reason = {'p-abortCause', resourceLimitation}}},
							NewTPDU = 'TCMessage':encode('TCMessage', Abort),
							SccpParms = #'N-UNITDATA'{calledAddress = UdataParms#'N-UNITDATA'.callingAddress,
									callingAddress = UdataParms#'N-UNITDATA'.calledAddress,
									sequenceControl = false, returnOption = false,
									userData = NewTPDU},
							%% TR-UNI request TSL -> SCCP
							gen_fsm:send_event(State#state.nsap, {'N', 'UNITDATA', request, SccpParms}),
							error_logger:error_report(["Unable to create TSM for received N-BEGIN",
									{error, Reason}, {nsap, State#state.nsap},
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}])
					end,
					{noreply, State};
				{error, Reason} ->
%% TODO
					%% is OTID derivable?
					%%    Build ABORT message with appropraite P-Abort Cause value
					%%    N-UNITDATA request TSL -> SCCP
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
					error_logger:error_report(["Syntax error in received N-BEGIN", {error, Reason},
									{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, Continue} ->
					%% DTID assigned?
					case catch ets:lookup_element(tcap_transaction, TPDU#'Continue'.dtid, 2) of
						{error, _Reason}  ->
							error_logger:error_report(["DTID not found in received N-CONTINUE",
									{dtid, TPDU#'End'.dtid}, {nsap, State#state.nsap},
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
%% TODO
							%% Build ABORT message with appropriate P-Abort Cause values
							%% N-UNITDATA request TSL -> SCCP
							%% Discard received message
							%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
							{noreply, State};
						TSM ->
							TsmParms = UdataParms#'N-UNITDATA'{userData = Continue},
							%% CONTINUE received TSM <- TCO
							gen_fsm:send_event(TSM, {'CONTINUE', received, TsmParms}),
							{noreply, State}
					end;
				{error, Reason} ->
%% TODO
					%% OTID derivable?
					%% DTID assigned?
					%% Build ABORT message with appropraite P-Abort Cause value
					%% N-UNITDATA request TSL -> SCCP
					%% Local Abort TSM <- TCO
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
					error_logger:error_report(["Syntax error in received N-CONTINUE", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {'end', TPDU}} ->
			case 'TR':decode('End', TPDU) of
				{ok, End} ->
					%% DTID assigned?
					case catch ets:lookup(tcap_transaction, TPDU#'End'.dtid, 2) of
						{error, _Reason}  ->
							error_logger:error_report(["DTID not found in received N-END",
									{dtid, TPDU#'End'.dtid}, {nsap, State#state.nsap}, 
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
							%% Discard received message
							%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
							{noreply, State};
						TSM ->
							TsmParms = UdataParms#'N-UNITDATA'{userData = End},
							%% END received TSM <- TCO
							gen_fsm:send_event(TSM, {'END', received, TsmParms}),
							{noreply, State}
					end;
				{error, Reason} ->
%% TODO
					%% DTID assigned?
					%%    Local Abort TSM <- TCO
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
					error_logger:error_report(["Syntax error in received N-END", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {abort, TPDU}} ->
			case 'TR':decode('Abort', TPDU) of
				{ok, Abort} ->
					%% DTID assigned?
					case catch ets:lookup(tcap_transaction, TPDU#'Abort'.dtid, 2) of
						{error, _Reason} ->
							error_logger:error_report(["DTID not found in received N-ABORT",
									{dtid, TPDU#'Abort'.dtid}, {nsap, State#state.nsap},
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
							%% Discard received message
							%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
							{noreply, State};
						TSM ->
							TsmParms = UdataParms#'N-UNITDATA'{userData = Abort},
							%% Abort received TSM <- TCO
							gen_fsm:send_event(TSM, {'ABORT', received, TsmParms}),
							{noreply, State}
					end;
				{error, Reason} ->
%% TODO
					%% DTID assigned?
					%%    Local Abort TSM <- TCO
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
					error_logger:error_report(["Syntax error in received N-ABORT", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {error, Reason}} ->
%% TODO
			%% Message type unknown
			%% OTID derivable?
			%% DTID assigned?
			%% Build ABORT message with appropraite P-Abort Cause value
			%% N-UNITDATA request TSL -> SCCP
			%% Local Abort TSM <- TCO
			%% Discard received message
			%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
			error_logger:error_report(["Unknown TCMessage received", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
			{noreply, State}
	end;
handle_cast({'N', 'NOTICE', indication, NoticeParms}, State) ->
	%% Extract the originating transactionID
	case 'TR':decode('TCMessage', NoticeParms#'N-NOTICE'.userData) of
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, Begin} ->
					TransactionID = Begin#'Begin'.otid;
				_ ->
					TransactionID = undefined
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, Continue} ->
					TransactionID = Continue#'Continue'.otid;
				_ ->
					TransactionID = undefined
			end;
		_ ->
			TransactionID = undefined
	end,
	%% TR-NOTICE indication CSL <- TSL
	%% reference: Figure A.3/Q.774 (sheet 2 of 4)
	%% The CSL is a null layer for this indication so it becomes
	%% TC-NOTICE indication TCU <- TSL
	%% reference: Figure A.5/Q.774 (sheet 7 of 11)
	%% reference: Figure A.3/Q.774 (sheet 10 of 11)
	TcParms = #'TC-NOTICE'{
			dialogueID = TransactionID,
			origAddress = NoticeParms#'N-NOTICE'.callingAddress,
			destAddress = NoticeParms#'N-NOTICE'.calledAddress,
			reportCause = NoticeParms#'N-NOTICE'.reason},
	gen_fsm:send_event(State#state.usap, {'TC', 'NOTICE', indication, TcParms}),
	{noreply, State};
%%%
%%% service primitive requests from the TR-User
%%% reference: Figure A.3/Q.774 (sheets 2&3 of 4)
handle_cast({'TR', 'UNI', request, UniParms}, State) 
		when is_record(UniParms, 'TR-UNI') ->
	%% Assemble TR-portion of UNI message
	{SequenceControl, ReturnOption} = UniParms#'TR-UNI'.qos,
	DialoguePortion = (UniParms#'TR-UNI'.userData)#'TR-user-data'.dialoguePortion,
	ComponentPortion = (UniParms#'TR-UNI'.userData)#'TR-user-data'.componentPortion,
	TPDU = 'TR':encode('TCMessage', {unidirectional, #'Unidirectional'{
			dialoguePortion = DialoguePortion, components = ComponentPortion}}),
	SccpParms = #'N-UNITDATA'{calledAddress = UniParms#'TR-UNI'.destAddress,
			callingAddress =  UniParms#'TR-UNI'.origAddress,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			userData = TPDU},
	gen_fsm:send_event(State#state.nsap, {'N', 'UNITDATA', request, SccpParms}),
	{noreply, State};
handle_cast({'TR', 'BEGIN', request, BeginParms}, State) 
		when is_record(BeginParms, 'TR-BEGIN') ->
	%% Create a Transaction State Machine (TSM)
	OTID = BeginParms#'TR-BEGIN'.transactionID,
	ChildName = list_to_atom("tsm_" ++ integer_to_list(OTID)),
	{ok, {M, F, A, Mods}} = application:get_env(start_tsm),
	StartFunc = {M, F, A ++ [{State#state.nsap, State#state.usap, OTID, ChildName}]},
	ChildSpec = {ChildName, StartFunc, temporary, infinity, worker, Mods},
	{ok, TSM} = supervisor:start_child(State#state.sup, ChildSpec),
	gen_fsm:send_event(TSM, {'BEGIN', transaction, BeginParms}),
	{noreply, State};
handle_cast({'TR', 'CONTINUE', request, ContParms}, State)
		when is_record(ContParms, 'TR-CONTINUE') ->
	TransactionID = ContParms#'TR-CONTINUE'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'CONTINUE', transaction, ContParms}),
	{noreply, State};
handle_cast({'TR', 'END', request, EndParms}, State)
		when is_record(EndParms, 'TR-END') ->
	TransactionID = EndParms#'TR-END'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'END', transaction, EndParms}),
	{noreply, State};
handle_cast({'TR', 'U-ABORT', request, AbortParms}, State)
		when is_record(AbortParms, 'TR-U-ABORT') ->
	TransactionID = AbortParms#'TR-U-ABORT'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'ABORT', transaction, AbortParms}),
	{noreply, State};
%%
%% The TSM sends us a message as it's last action so
%% we can remove the supervisor child specification
%%
handle_cast({'tsm-stopped', SupRef}, State) ->
	supervisor:delete_child(State#state.sup, SupRef),
	%% reference: Figure A.3/Q/774 (sheet 2 of 4)
	{noreply, State};
% unrecognized request
handle_cast(Request, State) ->
	Module = State#state.module,
	case Module:handle_cast(Request, State#state.ext_state) of
		{noreply, ExtState} ->
			{noreply, State#state{ext_state = ExtState}};
		{noreply, ExtState, Timeout} ->
			{noreply, State#state{ext_state = ExtState}, Timeout};
		{primitive, Primitive, ExtState} ->
			handle_cast(Primitive, State#state{ext_state = ExtState});
		{stop, Reason, ExtState} ->
			{stop, Reason, State#state{ext_state = ExtState}}
	end.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}
				| {primitive, Primitive, NewState :: state()},
		Primitive :: {'N', 'UNITDATA', indication, #'N-UNITDATA'{}}
				| {'N', 'NOTICE', indication, #'N-NOTICE'{}}.
%% @doc Handle a received message.
%%
%% 	A user callback module may return an SCCP service primitive
%% 	to TCO for processing with the return value 
%% 	`{primitive, Primitive, NewState}'.
%%
%% @@see //stdlib/gen_server:handle_info/2
%% @private
handle_info({'EXIT', _Pid, Reason}, State) ->
	{stop, Reason, State};
handle_info(Info, State) ->
	Module = State#state.module,
	case erlang:function_exported(Module, handle_info, 2) of
		true ->
			case Module:handle_info(Info, State#state.ext_state) of
				{noreply, ExtState} ->
					{noreply, State#state{ext_state = ExtState}};
				{noreply, ExtState, Timeout} ->
					{noreply, State#state{ext_state = ExtState}, Timeout};
				{primitive, Primitive, ExtState} ->
					handle_cast(Primitive, State#state{ext_state = ExtState});
				{stop, Reason, ExtState} ->
					{stop, Reason, State#state{ext_state = ExtState}}
			end;
		false ->
			{noreply, State}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: state().
%% @see //stdlib/gen_server:terminate/3
%% @private
terminate(Reason, State) ->
	Module = State#state.module,
	case erlang:function_exported(Module, terminate, 2) of
		true ->
			Module:terminate(Reason, State#state.ext_state);
		false ->
			ok
	end.

-spec code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
%% @see //stdlib/gen_server:code_change/3
%% @private
code_change(OldVersion, State, Extra) ->
	Module = State#state.module,
	case erlang:function_exported(Module, code_change, 3) of
		true ->
			case Module:code_change(OldVersion, State#state.ext_state, Extra) of
				{ok, ExtState} ->
					{ok, State#state{ext_state = ExtState}};
				{error, Reason} ->
					{error, Reason}
			end;
		false ->
			{ok, State}
	end.

-spec format_status(Opt, StatusData) -> Status
	when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().
%% @see //stdlib/gen_server:format_status/3
%% @private
format_status(Opt, [PDict, State] = _StatusData) ->
	Module = State#state.module,
	case erlang:function_exported(Module, format_status, 2) of
		true ->
			Module:format_status(Opt, [PDict, State#state.ext_state]);
		false ->
			case Opt of
				terminate ->
					State;
				_ ->
					[{data, [{"State", State}]}]
			end
	end.

%%----------------------------------------------------------------------
%%  The gen_server API functions
%%----------------------------------------------------------------------

%% @hidden
start(Module, SupRef, Args, Options) ->
	gen_server:start(?MODULE, [SupRef, Module, Args], Options).

%% @hidden
start(ServerRef, SupRef, Module, Args, Options) ->
	gen_server:start(ServerRef, ?MODULE, [SupRef, Module, Args], Options).

%% @hidden
start_link(Module, Args, Options) ->
	gen_fsm:start_link(?MODULE, [Module, Args], Options).

%% @hidden
start_link(ServerRef, Module, Args, Options) ->
	gen_fsm:start_link(ServerRef, ?MODULE, [Module, Args], Options).

%% @hidden
call(ServerRef, Request) ->
	gen_server:call(ServerRef, Request).

%% @hidden
call(ServerRef, Request, Timeout) ->
	gen_server:call(ServerRef, Request, Timeout).

%% @hidden
multi_call(Name, Request) ->
	gen_server:multi_call(Name, Request).

%% @hidden
multi_call(Nodes, Name, Request) ->
	gen_server:multi_call(Nodes, Name, Request).

%% @hidden
multi_call(Nodes, Name, Request, Timeout) ->
	gen_server:multi_call(Nodes, Name, Request, Timeout).

%% @hidden
cast(ServerRef, Request) ->
	gen_server:cast(ServerRef, Request).

%% @hidden
abcast(Name, Request) ->
	gen_server:abcast(Name, Request).

%% @hidden
abcast(Nodes, Name, Request) ->
	gen_server:abcast(Nodes, Name, Request).

%% @hidden
reply(Client, Reply) ->
	gen_server:reply(Client, Reply).

%% @hidden
enter_loop(Module, Options, State) ->
	gen_server:enter_loop(Module, Options, State).

%% @hidden
enter_loop(Module, Options, State, ServerName, Timeout) ->
	gen_server:enter_loop(Module, Options, State, ServerName, Timeout).

%% @hidden
enter_loop(Module, Options, State, Timeout) ->
	gen_server:enter_loop(Module, Options, State, Timeout).
% enter_loop(Module, Options, State, ServerName) ->
%	gen_server:enter_loop(Module, Options, State, ServerName).

%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

-spec new_tid() -> tid().
%% @doc Get the next originating transaction id from the global counter
%%
%% NOTE:  we are simply assuming that when the counter rolls over the last 
%%        transaction to have this ID is long gone (4.2 billion IDs)
%% @private
%% @end
%% reference: Figure A.3 bis/Q.774
new_tid() ->
	ets:update_counter(tcap_transaction, transactionID, {2, 1, 16#ffffffff, 0}).


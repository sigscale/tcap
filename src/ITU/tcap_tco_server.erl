%%% tcap_tco_server.erl
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom, 2010-2011 Harald Welte
%%% @author Vance Shipley <vances@motivity.ca>
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
%%% Copyright (c) 2010-2011, Harald Welte
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
%%% @doc Transaction Coordinator (TCO) functional block within the
%%% 		transaction sub-layer (TSL) of ITU TCAP.
%%%
%%% 	This module implements the transaction coordinator (TCO)
%%% 	functional block.  Adaptations to specific SCCP layer 
%%% 	and TC-User implementations may be implemented as callback
%%% 	modules behaving to this behaviour module.  This module behaves
%%% 	to {@link //stdlib/gen_server. gen_server}.
%%%
%%% 	==Usage==
%%% 	The callback module should be implemented as a gen_server
%%% 	behaviour but with a `{@module}' behaviour module attribute:
%%% 	```
%%% 	-behaviour({@module}).
%%% 	'''
%%% 	The call back module handles the SCCP &#8594; TCAP primitives
%%% 	directly, performs any reformatting required, and returns the
%%% 	standard primitives to the `{@module}' handler.  A very
%%% 	simple example is the {@link //sccp. sccp}
%%% 	application which implements the SCCP SAP as a pid() which sends
%%%   and receives messages in the primitive format.  In our callback
%%% 	module we create a `handle_info/2' clause which matches
%%% 	the primitives:
%%% 	```
%%% 	handle_info({'N', _, indication, _} = Primitive, State) -&gt;
%%% 	      {primitive, Primitive, State}.
%%% 	'''
%%% 	As the example above illustrates the `{@module}'
%%% 	behaviour extends the allowed return values to accept the direct
%%% 	return of a received service primitive.
%%%
%%% 	The `handle_cast/2' function may be used in the same way.
%%%
%%% 	<h2><a name="callbacks">Callback Functions</a></h2>
%%% 	In addition to the {@link //stdlib/gen_server. gen_server}
%%% 	behaviour callbacks the following callback functions are used.
%%% 	
%%% 	<h3 class="function">
%%% 		<a name="send_primitive-2">send_primitive/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>send_primitive(Primitive, State) -&gt; void()</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Primitive = {'N', 'UNITDATA', request, UdataParams}</tt></li>
%%% 			<li><tt>UdataParams = #'N-UNITDATA'{}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	The `TCO' will call this function when it has a service primitive
%%% 	to deliver to the SCCP layer.
%%%
%%% 	<h3  class="function">
%%% 		<a name="start_aei-2">start_aei/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>start_aei(DialoguePortion, State) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>DialoguePortion = binary()</tt></li>
%%% 			<li><tt>TSL = pid()</tt></li>
%%% 			<li><tt>State = term()</tt></li>
%%% 			<li><tt>Result = {ok, DHA, CCO, TCU, State}
%%% 					| {error, Reason}</tt></li>
%%% 			<li><tt>DHA = pid()</tt></li>
%%% 			<li><tt>CCO = pid()</tt></li>
%%% 			<li><tt>TCU = pid()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	This function is called by TCO to initialize an Application
%%% 	Entity Instance (AEI) in response to a remote TC-User initiating
%%% 	a dialogue. A transaction capabilities (TC) Application Service
%%% 	Element (ASE) is represented by a newly created TC component
%%% 	sublayer (CSL) instance.
%%%
%%% 	`DialoguePortion' is the undecoded dialogue portion.
%%%
%%% 	`DHA' is the pid of the dialogue handler in the newly created CSL.
%%%
%%% 	`CCO' is the pid of the component coordinator in the newly created CSL.
%%%
%%% 	`TCU' is the pid of the TC-User which shall received indications
%%% 	from the CSL.
%%%
%%% 	<h3  class="function">
%%% 		<a name="start_dialogue-2">start_dialogue/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>start_dialogue(DialoguePortion, State) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>DialoguePortion = binary()</tt></li>
%%% 			<li><tt>State = term()</tt></li>
%%% 			<li><tt>Result = {ok, DHA, State} | {error, Reason}</tt></li>
%%% 			<li><tt>DHA = pid()</tt></li>
%%% 			<li><tt>Reason = term()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	The callback module may optionally export this function
%%%	to overide the default method used to start a dialogue
%%% 	handler (DHA).
%%%
%%% 	The start function must create and link to the child process,
%%% 	and should return `{ok, DHA}' where `DHA' is the pid of
%%% 	the (possibly remote) DHA process.
%%% @end
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_tco_server).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').

-behaviour(gen_server).

% export the gen_server interface
-export([start/3, start/4, start_link/3, start_link/4,
		call/2, call/3, cast/2, abcast/2, abcast/3, reply/2,
		multi_call/2, multi_call/3, multi_call/4,
		enter_loop/3, enter_loop/4, enter_loop/5]).

% export the gen_server call backs
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, handle_continue/2,
		code_change/3, format_status/2]).

% export the private api
-export([new_tid/0]).
-export_type([tid/0]).

-include("TCAPMessages.hrl").
%-include("TR.hrl").
-include("tcap.hrl").
-include("sccp_primitive.hrl").

-record(state,
		{sup:: pid(),
		module :: atom(),
		ext_state :: any(),
		usap :: pid()}).
-type state() :: #state{}.

-type tid() :: 0..4294967295.

-callback send_primitive(Primitive, State) -> any()
	when
		State :: state(),
		Primitive :: {'N', 'UNITDATA', request, UdataParams},
		UdataParams :: #'N-UNITDATA'{}.
-callback start_aei(DialoguePortion, State) -> Result
	when
		DialoguePortion:: binary(),
		State :: term(),
		Result :: {ok, DHA, CCO, TCU, State} | {error, Reason},
		DHA :: pid(),
		CCO :: pid(),
		TCU :: pid(),
		Reason :: term().
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
		terminate/2, code_change/3, format_status/2]).

%%----------------------------------------------------------------------
%%  The gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: state()}
				| {ok, State :: state(), Timeout :: timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term()} | ignore.
%% @see //stdlib/gen_server:init/1
%% @private
init([Module, [Sup | Args]])
		when is_atom(Module), is_pid(Sup) ->
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
%
% reference: Figure A.3/Q.774 (sheet 1 of 4)
handle_cast({'N', 'UNITDATA', indication,
		#'N-UNITDATA'{userData = UserData1} = UdataParams},
		#state{module = Module, ext_state = ExtState1} = State) ->
	{ok, {Tag, ActRes}} = 'TR':decode('TCMessage', UserData1),
%					{error, Reason} ->
%					% Discard received message
%					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
%					error_logger:error_report(["Syntax error in received N-UNI", {error, Reason},
%							{caller, UdataParams#'N-UNITDATA'.callingAddress},
%							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
%					{noreply, State}
%			end;
	PpRes = {Tag, postproc_tcmessage(ActRes)},
	case PpRes of
		{unidirectional, #'Unidirectional'{dialoguePortion = DialoguePortion,
				components = ComponentPortion}} ->
			case Module:start_aei(DialoguePortion, ExtState1) of
				{ok, DHA, _CCO, _TCU, ExtState2}  ->
					% TR-UNI indication CSL <- TSL
					UserData2 = #'TR-user-data'{dialoguePortion = DialoguePortion,
							componentPortion = ComponentPortion},
					TrParams = #'TR-UNI'{qos = {UdataParams#'N-UNITDATA'.sequenceControl,
							UdataParams#'N-UNITDATA'.returnOption},
							destAddress = UdataParams#'N-UNITDATA'.calledAddress,
							origAddress = UdataParams#'N-UNITDATA'.callingAddress,
							userData = UserData2},
					gen_fsm:send_event(DHA, {'TR', 'UNI', indication, TrParams}),
					{noreply, State#state{ext_state = ExtState2}};
				{error, _Reason} ->
					{noreply, State}
			end;
		{'begin', TPDU = #'Begin'{}} ->
			% Assign local transaction ID
			TransactionID = new_tid(),
			ChildSpec = [],
			% Is TID = no TID?
			% Note:  The assignment of the ID above just gets the next available
			%        value and doesn't ensure that it is not in use (unlikely)
			%        or that there are enough resources available.  The real
			%        test is in whether the start succeeds.
			case supervisor:start_child(State#state.sup, ChildSpec) of
				{ok, _TransSupPid} ->
					% Created a Transaction State Machine (TSM)
					case ets:lookup_element(tcap_transaction, TransactionID, 2) of
					TSM when is_integer(TSM) ->
						TsmParams = UdataParams#'N-UNITDATA'{userData = TPDU},
						% BEGIN received TSM <- TCO
						gen_fsm:send_event(TSM, {'BEGIN', received, TsmParams});
					{error, _Reason} ->
						error_logger:error_report(["Unable to find TSM that was just started"])
					end;
				{error, Reason} ->
					error_logger:error_report(["Unable to start TSM", {childspec, ChildSpec}, {error, Reason}]),
					% TID = no TID
					% Build ABORT message (P-Abort Cause = Resource Limitation)
					Abort = {abort, #'Abort'{dtid = encode_tid(TPDU#'Begin'.otid),
							reason = {'p-abortCause', resourceLimitation}}},
					case 'TR':encode('TCMessage', Abort) of
					{ok, EncAbort} ->
						SccpParams = #'N-UNITDATA'{calledAddress = UdataParams#'N-UNITDATA'.callingAddress,
								callingAddress = UdataParams#'N-UNITDATA'.calledAddress,
								sequenceControl = false,
								returnOption = false,
								userData = list_to_binary(EncAbort)},
						% TR-UNI request TSL -> SCCP
						Module = State#state.module,
						Module:send_primitive({'N', 'UNITDATA', request, SccpParams}, State#state.ext_state);
					{error, Err} ->
						error_logger:error_report(["Error generating ASN1", {abort, Abort}, {error, Err}])
					end,
					error_logger:error_report(["Unable to create TSM for received N-BEGIN",
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}])
			end,
			{noreply, State};
%				{error, Reason} ->
% TODO
%					% is OTID derivable?
%					%    Build ABORT message with appropraite P-Abort Cause value
%					%    N-UNITDATA request TSL -> SCCP
%					% Discard received message
%					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
%					error_logger:error_report(["Syntax error in received N-BEGIN", {error, Reason},
%									{caller, UdataParams#'N-UNITDATA'.callingAddress},
%									{called, UdataParams#'N-UNITDATA'.calledAddress}]),
%					{noreply, State}
		{continue, TPDU = #'Continue'{dtid = Dtid}} ->
			% DTID assigned?
			case ets:lookup_element(tcap_transaction, Dtid, 2) of
				{error, _Reason}  ->
					error_logger:error_report(["DTID not found in received N-CONTINUE",
								{dtid, Dtid},
								{caller, UdataParams#'N-UNITDATA'.callingAddress},
								{called, UdataParams#'N-UNITDATA'.calledAddress}]),
% TODO
					% Build ABORT message with appropriate P-Abort Cause values
					% N-UNITDATA request TSL -> SCCP
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
					{noreply, State};
				TSM ->
					TsmParams = UdataParams#'N-UNITDATA'{userData = TPDU},
					% CONTINUE received TSM <- TCO
					gen_fsm:send_event(TSM, {'CONTINUE', received, TsmParams}),
					{noreply, State}
			end;
%				{error, Reason} ->
% TODO
					% OTID derivable?
					% DTID assigned?
					% Build ABORT message with appropraite P-Abort Cause value
					% N-UNITDATA request TSL -> SCCP
					% Local Abort TSM <- TCO
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
%					error_logger:error_report(["Syntax error in received N-CONTINUE", {error, Reason},
%							{caller, UdataParams#'N-UNITDATA'.callingAddress},
%							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
%					{noreply, State}
%			end;
		{'end', TPDU = #'End'{dtid = Dtid}} ->
			% DTID assigned?
			case ets:lookup_element(tcap_transaction, Dtid, 2) of
				{error, _Reason}  ->
					error_logger:error_report(["DTID not found in received N-END",
								{dtid, Dtid},
								{caller, UdataParams#'N-UNITDATA'.callingAddress},
								{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					{noreply, State};
				TSM ->
					TsmParams = UdataParams#'N-UNITDATA'{userData = TPDU},
					% END received TSM <- TCO
					gen_fsm:send_event(TSM, {'END', received, TsmParams}),
					{noreply, State}
			end;
%				{error, Reason} ->
% TODO
%					% DTID assigned?
%					%    Local Abort TSM <- TCO
%					% Discard received message
%					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
%					error_logger:error_report(["Syntax error in received N-END", {error, Reason},
%							{caller, UdataParams#'N-UNITDATA'.callingAddress},
%							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
%					{noreply, State}
%			end;
		{abort, TPDU = #'Abort'{}} ->
			% DTID assigned?
			case catch ets:lookup(tcap_transaction, TPDU#'Abort'.dtid, 2) of
				{error, _Reason} ->
					error_logger:error_report(["DTID not found in received N-ABORT",
							{dtid, TPDU#'Abort'.dtid},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					{noreply, State};
				TSM ->
					TsmParams = UdataParams#'N-UNITDATA'{userData = TPDU},
					% Abort received TSM <- TCO
					gen_fsm:send_event(TSM, {'ABORT', received, TsmParams}),
					{noreply, State}
			end;
%					{error, Reason} ->
% TODO
%					% DTID assigned?
%					%    Local Abort TSM <- TCO
%					% Discard received message
%					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
%					error_logger:error_report(["Syntax error in received N-ABORT", {error, Reason},
%							{caller, UdataParams#'N-UNITDATA'.callingAddress},
%							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
%					{noreply, State}
%			end;
		{error, Reason} ->
% TODO
			% Message type unknown
			% OTID derivable?
			% DTID assigned?
			% Build ABORT message with appropraite P-Abort Cause value
			% N-UNITDATA request TSL -> SCCP
			% Local Abort TSM <- TCO
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
			error_logger:error_report(["Unknown TCMessage received", {error, Reason},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
			{noreply, State}
	end;
handle_cast({'N', 'NOTICE', indication, NoticeParams}, State) ->
	% Extract the originating transactionID
	case 'TR':decode('TCMessage', NoticeParams#'N-NOTICE'.userData) of
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
	% TR-NOTICE indication CSL <- TSL
	% reference: Figure A.3/Q.774 (sheet 2 of 4)
	% The CSL is a null layer for this indication so it becomes
	% TC-NOTICE indication TCU <- TSL
	% reference: Figure A.5/Q.774 (sheet 7 of 11)
	% reference: Figure A.3/Q.774 (sheet 10 of 11)
	TcParams = #'TC-NOTICE'{
			dialogueID = TransactionID,
			origAddress = NoticeParams#'N-NOTICE'.callingAddress,
			destAddress = NoticeParams#'N-NOTICE'.calledAddress,
			reportCause = NoticeParams#'N-NOTICE'.reason},
	% TODO:  fixme!!! gen_fsm:send_event(State#state.usap, {'TC', 'NOTICE', indication, TcParams}),
	{noreply, State};
%%
%% service primitive requests from the TR-User
%% reference: Figure A.3/Q.774 (sheets 2&3 of 4)
handle_cast({'TR', 'UNI', request, UniParams}, State) 
		when is_record(UniParams, 'TR-UNI') ->
	% Assemble TR-portion of UNI message
	{SequenceControl, ReturnOption} = UniParams#'TR-UNI'.qos,
	DialoguePortion = (UniParams#'TR-UNI'.userData)#'TR-user-data'.dialoguePortion,
	ComponentPortion = (UniParams#'TR-UNI'.userData)#'TR-user-data'.componentPortion,
	case 'TR':encode('TCMessage', {unidirectional, #'Unidirectional'{
			 dialoguePortion = DialoguePortion, components = ComponentPortion}}) of
		{ok, TPDU} ->
			TpduBin = iolist_to_binary(TPDU),
			SccpParams = #'N-UNITDATA'{calledAddress = UniParams#'TR-UNI'.destAddress,
					callingAddress =  UniParams#'TR-UNI'.origAddress,
					sequenceControl = SequenceControl, returnOption = ReturnOption,
					userData = TpduBin},
			Module = State#state.module,
			Module:send_primitive({'N', 'UNITDATA', request, SccpParams}, State#state.ext_state),
			{noreply, State};
		{error, Err} ->
			error_logger:error_report(["Error generating ASN1", {error, Err},
					{dialogue_portion, DialoguePortion},
					{components, ComponentPortion}]),
			{noreply, State}
	end;
handle_cast({'TR', 'BEGIN', request, BeginParams}, State)
		when is_record(BeginParams, 'TR-BEGIN') ->
	% Create a Transaction State Machine (TSM)
	TransactionID = BeginParams#'TR-BEGIN'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'BEGIN', transaction, BeginParams}),
	{noreply, State};
handle_cast({'TR', 'CONTINUE', request, ContParams}, State)
		when is_record(ContParams, 'TR-CONTINUE') ->
	TransactionID = ContParams#'TR-CONTINUE'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'CONTINUE', transaction, ContParams}),
	{noreply, State};
handle_cast({'TR', 'END', request, EndParams}, State)
		when is_record(EndParams, 'TR-END') ->
	TransactionID = EndParams#'TR-END'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'END', transaction, EndParams}),
	{noreply, State};
handle_cast({'TR', 'U-ABORT', request, AbortParams}, State)
		when is_record(AbortParams, 'TR-U-ABORT') ->
	TransactionID = AbortParams#'TR-U-ABORT'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'ABORT', transaction, AbortParams}),
	{noreply, State};
%
% The TSM sends us a message as it's last action so
% we can remove the supervisor child specification
%
handle_cast({'tsm-stopped', SupRef}, State) ->
	supervisor:delete_child(State#state.sup, SupRef),
	% reference: Figure A.3/Q/774 (sheet 2 of 4)
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

-spec handle_continue(Info, State) -> Result
	when
		Info :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle continued execution.
handle_continue(Info, State) ->
	Module = State#state.module,
	case erlang:function_exported(Module, handle_continue, 2) of
		true ->
			case Module:handle_continue(Info, State#state.ext_state) of
				{noreply, ExtState} ->
					{noreply, State#state{ext_state = ExtState}};
				{noreply, ExtState, Timeout} ->
					{noreply, State#state{ext_state = ExtState}, Timeout};
				{stop, Reason, ExtState} ->
					{stop, Reason, State#state{ext_state = ExtState}}
			end;
		false ->
			{noreply, State}
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
start(Module, Args, Options) ->
	gen_server:start(?MODULE, [Module, Args], Options).

%% @hidden
start(ServerName, Module, Args, Options) ->
	gen_server:start(ServerName, ?MODULE, [Module, Args], Options).

%% @hidden
start_link(Module, Args, Options) ->
	gen_server:start_link(?MODULE, [Module, Args], Options).

%% @hidden
start_link(ServerName, Module, Args, Options) ->
	gen_server:start_link(ServerName, ?MODULE, [Module, Args], Options).

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

% convert a TID from the four-octet binary/list form (OCTET STRING) to unsigned int
%% @hidden
decode_tid(Bin) when is_binary(Bin) ->
	binary:decode_unsigned(Bin);
decode_tid(List) when is_list(List) ->
	decode_tid(list_to_binary(List)).

%% @hidden
encode_tid(In) when is_integer(In) ->
	<<In:32/big>>;
encode_tid(In) when is_list(In) ->
	list_to_binary(In);
encode_tid(In) when is_binary(In) ->
	In.

%% @hidden
postproc_tcmessage(#'Unidirectional'{} = Unidirectional) ->
	Unidirectional;
postproc_tcmessage(#'Continue'{otid = Otid, dtid = Dtid} = Continue) ->
	Continue#'Continue'{otid = decode_tid(Otid), dtid = decode_tid(Dtid)};
postproc_tcmessage(#'End'{dtid = Dtid} = End) ->
	End#'End'{dtid = decode_tid(Dtid)};
postproc_tcmessage(#'Begin'{otid = Otid} = Begin) ->
	Begin#'Begin'{otid = decode_tid(Otid)}.


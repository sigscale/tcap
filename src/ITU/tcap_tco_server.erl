%%% tcap_tco_server.erl
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
%%% 	handle_info({'N', _, indication, _} = Primitive, State) ->
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
%%% 			<tt>send_primitive(Primitive, State) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Primitive = {'N', 'UNITDATA', request, UdataParams}</tt></li>
%%% 			<li><tt>UdataParams = #'N-UNITDATA'{}</tt></li>
%%% 			<li><tt>Result = {noreply, NewState}
%%% 					| {noreply, NewState, Timeout}
%%% 					| {noreply, NewState, hibernate}
%%% 					| {noreply, NewState, {continue, Continue}}
%%% 					| {stop, Reason, NewState}</tt>
%%% 			</li>
%%% 			<li><tt>NewState = term()</tt></li>
%%% 			<li><tt>Timeout = int()>=0 | infinity</tt></li>
%%% 			<li><tt>Continue = term()</tt></li>
%%% 			<li><tt>Reason = term()</tt></li>
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
%%% 			<li><tt>DialoguePortion = #'EXTERNAL'{}</tt></li>
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
%%% 	`DialoguePortion' is the dialogue portion.
%%%
%%% 	`DHA' is the pid of the dialogue handler in the newly created CSL.
%%%
%%% 	`CCO' is the pid of the component coordinator in the newly created CSL.
%%%
%%% 	`TCU' is the pid of the TC-User which shall received indications
%%% 	from the CSL.
%%% @end
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_tco_server).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-copyright('Copyright (c) 2010-2011 Harald Welte').
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').
-author('Harald Welte <laforge@gnumonks.org>').

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
-include("DialoguePDUs.hrl").
-include("tcap.hrl").
-include_lib("sccp/include/sccp_primitive.hrl").

-record(state,
		{tsl_sup:: pid(),
		tsm_sup :: pid(),
		tsm  = #{} :: map(),
		callback :: atom() | #tcap_tco_cb{},
		ext_state :: any()}).
-type state() :: #state{}.

-type tid() :: 0..4294967295.

-callback send_primitive(Primitive, State) -> Result
	when
		Primitive :: {'N', 'UNITDATA', request, UdataParams},
		UdataParams :: #'N-UNITDATA'{},
		State :: any(),
		Result :: {noreply, NewState :: any()}
				| {noreply, NewState :: any(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: any()}.
-callback start_aei(DialoguePortion, State) -> Result
	when
		DialoguePortion :: #'EXTERNAL'{},
		State :: any(),
		Result :: {ok, DHA, CCO, TCU, State} | {error, Reason},
		DHA :: pid(),
		CCO :: pid(),
		TCU :: pid(),
		Reason :: term().
-callback init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: any()}
				| {ok, State :: any(), Timeout :: timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term()} | ignore.
-callback handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: {pid(), Tag :: any()},
		State :: any(),
		Result :: {reply, Reply :: term(), NewState :: any()}
				| {reply, Reply :: term(), NewState :: any(), timeout() | hibernate | {continue, term()}}
				| {noreply, NewState :: any()}
				| {noreply, NewState :: any(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), Reply :: term(), NewState :: any()}
				| {stop, Reason :: term(), NewState :: any()}.
-callback handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: any(),
		Result :: {noreply, NewState :: any()}
				| {noreply, NewState :: any(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: any()}
				| {primitive, Primitive, NewState :: any()},
		Primitive :: {'N', 'UNITDATA', indication, #'N-UNITDATA'{}}
				| {'N', 'NOTICE', indication, #'N-NOTICE'{}}.
-callback handle_continue(Info, State) -> Result
	when
		Info :: term(),
		State :: any(),
		Result :: {noreply, NewState :: any()}
				| {noreply, NewState :: any(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: any()}.
-callback handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State :: any(),
		Result :: {noreply, NewState :: any()}
				| {noreply, NewState :: any(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: any()}
				| {primitive, Primitive, NewState :: any()},
		Primitive :: {'N', 'UNITDATA', indication, #'N-UNITDATA'{}}
				| {'N', 'NOTICE', indication, #'N-NOTICE'{}}.
-callback terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: any().
-callback code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: any(),
		Extra :: term(),
		Result :: {ok, NewState :: any()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status
	when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: any(),
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
init([Callback, [Sup | Args]])
		when (is_atom(Callback) or is_record(Callback, tcap_tco_cb)),
		is_pid(Sup) ->
	process_flag(trap_exit, true),
	CbArgs = [Args],
   case tcap_tco_callback:cb(init, Callback, CbArgs) of
		{ok, ExtState} ->
			NewState = #state{tsl_sup = Sup, callback = Callback, ext_state = ExtState},
			{ok, NewState, {continue, {?MODULE, infinity}}};
		{ok, ExtState, Timeout} ->
			NewState = #state{tsl_sup = Sup, callback = Callback, ext_state = ExtState},
			{ok, NewState, {continue, {?MODULE, Timeout}}};
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
handle_call({'TR', 'BEGIN', request,
		#'TR-BEGIN'{transactionID = TransactionID} = BeginParams},
		{DHA, _} = _From, #state{tsm_sup = Sup, tsm = Map} = State) ->
	case supervisor:start_child(Sup, [[self(), DHA, TransactionID], []]) of
		{ok, TSM} ->
			gen_statem:cast(TSM, {'BEGIN', transaction, BeginParams}),
			{reply, ok, State#state{tsm = Map#{TransactionID => TSM}}};
		{error, Reason} ->
			{stop, {error, Reason}, Reason, State}
	end;
handle_call(Request, From,
		#state{callback = Callback, ext_state = ExtState} = State) ->
	CbArgs = [Request, From, ExtState],
	case tcap_tco_callback:cb(handle_call, Callback, CbArgs) of
		{reply, Reply, ExtState1} ->
			{reply, Reply, State#state{ext_state = ExtState1}};
		{reply, Reply, ExtState1, Timeout} ->
			{reply, Reply, State#state{ext_state = ExtState1}, Timeout};
		{noreply, ExtState1} ->
			{noreply, State#state{ext_state = ExtState1}};
		{noreply, ExtState1, Timeout} ->
			{noreply, State#state{ext_state = ExtState1}, Timeout};
		{stop, Reason, Reply, ExtState1} ->
			{stop, Reason, Reply, State#state{ext_state = ExtState1}};
		{stop, Reason, ExtState1} ->
			{stop, Reason, State#state{ext_state = ExtState1}}
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
%% 	A user callback may return an SCCP service primitive
%% 	to TCO for processing with the return value 
%% 	`{primitive, Primitive, NewState}'.
%% @@see //stdlib/gen_server:handle_cast/2
%% @private
%% @end
% service primitive indications from the network layer
%
% reference: Figure A.3/Q.774 (sheet 1 of 4)
handle_cast({'N', 'UNITDATA', indication,
		#'N-UNITDATA'{userData = UserData1, calledAddress = CalledAddress,
		callingAddress = CallingAddress, sequenceControl = SequenceControl,
		returnOption = ReturnOption} = UdataParams},
		#state{callback = Callback, ext_state = ExtState1,
		tsm_sup = TsmSup, tsm = Map} = State) ->
	case 'TR':decode('TCMessage', UserData1) of
		{ok, {unidirectional, #'Unidirectional'{dialoguePortion = DialoguePortion,
				components = ComponentPortion}}} ->
			CbArgs = [DialoguePortion, ExtState1],
			case tcap_tco_callback:cb(start_aei, Callback, CbArgs) of
				{ok, DHA, _CCO, _TCU, ExtState2}  ->
					% TR-UNI indication CSL <- TSL
					UserData2 = #'TR-user-data'{dialoguePortion = DialoguePortion,
							componentPortion = ComponentPortion},
					TrParams = #'TR-UNI'{qos = {SequenceControl, ReturnOption},
							destAddress = CalledAddress, origAddress = CallingAddress,
							userData = UserData2},
					gen_statem:cast(DHA, {'TR', 'UNI', indication, TrParams}),
					{noreply, State#state{ext_state = ExtState2}};
				{error, Reason} ->
					error_logger:warning_report(["Error starting AEI",
							{error, Reason},
							{dialogue_portion, DialoguePortion},
							{calling, CallingAddress}, {called, CalledAddress}]),
					{noreply, State}
			end;
		{ok, {'begin', #'Begin'{dialoguePortion = DialoguePortion,
				otid = Otid} = Begin1}} ->
			OTID = decode_tid(Otid),
			CbArgs = [DialoguePortion, ExtState1],
			case tcap_tco_callback:cb(start_aei, Callback, CbArgs) of
				{ok, DHA, _CCO, _TCU, ExtState2}  ->
					NewState = State#state{ext_state = ExtState2},
					% Assign local transaction ID
					TransactionID = new_tid(),
					ChildSpec = [[self(), DHA, TransactionID], []],
					% Is TID = no TID?
					case supervisor:start_child(TsmSup, ChildSpec) of
						{ok, TSM} ->
							Begin2 = Begin1#'Begin'{otid = OTID},
							TsmParams = UdataParams#'N-UNITDATA'{userData = Begin2},
							gen_statem:cast(TSM, {'BEGIN', received, TsmParams}),
							{noreply, NewState#state{tsm = Map#{TransactionID => TSM}}};
						{error, Reason} ->
							% TID = no TID
							Abort = {abort, #'Abort'{dtid = OTID,
									reason = {'p-abortCause', resourceLimitation}}},
							{ok, EncAbort} = 'TR':encode('TCMessage', Abort),
							SccpParams = #'N-UNITDATA'{calledAddress = CallingAddress,
									callingAddress = CalledAddress, sequenceControl = false,
									returnOption = false, userData = EncAbort},
							Primitive = {'N', 'UNITDATA', request, SccpParams},
							CbArgs = [Primitive, ExtState1],
							case tcap_tco_callback:cb(send_primitive, Callback, CbArgs) of
								Result when element(1, Result) == noreply  ->
									{stop, Reason, NewState#state{ext_state = element(2, Result)}};
								{stop, _, ExtState2} ->
									{stop, Reason, NewState#state{ext_state = ExtState2}}
							end
					end;
				{error, Reason} ->
					% TID = no TID
					Abort = {abort, #'Abort'{dtid = Otid,
							reason = {'p-abortCause', resourceLimitation}}},
					{ok, EncAbort} = 'TR':encode('TCMessage', Abort),
					SccpParams = #'N-UNITDATA'{calledAddress = CallingAddress,
							callingAddress = CalledAddress, sequenceControl = false,
							returnOption = false, userData = EncAbort},
					Primitive = {'N', 'UNITDATA', request, SccpParams},
					CbArgs = [Primitive, ExtState1],
					case tcap_tco_callback:cb(send_primitive, Callback, CbArgs) of
						Result when element(1, Result) == noreply  ->
							error_logger:warning_report(["Error starting AEI",
									{error, Reason},
									{dialogue_portion, DialoguePortion},
									{calling, CallingAddress}, {called, CalledAddress}]),
							{noreply, State#state{ext_state = element(2, Result)}};
						{stop, _, ExtState2} ->
							{stop, Reason, State#state{ext_state = ExtState2}}
					end
			end;
		{ok, {'begin', _TPDU}} ->
			% is OTID derivable?
			%    Build ABORT message with appropraite P-Abort Cause value
			%    N-UNITDATA request TSL -> SCCP
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
			{noreply, State};
		{ok, {continue, #'Continue'{otid = Otid, dtid = Dtid} = Continue1}} ->
			% DTID assigned?
			OTID = decode_tid(Otid),
			DTID = decode_tid(Dtid),
			case maps:find(DTID, Map) of
				{ok, TSM} ->
					Continue2 = Continue1#'Continue'{otid = OTID,
							dtid = DTID},
					TsmParams = UdataParams#'N-UNITDATA'{userData = Continue2},
					gen_statem:cast(TSM, {'CONTINUE', received, TsmParams}),
					{noreply, State};
				error ->
% TODO Handle Continue with unknown transaction ID.
					% Build ABORT message with appropriate P-Abort Cause values
					% N-UNITDATA request TSL -> SCCP
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
					{noreply, State}
			end;
		{ok, {continue, _TPDU}} ->
			% OTID derivable?
			% DTID assigned?
			% Build ABORT message with appropraite P-Abort Cause value
			% N-UNITDATA request TSL -> SCCP
			% Local Abort TSM <- TCO
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
			{noreply, State};
		{ok, {'end', #'End'{dtid = Dtid} = End1}} ->
			% DTID assigned?
			DTID = decode_tid(Dtid),
			case maps:find(DTID, Map) of
				{ok, TSM} ->
					End2 = End1#'End'{dtid = DTID},
					TsmParams = UdataParams#'N-UNITDATA'{userData = End2},
					% END received TSM <- TCO
					gen_statem:cast(TSM, {'END', received, TsmParams}),
					{noreply, State};
				error ->
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					{noreply, State}
			end;
		{ok, {'end', _TPDU}} ->
			% DTID assigned?
			%    Local Abort TSM <- TCO
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
			{noreply, State};
		{ok, {abort, #'Abort'{dtid = Dtid} = TPDU}} ->
			% DTID assigned?
			DTID = decode_tid(Dtid),
			case maps:find(DTID, Map) of
				{ok, TSM} ->
					TsmParams = UdataParams#'N-UNITDATA'{userData = TPDU},
					% Abort received TSM <- TCO
					gen_statem:cast(TSM, {'ABORT', received, TsmParams}),
					{noreply, State};
				error ->
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					{noreply, State}
			end;
		{ok, {abort, _TPDU}} ->
			% DTID assigned?
			%    Local Abort TSM <- TCO
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
			{noreply, State};
		{ok, {_Tag, _TPDU}} ->
			% Message type unknown
			% OTID derivable?
			% DTID assigned?
			% Build ABORT message with appropraite P-Abort Cause value
			% N-UNITDATA request TSL -> SCCP
			% Local Abort TSM <- TCO
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
			{noreply, State};
		{error, _Reason} ->
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
			{noreply, State}
	end;
handle_cast({'N', 'NOTICE', indication,
		#'N-NOTICE'{userData = UserData, callingAddress = CallingAddress,
		calledAddress = CalledAddress, reason = Reason} = _NoticeParams},
		#state{tsm = Map} = State) ->
	TransactionID  = case 'TR':decode('TCMessage', UserData) of
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, #'Begin'{otid = OTID} = _Begin} ->
					decode_tid(OTID);
				_ ->
					undefined
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, #'Continue'{otid = OTID} = _Continue} ->
					decode_tid(OTID);
				_ ->
					undefined
			end;
		_ ->
			undefined
	end,
	case maps:find(TransactionID, Map) of
		{ok, TSM} ->
			% TR-NOTICE indication CSL <- TSL
			% reference: Figure A.3/Q.774 (sheet 2 of 4)
			TrParams = #'TR-NOTICE'{transactionID = TransactionID,
					origAddress = CallingAddress, destAddress = CalledAddress,
					reportCause = Reason},
			gen_statem:cast(TSM, {'NOTICE', received, TrParams}),
			{noreply, State};
		error ->
			% @todo: Handle unknown TID
			{noreply, State}
	end;
%%
%% service primitive requests from the TR-User
%% reference: Figure A.3/Q.774 (sheets 2&3 of 4)
handle_cast({'TR', 'UNI', request,
		#'TR-UNI'{qos = QoS, destAddress = DestAddress,
		origAddress = OrigAddress, userData = UserData} = _UniParams},
		#state{callback = Callback, ext_state = ExtState} = State) ->
	{SequenceControl, ReturnOption} = QoS,
	DialoguePortion = UserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = UserData#'TR-user-data'.componentPortion,
	case 'TR':encode('TCMessage',
			{unidirectional, #'Unidirectional'{dialoguePortion = DialoguePortion,
			components = ComponentPortion}}) of
		{ok, TPDU} ->
			SccpParams = #'N-UNITDATA'{userData = TPDU,
					calledAddress = DestAddress, callingAddress =  OrigAddress,
					sequenceControl = SequenceControl, returnOption = ReturnOption},
			Primitive = {'N', 'UNITDATA', request, SccpParams},
			CbArgs = [Primitive, ExtState],
			case tcap_tco_callback:cb(send_primitive, Callback, CbArgs) of
				{noreply, ExtState1} ->
					{noreply, State#state{ext_state = ExtState1}};
				{noreply, ExtState1, Timeout} ->
					{noreply, State#state{ext_state = ExtState1}, Timeout};
				{stop, Reason, ExtState1} ->
					{stop, Reason, State#state{ext_state = ExtState1}}
			end;
		{error, Reason} ->
			error_logger:error_report(["Error generating ASN1",
					{error, Reason}, {message_type, unidirectional},
					{dialogue_portion, DialoguePortion},
					{components, ComponentPortion}]),
			{noreply, State}
	end;
handle_cast({'TR', 'CONTINUE', request,
		#'TR-CONTINUE'{transactionID = TransactionID} = ContParams},
		#state{tsm = Map} = State) ->
	case maps:find(TransactionID, Map) of
		{ok, TSM} ->
			gen_statem:cast(TSM, {'CONTINUE', transaction, ContParams}),
			{noreply, State};
		error ->
			% @todo: Handle unknown TID
			{noreply, State}
	end;
handle_cast({'TR', 'END', request,
		#'TR-END'{transactionID = TransactionID} = EndParams},
		#state{tsm = Map} = State) ->
	case maps:find(TransactionID, Map) of
		{ok, TSM} ->
			gen_statem:cast(TSM, {'END', transaction, EndParams}),
			{noreply, State};
		error ->
			% @todo: Handle unknown TID
			{noreply, State}
	end;
handle_cast({'TR', 'U-ABORT', request,
		#'TR-U-ABORT'{transactionID = TransactionID} = AbortParams},
		#state{tsm = Map} = State) ->
	case maps:find(TransactionID, Map) of
		{ok, TSM} ->
			gen_statem:cast(TSM, {'ABORT', transaction, AbortParams}),
			{noreply, State};
		error ->
			% @todo: Handle unknown TID
			{noreply, State}
	end;
handle_cast({'N', 'UNITDATA', request, _} = Primitive,
		#state{callback = Callback, ext_state = ExtState} = State) ->
	CbArgs = [Primitive, ExtState],
	case tcap_tco_callback:cb(send_primitive, Callback, CbArgs) of
		{noreply, ExtState1} ->
			{noreply, State#state{ext_state = ExtState1}};
		{noreply, ExtState1, Timeout} ->
			{noreply, State#state{ext_state = ExtState1}, Timeout};
		{stop, Reason, ExtState1} ->
			{stop, Reason, State#state{ext_state = ExtState1}}
	end;
handle_cast(Request,
		#state{callback = Callback, ext_state = ExtState} = State) ->
	CbArgs = [Request, ExtState],
	case tcap_tco_callback:cb(handle_cast, Callback, CbArgs) of
		{noreply, ExtState1} ->
			{noreply, State#state{ext_state = ExtState1}};
		{noreply, ExtState1, Timeout} ->
			{noreply, State#state{ext_state = ExtState1}, Timeout};
		{primitive, Primitive, ExtState1} ->
			handle_cast(Primitive, State#state{ext_state = ExtState1});
		{stop, Reason, ExtState1} ->
			{stop, Reason, State#state{ext_state = ExtState1}}
	end.

-spec handle_continue(Info, State) -> Result
	when
		Info :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle continued execution.
handle_continue({?MODULE, Timeout}, #state{tsl_sup = TslSup} = State) ->
	Children = supervisor:which_children(TslSup),
	{_, TsmSup, _, _} = lists:keyfind(tcap_transaction_sup, 1, Children),
	NewState = State#state{tsm_sup = TsmSup},
	{noreply, NewState, Timeout};
handle_continue(Info, #state{callback = Callback} = State)
		when is_atom(Callback) ->
	case erlang:function_exported(Callback, handle_continue, 2) of
		true ->
			handle_continue1(Info, State);
		false ->
			{noreply, State}
	end;
handle_continue(Info, #state{callback = Callback} = State)
		when is_record(Callback, tcap_tco_cb) ->
	handle_continue1(Info, State).
%% @hidden
handle_continue1(Info,
		#state{callback = Callback, ext_state = ExtState} = State) ->
	CbArgs = [Info, ExtState],
	case tcap_tco_callback:cb(handle_continue, Callback, CbArgs) of
		{noreply, ExtState1} ->
			{noreply, State#state{ext_state = ExtState1}};
		{noreply, ExtState1, Timeout} ->
			{noreply, State#state{ext_state = ExtState1}, Timeout};
		{stop, Reason, ExtState1} ->
			{stop, Reason, State#state{ext_state = ExtState1}}
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
handle_info({'EXIT', _Pid, {shutdown, {tcap_tsm_fsm, TID}}},
		#state{tsm = Map} = State) ->
	{noreply, State#state{tsm = maps:remove(TID, Map)}};
handle_info({'EXIT', _Pid, {shutdown, _}} = Info, State) ->
	handle_info1(Info, State);
handle_info({'EXIT', Pid, _Reason} = Info, #state{tsm = Map} = State) ->
	F = fun F({TID, Value, _Iterator}) when Value == Pid ->
				{noreply, State#state{tsm = maps:remove(TID, Map)}};
			F({_, _, Iterator}) ->
				F(maps:next(Iterator));
			F(none) ->
				handle_info1(Info, State)
	end,
	F(maps:next(maps:iterator(Map)));
handle_info(Info, State) ->
	handle_info1(Info, State).
%% @hidden
handle_info1(Info, #state{callback = Callback} = State)
		when is_atom(Callback) ->
	case erlang:function_exported(Callback, handle_info, 2) of
		true ->
			handle_info2(Info, State);
		false ->
			{noreply, State}
	end;
handle_info1(Info, #state{callback = Callback} = State)
		when is_record(Callback, tcap_tco_cb) ->
	handle_info2(Info, State).
%% @hidden
handle_info2(Info,
		#state{callback = Callback, ext_state = ExtState} = State) ->
	CbArgs = [Info, ExtState],
	case tcap_tco_callback:cb(handle_info, Callback, CbArgs) of
		{noreply, ExtState1} ->
			{noreply, State#state{ext_state = ExtState1}};
		{noreply, ExtState1, Timeout} ->
			{noreply, State#state{ext_state = ExtState1}, Timeout};
		{primitive, Primitive, ExtState1} ->
			handle_cast(Primitive, State#state{ext_state = ExtState1});
		{stop, Reason, ExtState1} ->
			{stop, Reason, State#state{ext_state = ExtState1}}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: state().
%% @see //stdlib/gen_server:terminate/3
%% @private
terminate(Reason, #state{callback = Callback} = State)
		when is_atom(Callback) ->
	case erlang:function_exported(Callback, terminate, 2) of
		true ->
			terminate1(Reason, State);
		false ->
			ok
	end;
terminate(Reason, #state{callback = Callback} = State)
		when is_record(Callback, tcap_tco_cb) ->
	terminate1(Reason, State).
%% @hidden
terminate1(Reason,
		#state{callback = Callback, ext_state = ExtState} = _State) ->
	CbArgs = [Reason, ExtState],
	tcap_tco_callback:cb(terminate, Callback, CbArgs).

-spec code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
%% @see //stdlib/gen_server:code_change/3
%% @private
code_change(OldVersion, #state{callback = Callback} = State, Extra)
		when is_atom(Callback) ->
	case erlang:function_exported(Callback, code_change, 3) of
		true ->
			code_change1(OldVersion, State, Extra);
		false ->
			{ok, State}
	end;
code_change(OldVersion, #state{callback = Callback} = State, Extra)
		when is_record(Callback, tcap_tco_cb) ->
	code_change1(OldVersion, State, Extra).
%% @hidden
code_change1(OldVersion,
		#state{callback = Callback, ext_state = ExtState} = State, Extra) ->
	CbArgs = [OldVersion, ExtState, Extra],
	case tcap_tco_callback:cb(terminate, Callback, CbArgs) of
		{ok, ExtState} ->
			{ok, State#state{ext_state = ExtState}};
		{error, Reason} ->
			{error, Reason}
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
format_status(Opt,
		[_PDict, #state{callback = Callback} = State] = StatusData)
		when is_atom(Callback) ->
	case erlang:function_exported(Callback, format_status, 2) of
		true ->
			format_status1(Opt, StatusData);
		false ->
			case Opt of
				terminate ->
					State;
				_ ->
					[{data, [{"State", State}]}]
			end
	end;
format_status(Opt,
		[_PDict, #state{callback = Callback} = _State] = StatusData)
		when is_record(Callback, tcap_tco_cb) ->
	format_status1(Opt, StatusData).
%% @hidden
format_status1(Opt, [PDict, #state{callback = Callback,
		ext_state = ExtState} = _State] = _StatusData) ->
	CbArgs = [Opt, [PDict, ExtState]],
	tcap_tco_callback:cb(format_status, Callback, CbArgs).

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


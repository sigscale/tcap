%%% tcap_cco_server.erl
%%%---------------------------------------------------------------------
%%% @copyright 2010-2011 Harald Welte
%%% 		2021 SigScale Global Inc.
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
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
%%% @doc TCAP Component Coordinator (CCO) functional block within the
%%% 		component sub-layer of ITU TCAP.
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
-module(tcap_cco_server).
-copyright('Copyright (c) 2010-2011 Harald Welte').
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Harald Welte <laforge@gnumonks.org>').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

-include("tcap.hrl").
-include("TR.hrl").
-include("TC.hrl").

-record(state,
		{cco_sup :: pid(),
		dha_sup :: pid(),
		usap :: pid(),
		did :: 0..4294967295,
		components = [] :: [{Primitive :: tuple(), ASN :: tuple()}],
		dha :: pid(),
		ism_sup :: pid(),
		ism = #{} :: #{InvokeId :: -128..127 := (ISM :: pid())}}).
-type state() :: #state{}.

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
init([CcoSup, DhaSup, _TCO, TCU]) ->
	process_flag(trap_exit, true),
	{ok, #state{cco_sup = CcoSup, dha_sup = DhaSup,
			usap = TCU}, {continue, init}}.

-spec handle_continue(Continue, State) -> Result
	when
		Continue :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @see //stdlib/gen_server:handle_continue/2
%% @private
handle_continue(init, #state{dha_sup = DhaSup,
		cco_sup = CcoSup} = State) ->
	Children1 = supervisor:which_children(CcoSup),
	{_, IsmSup, _, _} = lists:keyfind(tcap_invocation_sup, 1, Children1),
	Children2 = supervisor:which_children(DhaSup),
	{_, DHA, _, _} = lists:keyfind(tcap_dha_fsm, 1, Children2),
	{noreply, State#state{ism_sup = IsmSup, dha = DHA}}.

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
handle_call(Request, _From, State) ->
	{stop, Request, State}.

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%%
%% @@see //stdlib/gen_server:handle_cast/2
%% @private
%% @end
% from TCU: TC-INVOKE.req
handle_cast({'TC','INVOKE',request,
		#'TC-INVOKE'{invokeID = InvokeId, linkedID = LinkedId,
		operation = Operation, parameters = Parameters} = InvokeParam},
		#state{components = Components} = State) ->
	Invoke = #'Invoke'{invokeId = invoke_id(InvokeId),
			linkedId = invoke_id(LinkedId), opcode = Operation,
			argument = argument(Parameters)},
	% assemble INVOKE component
	Component = {InvokeParam, {invoke, Invoke}},
	% mark it as available
	NewState = State#state{components = [Component | Components]},
	% what to do with class and timeout?
	{noreply, NewState};
% from TCU: TC-U-CANCEL.req
handle_cast({'TC','U-CANCEL',request,
		#'TC-U-CANCEL'{invokeID = InvokeId} = _Param},
		#state{components = Components, ism = ISMs} = State) ->
	% if there are any INV componnents waiting, discard them
	F = fun F([{#'TC-INVOKE'{invokeID = ID}, _} | T], Acc)
					when ID =:= InvokeId ->
				NewComponents = lists:reverse(Acc) ++ T,
				{noreply, State#state{components = NewComponents}};
			F([H | T], Acc) ->
				F(T, [H | Acc]);
			F([], _Acc) ->
				% if not, check any active ISM, if yes, terminate ISM
				case maps:take(InvokeId, ISMs) of
					{ISM, NewISMs} ->
						gen_statem:cast(ISM, terminate),
						{noreply, State#state{ism = NewISMs}};
					error ->
						{stop, ism_not_found, State}
				end
	end,
	F(Components, []);
% from TCL -> CHA (CCO): TC-RESULT-{L,NL}, U-ERROR
handle_cast({'TC', 'RESULT-L', request,
		#'TC-RESULT-L'{invokeID = InvokeId,
		operation = Operation, parameters = Parameters} = Param},
		#state{components = Components} = State) ->
	ReturnResultResult = #'ReturnResult_result'{opcode = Operation,
					result = argument(Parameters)},
	ReturnResult = #'ReturnResult'{invokeId = invoke_id(InvokeId),
			result = ReturnResultResult},
	Component = {Param, {returnResult, ReturnResult}},
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
handle_cast({'TC', 'RESULT-NL', request,
		#'TC-RESULT-NL'{invokeID = InvokeId,
		operation = Operation, parameters = Parameters} = Param},
		#state{components = Components} = State) ->
	ReturnResultResult = #'ReturnResult_result'{opcode = Operation,
					result = argument(Parameters)},
	ReturnResult = #'ReturnResult'{invokeId = invoke_id(InvokeId),
			result = ReturnResultResult},
	Component = {Param, {returnResultNotLast, ReturnResult}},
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
handle_cast({'TC', 'U-ERROR', request,
		#'TC-U-ERROR'{invokeID = InvokeId,
		error = Error, parameters = Parameters} = Param},
		#state{components = Components} = State) ->
	ReturnError = #'ReturnError'{invokeId = invoke_id(InvokeId),
			errcode = Error, parameter = argument(Parameters)},
	% Figure A.6/Q.774 (1 of 4)
	% assemble requested component
	Component = {Param, {returnError, ReturnError}},
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
% TCL->CHA: TC-U-REJECT.req
handle_cast({'TC','U-REJECT',request,
		#'TC-U-REJECT'{invokeID = InvokeId, problemCode = Problem} = Param},
		#state{components = Components} = State) ->
	Reject = #'Reject'{invokeId = invoke_id(InvokeId), problem = Problem},
	% assemble reject component
	Component = {Param, {reject, Reject}}, %FIXME
	% FIXME: if problem type Result/Error, terminate ISM
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
% DHA -> CHA (CCO): Components received
handle_cast({components, Components},
		#state{usap = USAP, did = DID, ism = ISMs} = State) ->
	% Figure A.6/Q.774 (2 of 4)
	Flast = fun([]) -> true; (_) -> false end,
	F = fun F([{invoke, #'Invoke'{invokeId = InvokeId,
					linkedId = asn1_NOVALUE, opcode = Operation,
					argument = Argument}} | T]) ->
				% Linked operation? (no)
				Invoke = #'TC-INVOKE'{dialogueID = DID,
						invokeID = invoke_id(InvokeId),
						operation = Operation,
						parameters = argument(Argument),
						lastComponent = Flast(T)},
				gen_statem:cast(USAP,
						{'TC', 'INVOKE', indication, Invoke}),
				F(T);
			F([{invoke, #'Invoke'{invokeId = InvokeId,
					linkedId = {present, LinkedId}, opcode = Operation,
					argument = Argument}} | T]) ->
				% Linked operation? (yes)
				ISM = maps:get(LinkedId, ISMs),
				case sys:get_state(ISM) of
					{StateName, _Data} when
							% Linked ISM in operation sent state? (yes)
							StateName == sent_class_1;
							StateName == sent_class_2;
							StateName == sent_class_3;
							StateName == sent_class_4 ->
						Invoke = #'TC-INVOKE'{dialogueID = DID,
								invokeID = invoke_id(InvokeId),
								linkedID = LinkedId,
								operation = Operation,
								parameters = argument(Argument),
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'INVOKE', indication, Invoke}),
						F(T);
					{_, _Data} ->
						% Linked ISM in operation sent state? (no)
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = invoke_id(InvokeId),
								problemCode = {invoke, unrecognizedLinkedId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject}),
						F(T)
				end;
			F([{returnResultNotLast,
					#'ReturnResult'{invokeId = {present, InvokeId},
					result = #'ReturnResult_result'{opcode = Operation,
					result = ReturnResultResult}}} | T]) ->
				% Is ISM active?
				case maps:find(InvokeId, ISMs) of
					{ok, ISM} ->
						ReturnResult = #'TC-RESULT-NL'{dialogueID = DID,
								invokeID = InvokeId,
								operation = Operation,
								parameters = ReturnResultResult,
								lastComponent = Flast(T)},
						gen_statem:cast(ISM, ReturnResult);
					error ->
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = {invoke, unrecognizedInvokeId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject})
				end,
				F(T);
			F([{returnResultNotLast,
					#'ReturnResult'{invokeId = {present, InvokeId},
					result = asn1_NOVALUE}} | T]) ->
				% Is ISM active?
				case maps:find(InvokeId, ISMs) of
					{ok, ISM} ->
						ReturnResult = #'TC-RESULT-NL'{dialogueID = DID,
								invokeID = InvokeId,
								lastComponent = Flast(T)},
						gen_statem:cast(ISM, ReturnResult);
					error ->
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = {invoke, unrecognizedInvokeId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject})
				end,
				F(T);
			F([{returnResult,
					#'ReturnResult'{invokeId = {present, InvokeId},
					result = asn1_NOVALUE}} | T]) ->
				% Is ISM active?
				case maps:find(InvokeId, ISMs) of
					{ok, ISM} ->
						ReturnResult = #'TC-RESULT-L'{dialogueID = DID,
								invokeID = InvokeId,
								lastComponent = Flast(T)},
						gen_statem:cast(ISM, ReturnResult);
					error ->
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = {invoke, unrecognizedInvokeId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject})
				end,
				F(T);
			F([{returnResult,
					#'ReturnResult'{invokeId = {present, InvokeId},
					result = #'ReturnResult_result'{opcode = Operation,
					result = ReturnResultResult}}} | T]) ->
				% Is ISM active?
				case maps:find(InvokeId, ISMs) of
					{ok, ISM} ->
						ReturnResult = #'TC-RESULT-L'{dialogueID = DID,
								invokeID = InvokeId,
								operation = Operation,
								parameters = ReturnResultResult,
								lastComponent = Flast(T)},
						gen_statem:cast(ISM, ReturnResult);
					error ->
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = {invoke, unrecognizedInvokeId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject})
				end,
				F(T);
			F([{returnError,
					#'ReturnError'{invokeId = {present, InvokeId},
					errcode = ErrorCode, parameter = Parameter}} | T]) ->
				% Is ISM active?
				case maps:find(InvokeId, ISMs) of
					{ok, ISM} ->
						ReturnError = #'TC-U-ERROR'{dialogueID = DID,
								invokeID = InvokeId,
								error = ErrorCode,
								parameters = argument(Parameter),
								lastComponent = Flast(T)},
						gen_statem:cast(ISM, ReturnError);
					error ->
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = {invoke, unrecognizedInvokeId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject})
				end,
				F(T);
			F([{reject, #'Reject'{invokeId = {present, InvokeId},
					problem = Problem}} | T]) ->
				% Is ISM active?
				case maps:find(InvokeId, ISMs) of
					{ok, ISM} ->
						Reject = #'TC-U-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = Problem,
								lastComponent = Flast(T)},
						gen_statem:cast(ISM, Reject);
					error ->
						% Assemble REJECT component
						Reject = #'TC-L-REJECT'{dialogueID = DID,
								invokeID = InvokeId,
								problemCode = {invoke, unrecognizedInvokeId},
								lastComponent = Flast(T)},
						gen_statem:cast(USAP,
								{'TC', 'L-REJECT', indication, Reject})
				end,
				F(T);
			F([]) ->
				{noreply, State}
	end,
	F(Components);
% ISM -> CCO: Generate REJ component
handle_cast({reject_component, Reject},
		#state{components = Components} = State) ->
	Component = {Reject, 0}, %FIXME
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
% DHA -> CHA (CCO)
% Figure A.6/Q.774 (4 of 4)
handle_cast('request-components', State1) ->
	% for each component
	F = fun F(#state{components = [{#'TC-INVOKE'{class = Class,
					timeout = Timeout, invokeID = InvokeId}, ASN} | T],
					ism_sup = Sup, ism = ISMs, usap = USAP,
					did = DID} = State2, Acc) ->
				% if INVOKE component
				% start ISM and store ISM
				StartArgs = [[USAP, DID, InvokeId, self(), Class, Timeout], []],
				case supervisor:start_child(Sup, StartArgs) of
					{ok, ISM} ->
						% signal 'operation-sent' to ISM
						gen_statem:cast(ISM, 'operation-sent'),
						NewISMs = ISMs#{InvokeId => ISM},
						NewState = State2#state{components = T, ism = NewISMs},
						F(NewState, [ASN | Acc]);
					{error, Reason} ->
						{stop, Reason, State2}
				end;
			F(#state{components = [{_, ASN} | T]} = State2, Acc) ->
				F(State2#state{components = T}, [ASN | Acc]);
			F(#state{components = [], dha = DHA} = State2, Acc) ->
				% signal 'requested-components' to DHA
				gen_statem:cast(DHA, {'requested-components', Acc}),
				NewState = State2#state{components = []},
				{noreply, NewState}
	end,
	F(State1, []);
% from DHA -> CCO: dialogue-terminated
handle_cast('dialogue-terminated', State) ->
	% discard components awaiting transmission
	{stop, normal, State}.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a received message.
%%
%% @@see //stdlib/gen_server:handle_info/2
%% @private
handle_info({'EXIT', _Pid, Reason}, State) ->
	{stop, Reason, State}.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state().
%% @see //stdlib/gen_server:terminate/3
%% @private
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
%% @see //stdlib/gen_server:code_change/3
%% @private
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

-spec invoke_id(InvokeId) -> InvokeId
	when
		InvokeId :: integer() | undefined | asn1_NOVALUE | {present, integer()}.
%% @doc Invoke ID CODEC.
%% @private
invoke_id(undefined = _InvokeId) ->
	asn1_NOVALUE;
invoke_id(InvokeId) when is_integer(InvokeId) ->
	{present, InvokeId};
invoke_id({present, InvokeId}) when is_integer(InvokeId) ->
	InvokeId;
invoke_id(asn1_NOVALUE) ->
	undefined.

-spec argument(Argument) -> Argument
	when
		Argument :: asn1_NOVALUE | undefined | binary().
%% @doc Argument CODEC.
%% @private
argument(asn1_NOVALUE = _Argument) ->
	undefined;
argument(undefined = _Argument) ->
	asn1_NOVALUE;
argument(Argument) when is_binary(Argument) ->
	Argument.


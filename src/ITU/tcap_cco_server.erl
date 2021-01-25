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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("tcap.hrl").
-include("TR.hrl").
-include("TC.hrl").

-record(component, {asn_ber, user_prim}).
-record(state,
		{usap :: pid(),
		dialogueID :: 0..4294967295,
		components = [] :: [#component{}],
		dha :: pid(),
		ism = []}).
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
init([_TCO, TCU]) ->
	process_flag(trap_exit, true),
	{ok, #state{usap = TCU}}.

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
			linkedId = invoke_id(LinkedId), opcode = operation(Operation),
			argument = argument(Parameters)},
	% assemble INVOKE component
	Component = #component{user_prim = InvokeParam,
			asn_ber = {invoke, Invoke}},
	% mark it as available
	NewState = State#state{components = [Component | Components]},
	% what to do with class and timeout?
	{noreply, NewState};
% from TCU: TC-U-CANCEL.req
handle_cast({'TC','U-CANCEL',request,
		#'TC-U-CANCEL'{invokeID = InvokeId} = _Param},
		#state{components = Components, ism = ISM} = State) ->
	% if there are any INV componnents waiting, discard them
	case discard_inv_component(Components, InvokeId) of
		Components ->
			% if not, check any active ISM, if yes, terminate ISM
			NewISMs = terminate_active_ism(ISM, InvokeId),
			{noreply, State#state{ism = NewISMs}};
		NewComponents ->
			{noreply, State#state{components = NewComponents}}
	end;
% from TCL -> CHA (CCO): TC-RESULT-{L,NL}, U-ERROR
handle_cast({'TC', 'RESULT-L', request,
		#'TC-RESULT-L'{invokeID = InvokeId,
		operation = Operation, parameters = Parameters} = Param},
		#state{components = Components} = State) ->
	ReturnResultResult = #'ReturnResult_result'{opcode = operation(Operation),
					result = argument(Parameters)},
	ReturnResult = #'ReturnResult'{invokeId = invoke_id(InvokeId),
			result = ReturnResultResult},
	Component = #component{user_prim = Param,
			asn_ber = {returnResult, ReturnResult}},
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
handle_cast({'TC', 'RESULT-NL', request,
		#'TC-RESULT-NL'{invokeID = InvokeId,
		operation = Operation, parameters = Parameters} = Param},
		#state{components = Components} = State) ->
	ReturnResultResult = #'ReturnResult_result'{opcode = operation(Operation),
					result = argument(Parameters)},
	ReturnResult = #'ReturnResult'{invokeId = invoke_id(InvokeId),
			result = ReturnResultResult},
	Component = #component{user_prim = Param,
			asn_ber = {returnResultNotLast, ReturnResult}},
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
handle_cast({'TC', 'U-ERROR', request,
		#'TC-U-ERROR'{invokeID = InvokeId,
		error = Error, parameters = Parameters} = Param},
		#state{components = Components} = State) ->
	ReturnError = #'ReturnError'{invokeId = invoke_id(InvokeId),
			errcode = error(Error), parameter = argument(Parameters)},
	% Figure A.6/Q.774 (1 of 4)
	% assemble requested component
	Component = #component{user_prim = Param,
			asn_ber = {returnError, ReturnError}},
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
% TCL->CHA: TC-U-REJECT.req
handle_cast({'TC','U-REJECT',request,
		#'TC-U-REJECT'{invokeID = InvokeId, problemCode = Problem} = Param},
		#state{components = Components} = State) ->
	Reject = #'Reject'{invokeId = InvokeId, problem = Problem},
	% assemble reject component
	Component = #component{user_prim = Param,
			asn_ber = {reject, Reject}}, %FIXME
	% FIXME: if probelm type Result/Error, terminate ISM
	% mark component available for this dialogue
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
% DHA -> CHA (CCO): Components received
handle_cast({components, Components}, State) ->
	% Figure A.6/Q.774 (2 of 4)
	process_rx_components(State#state.ism, State#state.usap,
			      State#state.dialogueID, Components),
	{noreply, State};
% ISM -> CCO: Generate REJ component
handle_cast({reject_component, Reject},
		#state{components = Components} = State) ->
	Component = #component{user_prim = Reject, asn_ber = 0}, %FIXME
	NewState = State#state{components = [Component | Components]},
	{noreply, NewState};
% DHA -> CHA (CCO)
% Figure A.6/Q.774 (4 of 4)
handle_cast('request-components',
		#state{dha = DHA, components = []} = State) ->
	% if no components, signal 'no-components' to DHA
	gen_statem:cast(DHA, 'no-component'),
	{noreply, State};
handle_cast('request-components',
		#state{dha = DHA, components = Components1} = State) ->
	% for each component
	{Components2, ISMs} = process_request_components(Components1, State),
	% signal 'requested-components' to DHA
	gen_statem:cast(DHA, {'requested-components', Components2}),
	NewState = State#state{ism = State#state.ism ++ ISMs, components = []},
	{noreply, NewState};
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

-spec operation(Operation) -> Operation
	when
		Operation :: {local, integer()} | {global, tuple()} | integer() | tuple().
%% @doc Operation CODEC.
%% @private
operation({local, Operation}) when is_integer(Operation) ->
	Operation;
operation({global, Operation}) when is_tuple(Operation) ->
	Operation;
operation(Operation) when is_integer(Operation) ->
	{local, Operation};
operation(Operation) when is_tuple(Operation) ->
	{global, Operation}.

-spec argument(Argument) -> Argument
	when
		Argument :: asn1_NOVALUE | undefined.
%% @doc Argument CODEC.
%% @private
argument(asn1_NOVALUE = _Argument) ->
	undefined;
argument(undefined = _Argument) ->
	asn1_NOVALUE;
argument(Argument) when is_list(Argument) ->
	Argument.

%% @hidden
% Figure A.6/Q.774 (4 of 4)
process_request_components(Components, #state{} = State)
		when is_list(Components) ->
	process_request_components(Components, State, [], []).
%% @hidden
process_request_components([], _State, Acc, ISMs) ->
	{lists:reverse(Acc), ISMs};
process_request_components([#component{asn_ber = ASN,
		user_prim = #'TC-INVOKE'{class = Class, timeout = Timeout,
		invokeID = InvokeId}} | T], #state{ism = ISM, usap = USAP,
		dialogueID = DID} = State, Acc, ISMs) ->
	% if INVOKE component
	% start ISM and store ISM
	{ok, ISM} = tcap_invocation_sup:start_ism(USAP,
			DID, InvokeId, self(), Class, Timeout),
	% signal 'operation-sent' to ISM
	gen_statem:cast(ISM, 'operation-sent'),
	NewISMs = [{InvokeId, ISM} | ISMs],
	process_request_components(T, State, [ASN | Acc], NewISMs);
process_request_components([#component{asn_ber = ASN} | T], State, Acc, ISMs) ->
	process_request_components(T, State, [ASN | Acc], ISMs).

%% @hidden
% discard components of type INVOKE for matching InvokeId
discard_inv_component(Components, InvokeId) when is_list(Components) ->
	discard_inv_component(Components, InvokeId, []).
discard_inv_component([], _InvokeId, Acc) ->
	lists:reverse(Acc);
discard_inv_component([#component{user_prim
		= #'TC-INVOKE'{invokeID = InvokeId}} | T], InvokeId, Acc) ->
	discard_inv_component(T, InvokeId, Acc);
discard_inv_component([H | T], InvokeId, Acc) ->
	discard_inv_component(T, InvokeId, [H | Acc]).

%% @hidden
% iterate over list of ISMs, terminate the one with matching InvokeId
terminate_active_ism(ISMs, InvokeId) ->
	case lists:keytake(InvokeId, 1, ISMs) of
		{value, {InvokeId, ISM}, NewISMs} ->
			gen_statem:cast(ISM, terminate),
			NewISMs;
		false ->
			ISMs
	end.

% Convert from asn1ct-generated record to the primitive records
asn_rec_to_uprim({invoke, AsnRec}, DlgId, Last) when is_record(AsnRec, 'Invoke') ->
	#'TC-INVOKE'{dialogueID = DlgId,
		     invokeID = invoke_id(AsnRec#'Invoke'.invokeId),
		     linkedID = invoke_id(AsnRec#'Invoke'.linkedId),
		     operation = AsnRec#'Invoke'.opcode,
		     parameters = AsnRec#'Invoke'.argument,
		     lastComponent = Last};
asn_rec_to_uprim({returnResultNotLast, AsnRec}, DlgId, Last) when is_record(AsnRec, 'ReturnResult') ->
	case AsnRec#'ReturnResult'.result of
		#'ReturnResult_result'{opcode = Op, result = Result} ->
			ok;
		asn1_NOVALUE ->
			Op = undefined,
			Result = undefined
	end,
	#'TC-RESULT-NL'{dialogueID = DlgId,
			invokeID = invoke_id(AsnRec#'ReturnResult'.invokeId),
			operation = Op,
			parameters = Result,
			lastComponent = Last};
asn_rec_to_uprim({returnResult, AsnRec}, DlgId, Last) when is_record(AsnRec, 'ReturnResult') ->
	case AsnRec#'ReturnResult'.result of
		#'ReturnResult_result'{opcode = Op, result = Result} ->
			ok;
		asn1_NOVALUE ->
			Op = undefined,
			Result = undefined
	end,
	#'TC-RESULT-L'{dialogueID = DlgId,
			invokeID = invoke_id(AsnRec#'ReturnResult'.invokeId),
			operation = Op,
			parameters = Result,
			lastComponent = Last};
asn_rec_to_uprim({returnError, AsnRec}, DlgId, Last) when is_record(AsnRec, 'ReturnError') ->
	#'TC-U-ERROR'{dialogueID = DlgId,
		      invokeID = invoke_id(AsnRec#'ReturnError'.invokeId),
		      error = AsnRec#'ReturnError'.errcode,
		      parameters = AsnRec#'ReturnError'.parameter,
		      lastComponent = Last};
asn_rec_to_uprim({reject, AsnRec}, DlgId, Last) when is_record(AsnRec, 'Reject') ->
	#'TC-U-REJECT'{dialogueID = DlgId,
			invokeID = invoke_id(AsnRec#'Reject'.invokeId),
			problemCode = AsnRec#'Reject'.problem,
			lastComponent = Last}.

process_rx_components(ISMs, Usap, DlgId, [Head|[]]) ->
	process_rx_component(ISMs, Usap, DlgId, Head, true),
	ok;
process_rx_components(ISMs, Usap, DlgId, [Head|Tail]) ->
	process_rx_component(ISMs, Usap, DlgId, Head, false),
	process_rx_components(ISMs, Usap, DlgId, Tail).

process_rx_component(ISMs, Usap, DlgId, C={invoke, #'Invoke'{}}, Last) ->
	InvokeId = get_invoke_id_from_comp(C),
	{invoke, I} = C,
	case I#'Invoke'.linkedId of
	    asn1_NOVALUE ->
		ok;
	    Linked ->
		% check if Linked ISM is in operation sent state
		% FIXME
		ok
	end,
	Prim = asn_rec_to_uprim(C, DlgId, Last),
	gen_statem:cast(Usap, {'TC','INVOKE',indication,Prim});
process_rx_component(ISMs, _Usap, DlgId, C={reject, #'Reject'{problem=Problem}}, Last) ->
	InvokeId = get_invoke_id_from_comp(C),
	ISM = lists:keyfind(InvokeId, 1, ISMs),
	case Problem of
	    {invoke, _} ->
		% FIXME: ISM active (No -> Inform TC-User)
		gen_statem:cast(ISM, terminate);
	    _ ->
		ok
	end,
	% FIXME: decide on TC-U-REJECT or TC-R-REJECT
	Prim = asn_rec_to_uprim(C, DlgId, Last),
	{InvokeId, ISM} = lists:keyfind(InvokeId, 1, ISMs),
	gen_statem:cast(ISM, Prim);
process_rx_component(ISMs, _Usap, DlgId, Comp, Last) ->
	% syntax error?
	InvokeId = get_invoke_id_from_comp(Comp),
	{InvokeId, ISM} = lists:keyfind(InvokeId, 1, ISMs),
	% FIXME: ISM active (No -> 6)
	Prim = asn_rec_to_uprim(Comp, DlgId, Last),
	gen_statem:cast(ISM, Prim).

% get the invokeId from the given asn-record component tuple.
get_invoke_id_from_comp({invoke,
			 #'Invoke'{invokeId = InvokeId}}) ->
	invoke_id(InvokeId);
get_invoke_id_from_comp({returnResult,
			 #'ReturnResult'{invokeId = InvokeId}}) ->
	invoke_id(InvokeId);
get_invoke_id_from_comp({returnResultNotLast,
			 #'ReturnResult'{invokeId = InvokeId}}) ->
	invoke_id(InvokeId);
get_invoke_id_from_comp({returnError,
			 #'ReturnError'{invokeId = InvokeId}}) ->
	invoke_id(InvokeId);
get_invoke_id_from_comp({reject,
			 #'Reject'{invokeId = InvokeId}}) ->
	invoke_id(InvokeId).

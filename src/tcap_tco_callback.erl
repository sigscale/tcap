%%% tcap_tco_callback.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2015-2021 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements a default callback for the
%%%		{@link //tcap/tcap_tco_server. tcap_tco_server} behaviour.
%%%
%%% 	When starting a TCO {@link //tacp/tcap_tco_server. tcap_tco_server}
%%% 	with {@link //tcap/tcap:start_tsl/4. tcap:start_tsl/4}
%%% 	the `Callback' may be provided as a `#tcap_tco_cb{}' record
%%% 	definition of callback `fun()'. This module implements the default
%%% 	callback functions used when not defined in the record.
%%%
%%% 	Extra arguments for callback `fun()' may be defined in
%%% 	`#tcap_tco_cb.extra'.
%%%
-module(tcap_tco_callback).
-copyright('Copyright (c) 2015-2021 SigScale Global Inc.').

%% export the tcap_tco_callback public API
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, handle_continue/2,
		send_primitive/2, start_aei/2,
		code_change/3, format_status/2]).

%% export the tcap_tco_callback private API
-export([cb/3]).

-include("DialoguePDUs.hrl").
-include("tcap.hrl").
-include_lib("sccp/include/sccp_primitive.hrl").

-type state() :: [].

%%----------------------------------------------------------------------
%%  The tcap_tco_callback public API
%%----------------------------------------------------------------------

-spec send_primitive(Primitive, State) -> Result
	when
		Primitive :: {'N', 'UNITDATA', request, UdataParams},
		UdataParams :: #'N-UNITDATA'{},
		State :: state(),
		Result :: {noreply, NewState}
				| {noreply, NewState, Timeout}
				| {noreply, NewState, hibernate}
				| {noreply, NewState, {continue, Continue}}
				| {stop, Reason, NewState},
		NewState :: state(),
		Timeout :: non_neg_integer() | infinity,
		Continue :: term(),
		Reason :: term().
%% @see //tcap/tcap_tco_server:send_primitive/2
send_primitive(_Primitive, State) ->
	{noreply, State}.

-spec start_aei(DialoguePortion, State) -> Result
	when
		DialoguePortion:: binary(),
		State :: term(),
		Result :: {ok, DHA, CCO, TCU, State} | {error, Reason},
		DHA :: pid(),
		CCO :: pid(),
		TCU :: pid(),
		Reason :: term().
%% @see //tcap/tcap_tco_server:start_aei/2
start_aei(_DialoguePortion, _State) ->
	{error, not_implemented}.

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: state()}
				| {ok, State :: state(), Timeout :: timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term()} | ignore.
%% @see //tcap/tcap_tco_server:init/1
init(_Args) ->
	{ok, []}.

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
%% @see //tcap/tcap_tco_server:handle_call/3
handle_call(_Request, _From, State) ->
	{noreply, State}.

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
%% @see //tcap/tcap_tco_server:handle_cast/2.
handle_cast(_Request, State) ->
	{noreply, State}.

-spec handle_continue(Info, State) -> Result
	when
		Info :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @see //tcap/tcap_tco_server:handle_continue/2
handle_continue(_Info, State) ->
	{noreply, State}.

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
%% @see //tcap/tcap_tco_server:handle_info/2
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: state().
%% @see //tcap/tcap_tco_server:terminate/2
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
%% @see //tcap/tcap_tco_server:code_change/3
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

-spec format_status(Opt, StatusData) -> Status
	when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().
%% @see //tcap/tcap_tco_server:format_status/2
format_status(terminate = _Opt, [_PDict, State] = _StatusData) ->
	State;
format_status(_Opt, [_PDict, State] = _StatusData) ->
	[{data, [{"State", State}]}].

%%----------------------------------------------------------------------
%%  The tcap_tco_callback private API
%%----------------------------------------------------------------------

-spec cb(Handler, Cb, Args) -> Result
	when
		Handler :: atom(),
		Cb :: atom() | #tcap_tco_cb{},
		Args :: [term()],
		Result :: term().
%% @private
cb(Handler, Cb, Args) when is_atom(Cb) ->
	apply(Cb, Handler, Args);
cb(init, #tcap_tco_cb{init = false}, Args) ->
	apply(?MODULE, init, Args);
cb(init, #tcap_tco_cb{init = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(handle_call, #tcap_tco_cb{init = false}, Args) ->
	apply(?MODULE, handle_call, Args);
cb(handle_call, #tcap_tco_cb{handle_call= F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(handle_cast, #tcap_tco_cb{handle_cast = false}, Args) ->
	apply(?MODULE, handle_cast, Args);
cb(handle_cast, #tcap_tco_cb{handle_cast = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(handle_info, #tcap_tco_cb{handle_info = false}, Args) ->
	apply(?MODULE, handle_info, Args);
cb(handle_info, #tcap_tco_cb{handle_info = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(terminate, #tcap_tco_cb{terminate = false}, Args) ->
	apply(?MODULE, terminate, Args);
cb(terminate, #tcap_tco_cb{terminate = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(handle_continue, #tcap_tco_cb{handle_continue = false}, Args) ->
	apply(?MODULE, handle_continue, Args);
cb(send_primitive, #tcap_tco_cb{send_primitive = false}, Args) ->
	apply(?MODULE, send_primitive, Args);
cb(send_primitive, #tcap_tco_cb{send_primitive = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(start_aei, #tcap_tco_cb{start_aei = false}, Args) ->
	apply(?MODULE, start_aei, Args);
cb(start_aei, #tcap_tco_cb{start_aei = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(handle_continue, #tcap_tco_cb{handle_continue = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(code_change, #tcap_tco_cb{code_change = false}, Args) ->
	apply(?MODULE, code_change, Args);
cb(code_change, #tcap_tco_cb{code_change = F, extra = E}, Args) ->
	apply(F, Args ++ E);
cb(format_status, #tcap_tco_cb{format_status = false}, Args) ->
	apply(?MODULE, format_status, Args);
cb(format_status, #tcap_tco_cb{format_status = F, extra = E}, Args) ->
	apply(F, Args ++ E).


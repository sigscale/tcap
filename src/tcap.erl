%%% tcap.erl
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom Inc.,
%%% 		2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
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
%%% @doc Transaction Capabilities Application Part
%%%	<p>This module implements the user API to the TCAP protocol
%%% 	stack application. Transaction Capabilities are ...</p>
%%%
%%% @reference <a href="index.html">TCAP User's Guide</a>
%%%
-module(tcap).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% our published API functions
-export([start/0, stop/0, start_tsl/4, stop_tsl/1]).
-export([open/2, close/1]).

-type tcap_options() :: [Option :: {variant, Variant :: itu | ansi}].
-export_type([tcap_options/0]).

-include("DialoguePDUs.hrl").
-include("tcap.hrl").

%%----------------------------------------------------------------------
%%  The tcap public API
%%----------------------------------------------------------------------

-spec start() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv application:start(tcap)
%%
start() ->
	application:start(tcap).

-spec stop() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv application:stop(tcap)
%%
stop() ->
	application:stop(tcap).

-spec start_tsl(Name, Callback, Args, Opts) -> Result
	when
		Name :: {local, LocalName} | {global, GlobalName}
				| {via, ViaModule, ViaName},
		LocalName :: atom(),
		GlobalName :: term(),
		ViaModule :: atom(),
		ViaName :: term(),
		Callback :: atom() | #tcap_tco_cb{},
		Args :: [term()],
		Opts :: [Option],
		Option :: {timeout, Timeout} | {debug, [Flag]},
		Timeout :: pos_integer(),
		Flag :: trace | log | {logfile, file:filename()}
				| statistics | debug,
		Result :: {ok, TSL} | {error, Reason},
		TSL :: pid(),
		Reason :: term().
%% @doc Start a new transaction sublayer (TSL).
%%
%% 	The transaction sublayer (TSL) is realized as an
%% 	instance of a {@link tcap_tco_server. tcap_tco_server}
%% 	behaviour process. Binding to a lower layer (SCCP)
%% 	service access point (SAP) is accomplished by providing
%% 	a callback module `Callback' for the
%% 	{@link tcap_tco_server. tcap_tco_server} behaviour.
%%
%% 	The {@link tcap_tco_server. tcap_tco_server} process
%% 	will be registered with `Name'.
%%
%% 	`Callback' is the name of the callback module or a
%% 	`#tcap_tco_cb{}' record definition of callback `fun()'s.
%%
%% 	`Args' will be the arguments passed to `Callback:init/1'.
%%
%% 	`Opts' may include any of the options available to
%% 	{@link //stdlib/gen_server:start_link/3. gen_server:start_link/3}.
%%
start_tsl(Name, Callback, Args, Opts) ->
	case tcap_sup:start_tsl(make_ref(), [Name, Callback, Args, Opts]) of
		{ok, Sup} ->
			ChildSpecs = supervisor:which_children(Sup),
			{_, TSL, _, _} = lists:keyfind(tcap_tco_server, 1, ChildSpecs),
			link(TSL),
			{ok, TSL};
		{error, Reason} ->
			{error, Reason}
	end.

-spec stop_tsl(TSL) -> Result
	when
		TSL :: pid(),
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Stop a running transaction sublayer (TSL).
stop_tsl(TSL) when is_pid(TSL) ->
	stop_tsl(TSL, supervisor:which_children(tcap_sup)).
%% @hidden
stop_tsl(TSL, [{Id, Sup, _, _} | T]) ->
	ChildSpecs = supervisor:which_children(Sup),
	case lists:keyfind(TSL, 2, ChildSpecs) of
		{_, TSL, _, _} ->
			case supervisor:terminate_child(tcap_sup, Id) of
				ok ->
					supervisor:delete_child(tcap_sup, Id);
				{error, Reason} ->
					{error, Reason}
			end;
		_ ->
			stop_tsl(TSL, T)
	end;
stop_tsl(_TSL, []) ->
	{error, not_found}.

-spec open(TSL, TCU) -> CSL
	when
		TSL :: pid(),
		TCU :: pid(),
		CSL :: {ok, DHA, CCO} | {error, Reason},
		DHA :: pid(),
		CCO :: pid(),
		Reason :: term().
%% @doc Start a new component sublayer (CSL).
%%
%% 	`TSL' is the pid returned from a previous call to
%% 	{@link start_tsl/3. start_tsl/3}.
%%
%% 	`TCU' is the pid of a `TC-User'.
%%
%% 	Returns `{ok, DHA, CCO}', the pids of the dialogue handler and
%% 	component coordinator in the newly created component sublayer.
%%
open(TSL, TCU) ->
	case supervisor:start_child(tcap_csl_sup, [[TSL, TCU]]) of
		{ok, Sup1} ->
			Children1 = supervisor:which_children(Sup1),
			{_, DHA, _, _} = lists:keyfind(tcap_dha_fsm, 1, Children1),
			{_, Sup2, _, _} = lists:keyfind(tcap_components_sup, 1, Children1),
			Children2 = supervisor:which_children(Sup2),
			{_, CCO, _, _} = lists:keyfind(tcap_cco_server, 1, Children2),
			{ok, DHA, CCO};
		{error, Reason} ->
			{error, Reason}
	end.

-spec close(DHA) -> Result
	when
		DHA :: pid(),
		Result :: ok | {error, not_found}.
%% @doc Close a component sublayer (CSL).
%%
%% 	`DHA' is a pid returned in a previous call to 
%% 	{@link open/2. open/2}.
%%
close(DHA) when is_pid(DHA) ->
	try
		{_, _, _, [PDict | _]} = sys:get_status(DHA),
		lists:keyfind('$ancestors', 1, PDict)
	of
		{_, [Sup | _]} ->
			supervisor:terminate_child(tcap_csl_sup, Sup)
	catch
		_ ->
			not_found
	end.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------


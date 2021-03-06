%%% tcap_tsl_sup.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
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
%%% @docfile "{@docsrc supervision.edoc}"
%%%
-module(tcap_tsl_sup).
-copyright('Copyright (c) 2021 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

-include("DialoguePDUs.hrl").
-include("tcap.hrl").

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [Arg],
		Arg :: TcoName | TcoArgs | TcoOpts,
		TcoName :: {local, LocalName} | {global, GlobalName}
				| {via, ViaModule, ViaName},
		LocalName :: atom(),
		GlobalName :: term(),
		ViaModule :: atom(),
		ViaName :: term(),
		TcoArgs :: [term()],
		TcoOpts :: [Option],
		Option :: {timeout, Timeout} | {debug, [Flag]},
		Timeout :: pos_integer(),
		Flag :: trace | log | {logfile, file:filename()}
				| statistics | debug,
		Result :: {ok, {SupFlags, [ChildSpec]}},
		SupFlags :: supervisor:sup_flags(),
		ChildSpec :: supervisor:child_spec().
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([TcoName, TcoCallback, TcoArgs, TcoOpts] = _Args)
		when is_tuple(TcoName),
		(is_atom(TcoCallback) or is_record(TcoCallback, tcap_tco_cb)),
		is_list(TcoArgs), is_list(TcoOpts) ->
	ChildSpecs = [supervisor(tcap_transaction_sup, []),
			tco_server(TcoCallback, TcoName, [self() | TcoArgs], TcoOpts)],
	SupFlags = #{strategy => one_for_all, intensity => 10, period => 60},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec supervisor(StartMod, Args) -> Result
	when
		StartMod :: atom(),
		Args :: [term()],
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/supervisor. supervisor} behaviour.
%% @private
%%
supervisor(StartMod, Args) ->
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			type => supervisor, modules => [StartMod]}.

-spec tco_server(Callback, Name, Args, Opts) -> Result
	when
		Callback :: atom() | #tcap_tco_cb{},
		Name :: {local, LocalName} | {global, GlobalName}
				| {via, ViaModule, ViaName},
		LocalName :: atom(),
		GlobalName :: term(),
		ViaModule :: atom(),
		ViaName :: term(),
		Args :: [term()],
		Opts :: [Option],
		Option :: {timeout, Timeout} | {debug, [Flag]},
		Timeout :: pos_integer(),
		Flag :: trace | log | {logfile, file:filename()}
				| statistics | debug,
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link tcap_tco_server. tcap_tco_server} behaviour
%% 	with a registered name and options.
%% @private
%%
tco_server(Callback, Name, Args, Opts) ->
	StartMod = tcap_tco_server,
	Modules = case Callback of
		Callback when is_atom(Callback) ->
			[StartMod, Callback];
		Callback when is_record(Callback, tcap_tco_cb) ->
			[StartMod, tcap_tco_callback]
	end,
	StartArgs = [Name, Callback, Args, Opts],
	StartFunc = {StartMod, start_link, StartArgs},
	#{id => StartMod, start => StartFunc, modules => Modules}.


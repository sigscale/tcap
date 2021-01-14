%%% tcap_transaction_sup.erl
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
-module(tcap_transaction_sup).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [],
		Result :: {ok, {SupFlags, [ChildSpec]}},
		SupFlags :: supervisor:sup_flags(),
		ChildSpec :: supervisor:child_spec().
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([] = _Args) ->
	ChildSpecs = [fsm(tcap_tsm_fsm)],
	SupFlags = #{strategy => simple_one_for_one,
			intensity => 0, period => 1},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec fsm(StartMod) -> Result
	when
		StartMod :: atom(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/gen_fsm. gen_fsm} behaviour.
%% @private
%%
fsm(StartMod) ->
	StartArgs = [StartMod],
	StartFunc = {gen_fsm, start_link, StartArgs},
	#{id => StartMod, start => StartFunc, modules => [StartMod]}.


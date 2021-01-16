%%% tcap_sup.erl
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
-module(tcap_sup).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).
%% export the private API
-export([start_tsl/2]).

%%----------------------------------------------------------------------
%%  The tcap_sup private API
%%----------------------------------------------------------------------

-spec start_tsl(Id, Args) -> Result
	when
		Id :: term(),
		Args :: [term()],
		Result :: supervisor:startchild_ret().
%% @doc Start a {@link tcap_tsl_sup. tcap_tsl_sup} supervisor.
%%
%% 	The `ChildSpec' for the {@link tcap_tsl_sup. tcap_tsl_sup}
%% 	may be identified by the `child_id()' in `Id'.
%%
%% 	`Args' should be a list of `[Name, Module, Args, Opts]' used to
%% 	start the TCO ({@link tcap_tco_server. tcap_tco_server}) with
%% 	{@link //stdlib/gen_server:start_link/4. gen_server:start_link/4}.
%%
%% @private
start_tsl(Id, Args) when is_list(Args) ->
	StartMod = tcap_tsl_sup,
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	ChildSpec = #{id => Id, type => supervisor,
			start => StartFunc, restart => permanent,
			modules => [StartMod]},
	supervisor:start_child(?MODULE, ChildSpec).

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [],
		Result :: {ok, {SupFlags, [ChildSpec]}} | ignore,
		SupFlags :: supervisor:sup_flags(),
		ChildSpec :: supervisor:child_spec().
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init(_Args) ->
	ChildSpecs = [supervisor(tcap_csl_sup, tcap_csl_sup, [])],
	SupFlags = #{intensity => 10, period => 60},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec supervisor(StartMod, Name, Args) -> Result
	when
		Name :: atom(),
		StartMod :: atom(),
		Args :: [term()],
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/supervisor. supervisor} behaviour.
%% @private
%%
supervisor(StartMod, Name, Args) ->
	StartArgs = [{local, Name}, StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			type => supervisor, modules => [StartMod]}.


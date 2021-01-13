%%% tcap_api_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
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
%%% Test suite for the public API of the {@link //tcap. tcap} application.
%%%
-module(tcap_api_SUITE).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([start_tsl/0, start_tsl/1, stop_tsl/0, stop_tsl/1]).

-include("tcap.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	application:start(tcap),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	application:stop(tcap).

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
   Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[start_tsl, stop_tsl].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

start_tsl() ->
	[{userdata, [{doc, "Start a transaction sublayer (TSL)"}]}].

start_tsl(_Config) ->
	{ok, TSL} = tcap:start_tsl(tcap_test_nsap_server, [self()], []),
	unlink(TSL),
	true = is_process_alive(TSL).

stop_tsl() ->
	[{userdata, [{doc, "Stop a transaction sublayer (TSL)"}]}].

stop_tsl(_Config) ->
	{ok, TSL} = tcap:start_tsl(tcap_test_nsap_server, [self()], []),
	unlink(TSL),
	ok = tcap:stop_tsl(TSL),
	false = is_process_alive(TSL).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------


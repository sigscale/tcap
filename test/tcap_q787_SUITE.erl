%%% tcap_q787_SUITE.erl
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
%%% Test suite for the SS7 Transaction Capabilities protocol stack
%%% of the {@link //tcap. tcap} application.
%%% @reference ITU-T Q,787 Transaction Capabilities (TC) Test Specification .
%%%
-module(tcap_q787_SUITE).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
%% common_test optional callbacks
-export([groups/0, group/1]).
%% common_test test cases
-export(['1.1.1.1'/0, '1.1.1.1'/1]).

-include("tcap.hrl").
-include("sccp_primitive.hrl").
-include_lib("sccp/include/sccp.hrl").
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
	{ok, TSL} = tcap:start_tsl(tcap_test_user, [self()], []),
	unlink(TSL),
	[{tco_pid, TSL} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	TSL = ?config(tco_pid, Config),
	ok = tcap:stop_tsl(TSL).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[{group, tsl}].

-spec groups() -> GroupDefs
	when
		GroupDefs :: [Group],
		Group :: {GroupName, Properties, GroupsAndTestCases},
		GroupName :: atom(),
		Properties :: [parallel | sequence | Shuffle | {GroupRepeatType, N}],
		GroupsAndTestCases :: [Group | {group, GroupName}
				| TestCase | {testcase, TestCase, TCRepeatProps}],
		TestCase :: atom(),
		TCRepeatProps :: [{repeat, N} | {repeat_until_ok, N}
				| {repeat_until_fail, N}],
		Shuffle :: shuffle | {shuffle, Seed},
		Seed :: {integer(), integer(), integer()},
		GroupRepeatType :: repeat | repeat_until_all_ok | repeat_until_all_fail
				| repeat_until_any_ok | repeat_until_any_fail,
		N :: integer() | forever.
%% @doc Define test case groups.
%%
groups() ->
	[{tsl, [], ['1.1.1.1']}, {tcl, [], []}].

-spec group(GroupName) -> [Info]
	when
		GroupName :: atom(),
		Info :: term().
%% @doc Test case group information.
group(tsl) ->
	[{userdata, [{doc, "Q.787 7.1 TC Transaction Sublayer (TSL) test specification"}]}];
group(tcl) ->
	[{userdata, [{doc, "Q.787 7.2 TC Component Sublayer (CSL) test specification"}]}].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

'1.1.1.1'() ->
	[{userdata, [{doc, "Valid function; Unstructured dialogue; Tested side sending"}]}].

'1.1.1.1'(Config) ->
	TSL = ?config(tco_pid, Config),
	CalledParty = #party_address{pc = 6210, ssn = 146, ri = true},
	CallingParty = #party_address{pc = 6202, ssn = 146, ri = true},
	DestAddress = sccp_codec:party_address(CalledParty),
	OrigAddress = sccp_codec:party_address(CallingParty),
	ComponentPortion = crypto:strong_rand_bytes(rand:uniform(512)),
	UserData = #'TR-user-data'{dialoguePortion = asn1_NOVALUE,
			componentPortion = ComponentPortion},
	SequenceControl = false,
	ReturnOption = false,
	TrUniParms = #'TR-UNI'{qos = {SequenceControl, ReturnOption},
			destAddress = DestAddress, origAddress = OrigAddress,
			userData = UserData},
	gen_server:cast(TSL, {'TR', 'UNI', request, TrUniParms}),
	SccpParams = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UnitData} ->
			UnitData
	end,
	#'N-UNITDATA'{calledAddress = DestAddress,
			callingAddress = OrigAddress,
			sequenceControl = SequenceControl,
			returnOption = ReturnOption,
			userData = _} = SccpParams.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------


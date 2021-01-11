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
-export([receive_unidirectional/0, receive_unidirectional/1,
		send_unidirectional/0, send_unidirectional/1]).

-include("tcap.hrl").
-include("sccp_primitive.hrl").
-include("TC.hrl").
-include("TR.hrl").
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

-spec group(GroupName) -> [Info]
	when
		GroupName :: atom(),
		Info :: term().
%% @doc Test case group information.
group(tsl) ->
	[{userdata, [{doc, "Q.787 7.1 TC Transaction Sublayer (TSL) test specification"}]}];
group(csl) ->
	[{userdata, [{doc, "Q.787 7.2 TC Component Sublayer (CSL) test specification"}]}].

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
	TslCases = [receive_unidirectional, send_unidirectional],
	CslCases = [],
	[{tsl, [], TslCases}, {csl, [], CslCases}].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

send_unidirectional() ->
	[{userdata,
			[{number, "1.1.1.1"},
			{reference, "3.3.3.1.1/Q.774"},
			{title, "Valid function; Unstructured dialogue"},
			{subtitle, "Tested side sending"},
			{purpose, "To verify that signalling point A is able to correctly send a Unidirectional message."},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state."},
			{description,
					"1. Send a Unidirectional message from SP A to SP B.\n"
					"2. Check A: Was the Unidirectional message correctly sent from SP A?\n"
					"3. Check B: Was the TSL state machine associated with this transaction left in the idle state at SP A?"}]}].

send_unidirectional(Config) ->
	TSL = ?config(tco_pid, Config),
	Invoke = {invoke, #'Invoke'{invokeId = {present, 1},
			linkedId = asn1_NOVALUE,
			opcode = {local, 1},
			argument = asn1_NOVALUE}},
	{ok, ComponentPortion} = 'TC':encode('Components', [Invoke]),
	UserData = #'TR-user-data'{dialoguePortion = asn1_NOVALUE,
			componentPortion = ComponentPortion},
	SequenceControl = false,
	ReturnOption = false,
	CalledParty = #party_address{pc = 6210, ssn = 146, ri = true},
	CallingParty = #party_address{pc = 6202, ssn = 146, ri = true},
	DestAddress = sccp_codec:party_address(CalledParty),
	OrigAddress = sccp_codec:party_address(CallingParty),
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

receive_unidirectional() ->
	[{userdata,
			[{number, "1.1.1.1"},
			{reference, "3.3.3.1.2/Q.774"},
			{title, "Valid function; Unstructured dialogue"},
			{subtitle, "Tested side receiving"},
			{purpose, "To verify that signalling point A is able to correctly receive a Unidirectional message."},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state"},
			{description,
					"1. Send a Unidirectional message from SP B to SP A.\n"
					"2. Check A: Was the Unidirectional message correctly received at SP A?\n"
					"3. Check B: Was the TSL state machine associated with this transaction left in the idle state at SP A?"}]}].

receive_unidirectional(Config) ->
	TSL = ?config(tco_pid, Config),
	Invoke = {invoke, #'Invoke'{invokeId = {present, 1},
			linkedId = asn1_NOVALUE,
			opcode = {local, 1},
			argument = asn1_NOVALUE}},
	{ok, ComponentPortion} = 'TC':encode('Components', [Invoke]),
	Unidirectional = {unidirectional,
			#'Unidirectional'{components = ComponentPortion}},
	{ok, SccpUserData} = 'TR':encode('TCMessage', Unidirectional),
	CalledParty = #party_address{pc = 6202, ssn = 146, ri = true},
	CallingParty = #party_address{pc = 6210, ssn = 146, ri = true},
	DestAddress = sccp_codec:party_address(CalledParty),
	OrigAddress = sccp_codec:party_address(CallingParty),
	SequenceControl = false,
	ReturnOption = false,
	UnitData = #'N-UNITDATA'{calledAddress = DestAddress,
			callingAddress = OrigAddress,
			sequenceControl = SequenceControl,
			returnOption = ReturnOption,
			userData = SccpUserData},
	gen_server:cast(TSL, {'N', 'UNITDATA', indication, UnitData}),
	TcUniParams = receive
		{'TC', 'UNI', indication, #'TC-UNI'{} = UniParams} ->
		UniParams
	end,
	#'TC-UNI'{qos = {SequenceControl, ReturnOption},
			destAddress = DestAddress, origAddress = OrigAddress,
			componentsPresent = true} = TcUniParams.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------


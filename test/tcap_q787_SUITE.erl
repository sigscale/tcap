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
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
%% common_test optional callbacks
-export([init_per_group/2, end_per_group/2, groups/0, group/1]).
%% common_test test cases
-export([recv_unidirectional/0, recv_unidirectional/1,
		send_unidirectional/0, send_unidirectional/1,
		send_begin_prearranged/0, send_begin_prearranged/1,
		send_end_basic/0, send_end_basic/1,
		recv_end_basic/0, recv_end_basic/1,
		recv_msg_established/0, recv_msg_established/1,
		send_class_1/0, send_class_1/1]).

-include("DialoguePDUs.hrl").
-include("tcap.hrl").
-include("sccp_primitive.hrl").
-include("TC.hrl").
-include("TR.hrl").
-include("TCAP-Examples.hrl").
-include_lib("sccp/include/sccp.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc,
			"ITU-T Q.787 Transaction Capabilities (TC) Test Specification"}]},
			{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	application:start(tcap),
	{SPA, SPB} = addresses(),
   [{spa, SPA}, {spb, SPB} | Config].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	application:stop(tcap).

-spec init_per_group(Group :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test group.
%%
init_per_group(_Group, Config) ->
	Config.

-spec end_per_group(Group :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test group.
%%
end_per_group(_Group, _Config) ->
	ok.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(TestCase, Config)
		when TestCase == send_unidirectional;
		TestCase == recv_unidirectional;
		TestCase == send_begin_prearranged;
		TestCase == send_end_basic;
		TestCase == recv_end_basic;
		TestCase == recv_msg_established ->
	describe(TestCase),
	Module = tcap_test_tsl_server,
	{ok, TSL} = tcap:start_tsl({local, Module}, Module, [self()], []),
	unlink(TSL),
	[{tco, TSL} | Config];
init_per_testcase(TestCase, Config)
		when TestCase == send_class_1 ->
	describe(TestCase),
	Module = tcap_test_csl_server,
	{ok, TSL} = tcap:start_tsl({local, Module}, Module, [self()], []),
	unlink(TSL),
	[{tco, TSL} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	TSL = ?config(tco, Config),
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
	[{group, tsl}, {group, csl}].

-spec group(GroupName) -> [Info]
	when
		GroupName :: atom(),
		Info :: term().
%% @doc Test case group information.
group(tsl) ->
	[{userdata, [{doc, "7.1 TC Transaction Sublayer (TSL) test specification"}]}];
group(csl) ->
	[{userdata, [{doc, "7.2 TC Component Sublayer (CSL) test specification"}]}].

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
	TslCases = [send_unidirectional, recv_unidirectional,
			send_begin_prearranged, send_end_basic, recv_end_basic,
			recv_msg_established],
	CslCases = [send_class_1],
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
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	ComponentPortion = tr_invoke(1),
	UserData = #'TR-user-data'{componentPortion = ComponentPortion},
	SequenceControl = false,
	ReturnOption = true,
	TrUniParms = #'TR-UNI'{qos = {SequenceControl, ReturnOption},
			destAddress = SPB, origAddress = SPA, userData = UserData},
	tr_send({'TR', 'UNI', request, TrUniParms}, Config),
	SccpParams = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD} -> UD
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			userData = _} = SccpParams.

recv_unidirectional() ->
	[{userdata,
			[{number, "1.1.1.2"},
			{reference, "3.3.3.1.2/Q.774"},
			{title, "Valid function; Unstructured dialogue"},
			{subtitle, "Tested side receiving"},
			{purpose, "To verify that signalling point A is able to correctly receive a Unidirectional message."},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state"},
			{description,
					"1. Send a Unidirectional message from SP B to SP A.\n"
					"2. Check A: Was the Unidirectional message correctly received at SP A?\n"
					"3. Check B: Was the TSL state machine associated with this transaction left in the idle state at SP A?"}]}].

recv_unidirectional(Config) ->
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	ComponentPortion = tr_invoke(1),
	Unidirectional = #'Unidirectional'{components = ComponentPortion},
	sccp_send(SPA, SPB, Unidirectional, Config),
	TrUniParams = receive
		{'TR', 'UNI', indication, #'TR-UNI'{} = UP} -> UP
	end,
	#'TR-UNI'{qos = {false, true},
			destAddress = SPA, origAddress = SPB,
			userData = _} = TrUniParams.

send_begin_prearranged() ->
	[{userdata,
			[{number, "1.1.2.1.1 1)"},
			{reference, "3.3.3.2.1/Q.774 and 3.3.3.2.3/Q.774"},
			{title, "Valid function; Structured dialogue"},
			{subtitle, "Clearing before subsequent Message; Valid clearing "
					"from initiating side; Prearranged ending"},
			{purpose, "To verify that signalling point A is able to correctly "
					"send a Begin message and then terminate the transaction "
					"locally by the \"prearranged end\" method"},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state."},
			{description,
					"1. Send a Begin message from SP A to SP B.\n"
					"2. Before a reply is received from SP B, arrange for a "
						"TR-END request primitive (prearranged) to be passed to "
						"the TSL at SP A.\n"
					"3. Check A: Was the begin message correctly sent from SP A?\n"
					"4. Check B: Verify that an end message was not sent by SP A.\n"
					"5. Check C: Were tsl state machines associated with this "
						"transaction left in the idle state at SP A?\n"}]}].

send_begin_prearranged(Config) ->
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	TID = tcap_tco_server:new_tid(),
	ComponentPortion = tr_invoke(1),
	UserData = #'TR-user-data'{componentPortion = ComponentPortion},
	SequenceControl = false,
	ReturnOption = true,
	TrBeginParms = #'TR-BEGIN'{transactionID = TID,
			qos = {SequenceControl, ReturnOption},
			destAddress = SPB, origAddress = SPA,
			userData = UserData},
	Config1 = tr_send({'TR', 'BEGIN', request, TrBeginParms}, Config),
	SccpParams = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD} -> UD
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = false, returnOption = true,
			userData = _} = SccpParams,
	TrEndParms = #'TR-END'{transactionID = TID,
			qos = {SequenceControl, ReturnOption},
			termination = prearranged},
	tr_send({'TR', 'END', request, TrEndParms}, Config1),
	receive
		{'N', 'UNITDATA', request, _} ->
			ct:fail(end_sent)
	after
		1000 ->
			ok
	end.

send_end_basic() ->
	[{userdata,
			[{number, "1.1.2.1.2.1 1)"},
			{reference, "3.3.3.2.1/Q.774 and 3.3.3.2.3/Q.774"},
			{title, "Valid function; Structured dialogue"},
			{subtitle, "Clearing before subsequent Message; Valid clearing "
					"from responding side; IUT Sending; Basic ending"},
			{purpose, "To verify that signalling point A is able to receive a "
					"Begin message and then terminate the transaction by the "
					"\"basic end\" method"},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state"},
			{description,
					"1. Send a Begin message from SP B to SP A.\n"
					"2. On receipt of BEGIN indication arrange for a TR-END "
						"request primitive (basic) to be passed to the TSL at "
						"SP A.\n"
					"3. Check A: Was the begin message correctly received at "
						"SP A and passed to the TR-User?\n"
					"4. Check B: Was an end message correctly sent by SP A?\n"
					"5. Check C: Was the DTID in the end message the same as "
						"the otid in the begin message?\n"
					"6. Check C: Were TSL state machines associated with this "
						"transaction left in the idle state at SP A?"}]}].

send_end_basic(Config) ->
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	ComponentPortion1 = tr_invoke(1),
	TID = tcap_tco_server:new_tid(),
	Begin = #'Begin'{otid = <<TID:32>>, components = ComponentPortion1},
	Config1 = sccp_send(SPA, SPB, Begin, Config),
	TrBeginParams = receive
		{'TR', 'BEGIN', indication, #'TR-BEGIN'{} = BP} -> BP
	end,
	#'TR-BEGIN'{transactionID = LocalTID, qos = {false, true},
			destAddress = SPA, origAddress = SPB,
			userData = _} = TrBeginParams,
	ComponentPortion2 = tr_return_result(1),
	TrUserData1 = #'TR-user-data'{componentPortion = ComponentPortion2},
	TrEndParams = #'TR-END'{qos = {false, true},
			transactionID = LocalTID, userData = TrUserData1,
			termination = basic},
	tr_send({'TR', 'END', request, TrEndParams}, Config1),
	SccpParams = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD} -> UD
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = false, returnOption = true,
			userData = SccpUserData2} = SccpParams,
	{ok, {'end', End}} = 'TR':decode('TCMessage', SccpUserData2),
	#'End'{dtid = <<TID:32>>} = End.

recv_end_basic() ->
	[{userdata,
			[{number, "1.1.2.1.2.2 3)"},
			{reference, "3.3.3.2.1/Q.774 and 3.3.3.2.3/Q.774"},
			{title, "Valid function; Structured dialogue"},
			{subtitle, "Clearing before subsequent Message; "
					"Valid clearing from responding side; "
					"IUT receiving; Basic ending"},
			{purpose, "To verify that the signalling point A is able to "
					"terminate a transaction on reception of an END message"},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state"},
			{description,
					"1. Send a Begin message from SP A to SP B.\n"
					"2. Arrange for SP B to send an End message to SP A.\n"
					"3. Check A: Was the Begin message correctly sent from SP A?\n"
					"4. Check B: Was the End message correctly received at SP A?\n"
					"5. Check C: Were TSL state machines associated with this "
						"transaction left in the idle state at SP A?"}]}].

recv_end_basic(Config) ->
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	TID = tcap_tco_server:new_tid(),
	ComponentPortion1 = tr_invoke(1),
	UserData = #'TR-user-data'{componentPortion = ComponentPortion1},
	TrBeginParms = #'TR-BEGIN'{transactionID = TID, qos = {false, true},
			destAddress = SPB, origAddress = SPA, userData = UserData},
	tr_send({'TR', 'BEGIN', request, TrBeginParms}, Config),
	SccpParams = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD} -> UD
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = false, returnOption = true,
			userData = _} = SccpParams,
	ComponentPortion2 = tr_return_result(1),
	End = #'End'{dtid = <<TID:32>>, components = ComponentPortion2},
	sccp_send(SPA, SPB, End, Config),
	TrEndParams = receive
		{'TR', 'END', indication, #'TR-END'{} = EP} -> EP
	end,
	#'TR-END'{transactionID = TID} = TrEndParams.

recv_msg_established() ->
	[{userdata,
			[{number, "1.1.2.4.1"},
			{reference, "3.2.1.3/Q.774"},
			{title, "Valid function; Structured dialogue"},
			{subtitle, "Message exchange after transaction established; "
					"IUT initiating"},
			{purpose, "To verify the correct message flow between SP A and SP B, "
					"after transaction established (IUT initiating)"},
			{conditions, "SP A (TSL) and SP B (TSL) are to be in the idle state"},
			{description,
					"1. Arrange for SP A to send a Begin message to SP B.\n"
					"2. Arrange for SP B to send a Continue message to SP A.\n"
					"3. Arrange for SP A to send a Continue message to SP B.\n"
					"4. Arrange for SP B to send an END message to SP A.\n"
					"5. Check A: Was the Begin message correctly sent from SP A?\n"
					"6. Check B: Was the Continue message correctly received at SP A?\n"
					"7. Check C: Was the Continue message correctly sent from SP A?\n"
					"8. Check D: Was the End message correctly received at SP A?\n"
					"9. Check E: Was the TSL state machine left in the idle state at SP A?"}]}].

recv_msg_established(Config) ->
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	TIDA = tcap_tco_server:new_tid(),
	ComponentPortion1 = tr_invoke(1),
	UserData1 = #'TR-user-data'{componentPortion = ComponentPortion1},
	TrBeginParms = #'TR-BEGIN'{transactionID = TIDA, qos = {false, true},
			destAddress = SPB, origAddress = SPA, userData = UserData1},
	Config1 = tr_send({'TR', 'BEGIN', request, TrBeginParms}, Config),
	SccpParams1 = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD1} -> UD1
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = false, returnOption = true,
			userData = _} = SccpParams1,
	ComponentPortion2 = tr_return_result(1),
	TIDB = tcap_tco_server:new_tid(),
	Continue = #'Continue'{otid = <<TIDB:32>>, dtid = <<TIDA:32>>,
			components = ComponentPortion2},
	sccp_send(SPA, SPB, Continue, Config1),
	TrContParams1 = receive
		{'TR', 'CONTINUE', indication, #'TR-CONTINUE'{} = CP} -> CP
	end,
	#'TR-CONTINUE'{transactionID = TIDA,
			qos = {false, true}} = TrContParams1,
	ComponentPortion3 = tr_invoke(2),
	UserData2 = #'TR-user-data'{componentPortion = ComponentPortion3},
	TrContParams2 = #'TR-CONTINUE'{origAddress = SPA,
			qos = {false, true}, transactionID = TIDA,
			userData = UserData2},
	tr_send({'TR', 'CONTINUE', request, TrContParams2}, Config1),
	SccpParams2 = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD2} -> UD2
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = false, returnOption = true,
			userData = _} = SccpParams2,
	ComponentPortion4 = tr_return_result(2),
	End = #'End'{dtid = <<TIDA:32>>, components = ComponentPortion4},
	sccp_send(SPA, SPB, End, Config1),
	TrEndParams = receive
		{'TR', 'END', indication, #'TR-END'{} = EP} -> EP
	end,
	#'TR-END'{transactionID = TIDA, qos = {false, true}} = TrEndParams.

send_class_1() ->
	[{userdata,
			[{number, "2.1.1.1.1"},
			{reference, "3.2.1/Q.774"},
			{title, "Valid functions; Invoke component, unlinked operations"},
			{subtitle, "Class 1 single operation invocation; "
					"IUT as sender: receive result"},
			{purpose, "To verify that a single Class 1 operation can be "
					"successfully invoked and the successful completion of the "
					"operation can be received and delivered to the TC-User"},
			{conditions,
					"1) Arrange the TC-User stimulus such that an appropriate TSL "
						"message generated at SP A contains an Invoke component\n"
					"2) Arrange the data at SP B such that a Return Result-Last "
						"component can be generated"},
			{description,
					"1. Initiate a single operation invocation from SP A to "
						"SP B.\n"
					"2. Check A: Was the Invoke component with correct "
						"information sent by SP A?\n"
					"3. Check B: Was the Return Result-Last component with "
						"correct information passed to tc-user by SP A?\n"
					"4. Check C: Was the invocation state machine idle at "
						"SP A?"}]}].

send_class_1(Config) ->
	SPA = ?config(spa, Config),
	SPB = ?config(spb, Config),
	Config1 = tc_start(Config),
	DID = tcap_tco_server:new_tid(),
	Argument1 = invoke_argument(),
	TcInvokeParms = #'TC-INVOKE'{dialogueID = DID, class = 1,
			invokeID = 1, operation = {local, 1}, parameters = Argument1},
	tc_send({'TC', 'INVOKE', request, TcInvokeParms}, Config1),
	TcBeginParms = #'TC-BEGIN'{dialogueID = DID, qos = {false, true},
			destAddress = SPB, origAddress = SPA, componentsPresent = true},
	tc_send({'TC', 'BEGIN', request, TcBeginParms}, Config1),
	SccpParams = receive
		{'N', 'UNITDATA', request, #'N-UNITDATA'{} = UD} -> UD
	end,
	#'N-UNITDATA'{calledAddress = SPB, callingAddress = SPA,
			sequenceControl = false, returnOption = true,
			userData = _} = SccpParams,
	ComponentPortion = tr_return_result(1),
	End = #'End'{dtid = <<DID:32>>, components = ComponentPortion},
	sccp_send(SPA, SPB, End, Config1),
	TcResultParams = receive
		{'TC', 'RESULT-L', indication, #'TC-RESULT-L'{} = RL} -> RL
	end,
	#'TC-RESULT-L'{dialogueID = DID, invokeID = 1,
			operation = {local, 1}, parameters = _Result,
			lastComponent = true} = TcResultParams,
	TcEndParams = receive
		{'TC', 'END', indication, #'TC-END'{} = EP} -> EP
	end,
	#'TC-END'{dialogueID = DID, qos = {false, true},
			componentsPresent = true} = TcEndParams.

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

-spec describe(TestCase) -> ok
	when
		TestCase :: atom().
%% @doc Describe the test case in the log.
describe(TestCase) ->
	Group = get_group(TestCase),
	{_, UserData1} = lists:keyfind(userdata, 1, suite()),
	{_, UserData2} = lists:keyfind(userdata, 1, group(Group)),
	{_, UserData3} = lists:keyfind(userdata, 1, ?MODULE:TestCase()),
	Header1 = "<h2>" ++ proplists:get_value(doc, UserData1) ++ "</h2>",
	Header2 = "<h3>" ++ proplists:get_value(doc, UserData2) ++ "</h3>",
	Number = proplists:get_value(number, UserData3),
	Reference = proplists:get_value(reference, UserData3),
	Title = proplists:get_value(title, UserData3),
	Subtitle = proplists:get_value(subtitle, UserData3),
	Purpose = proplists:get_value(purpose, UserData3),
	Conditions = proplists:get_value(conditions, UserData3),
	Description = proplists:get_value(description, UserData3),
	Table = "<table>"
			"<tr><td>Test Number:</td>"
			"<td>" ++ Number ++ "</td></tr>"
			"<tr><td>Reference:</td>"
			"<td>" ++ Reference ++ "</td></tr>"
			"<tr><td>Title:</td>"
			"<td>" ++ Title ++ "</td></tr>"
			"<tr><td>Subtitle:</td>"
			"<td>" ++ Subtitle ++ "</td></tr>"
			"<tr><td>Purpose:</td>"
			"<td>" ++ Purpose ++ "</td></tr>"
			"<tr><td>Pre-test Conditions:</td>"
			"<td>" ++ Conditions ++ "</td></tr>"
			"<tr><td>Test Description:</td>"
			"<td>" ++ Description ++ "</td></tr>"
			"</table>",
	ct:comment(Title ++ " " ++ Subtitle),
	ct:log(Header1 ++ Header2 ++ Table).

-spec get_group(TestCase) -> Group
	when
		TestCase :: atom(),
		Group :: atom().
%%% @doc Get the `Group' a `TestCase' is a member of.
get_group(TestCase) ->
	get_group(TestCase, groups()).
%% @hidden
get_group(TestCase, [{Group, _, TestCases} | T]) ->
	case lists:member(TestCase, TestCases) of
		true ->
			Group;
		false ->
			get_group(TestCase, T)
	end;
get_group(_, []) ->
	none.

-spec addresses() -> {SPAAddress, SPBAddress}
	when
		SPAAddress :: binary(),
		SPBAddress :: binary().
%% @doc Encode SPA and SPB SCCP addresses.
addresses() ->
	SPAParty = #party_address{pc = 6202, ssn = 146, ri = true},
	SPBParty = #party_address{pc = 6210, ssn = 146, ri = true},
	SPAAddress = sccp_codec:party_address(SPAParty),
	SPBAddress = sccp_codec:party_address(SPBParty),
	{SPAAddress, SPBAddress}.

-spec unitdata(CalledAddress, CallingAddress, UserData) -> Result
	when
		CalledAddress :: binary(),
		CallingAddress :: binary(),
		UserData :: binary(),
		Result :: #'N-UNITDATA'{}.
%% @doc Return an SCCP unit data record.
unitdata(CalledAddress, CallingAddress, UserData) ->
	#'N-UNITDATA'{calledAddress = CalledAddress,
			callingAddress = CallingAddress,
			sequenceControl = false, returnOption = true,
			userData = UserData}.

-spec sccp_send(CalledAddress, CallingAddress, TCMessage, Config) -> Config
	when
		CalledAddress :: binary(),
		CallingAddress :: binary(),
		TCMessage :: #'Unidirectional'{} | #'Begin'{}
				| #'Continue'{} | #'End'{},
		Config :: [tuple()].
%% @doc Send SCCP data to TCO.
%%
%% 	If a new dialogue is required get the TC-User.
%%
sccp_send(CalledAddress, CallingAddress,
		#'Unidirectional'{} = TCMessage, Config) ->
	TSL = ?config(tco, Config),
	{ok, SccpUserData} = 'TR':encode('TCMessage', {unidirectional, TCMessage}),
	UnitData = unitdata(CalledAddress, CallingAddress, SccpUserData),
	gen_server:cast(TSL, {'N', 'UNITDATA', indication, UnitData}),
	receive
		{tcap_test_tsl_fsm, TCU} ->
			[{tcu, TCU} | Config]
	end;
sccp_send(CalledAddress, CallingAddress,
		#'Begin'{} = TCMessage, Config) ->
	TSL = ?config(tco, Config),
	{ok, SccpUserData} = 'TR':encode('TCMessage', {'begin', TCMessage}),
	UnitData = unitdata(CalledAddress, CallingAddress, SccpUserData),
	gen_server:cast(TSL, {'N', 'UNITDATA', indication, UnitData}),
	receive
		{tcap_test_tsl_fsm, TCU} ->
			[{tcu, TCU} | Config]
	end;
sccp_send(CalledAddress, CallingAddress,
		#'Continue'{} = TCMessage, Config) ->
	TSL = ?config(tco, Config),
	{ok, SccpUserData} = 'TR':encode('TCMessage', {continue, TCMessage}),
	UnitData = unitdata(CalledAddress, CallingAddress, SccpUserData),
	gen_server:cast(TSL, {'N', 'UNITDATA', indication, UnitData}),
	Config;
sccp_send(CalledAddress, CallingAddress,
		#'End'{} = TCMessage, Config) ->
	TSL = ?config(tco, Config),
	{ok, SccpUserData} = 'TR':encode('TCMessage', {'end', TCMessage}),
	UnitData = unitdata(CalledAddress, CallingAddress, SccpUserData),
	gen_server:cast(TSL, {'N', 'UNITDATA', indication, UnitData}),
	Config.

-spec tr_invoke(Id) -> Result
	when
		Id :: 1..127,
		Result :: binary().
%% @doc Encode an `invoke' component.
%%
%% 	Based on the `provideRoutingInformation'
%% 	operation described in Q.775 TCAP-Examples.
%%
tr_invoke(Id) ->
	InvokeId = {present, Id},
	OpCode = {local, 1},
	Argument = invoke_argument(),
	Invoke = #'Invoke'{invokeId = InvokeId,
			linkedId = asn1_NOVALUE,
			opcode = OpCode, argument = Argument},
	Component = {invoke, Invoke},
	{ok, ComponentPortion} = 'TC':encode('Components', [Component]),
	ComponentPortion.

-spec tr_return_result(Id) -> Result
	when
		Id :: 1..127,
		Result :: binary().
%% @doc Encode a `returnResult' component.
%%
%% 	Based on the `provideRoutingInformation'
%% 	operation described in Q.775 TCAP-Examples.
%%
tr_return_result(Id) ->
	InvokeId = {present, Id},
	OpCode = {local, 1},
	Result = result_argument(),
	ReturnResultResult = #'ReturnResult_result'{opcode = OpCode, result = Result},
	ReturnResult = #'ReturnResult'{invokeId = InvokeId, result = ReturnResultResult},
	Component = {returnResult, ReturnResult},
	{ok, ComponentPortion} = 'TC':encode('Components', [Component]),
	ComponentPortion.

-spec invoke_argument() -> Result
	when
		Result :: binary().
%% @doc Encode a TC-User `invoke' argument.
%%
%% 	Based on the `provideRoutingInformation'
%% 	operation described in Q.775 TCAP-Examples.
%%
invoke_argument() ->
	CalledNumber = #'IsdnNumber'{typeOfAddress = international,
			digits = "14165551234"},
	RequestArgument = #'RequestArgument'{calledNumber = CalledNumber},
	{ok, Argument} = 'TCAP-Examples':encode('RequestArgument', RequestArgument),
	Argument.

-spec result_argument() -> Result
	when
		Result :: binary().
%% @doc Encode a TC-User `returnResult' argument.
%%
%% 	Based on the `provideRoutingInformation'
%% 	operation described in Q.775 TCAP-Examples.
%%
result_argument() ->
	RoutingNumber = #'IsdnNumber'{digits = "14165550000",
			typeOfAddress = international},
	RoutingInformation = {reroutingNumber, RoutingNumber},
	{ok, Argument} = 'TCAP-Examples':encode('RoutingInformation', RoutingInformation),
	Argument.

-spec tr_send(Primitive, Config) -> Config
	when
		Primitive :: {'TR', Name, request, Parameters},
		Name :: 'UNI' | 'BEGIN' | 'CONTINUE' | 'END' | 'U-ABORT',
		Parameters :: #'TR-UNI'{} | #'TR-BEGIN'{} | #'TR-CONTINUE'{}
				| #'TR-END'{} | #'TR-U-ABORT'{},
		Config :: [tuple()].
%% @doc Request TR-User to send a primitive to TCO.
%%
%% 	Starts a TR-User if a new dialogue is required.
%%
tr_send({'TR', Name, request, _Parameters} = Primitive, Config)
		when Name == 'UNI'; Name == 'BEGIN' ->
	TCO = ?config(tco, Config),
	{ok, TCU} = gen_statem:start_link(tcap_test_tsl_fsm, [TCO, self()], []),
	receive
		{tcap_test_tsl_fsm, TCU} ->
			gen_statem:cast(TCU, {tr_send, Primitive}),
			[{tcu, TCU} | Config]
	end;
tr_send({'TR', Name, request, _Parameters} = Primitive, Config)
		when Name == 'CONTINUE'; Name == 'END' ->
	TCU = ?config(tcu, Config),
	gen_statem:cast(TCU, {tr_send, Primitive}).

-spec tc_start(Config) -> Config
	when
		Config :: [tuple()].
%% @doc Start TC-User and CSL.
tc_start(Config) ->
	TCO = ?config(tco, Config),
	{ok, TCU} = gen_statem:start_link(tcap_test_csl_fsm, [self()], []),
	{ok, DHA, CCO} = tcap:open(TCO, TCU),
	ok = gen_statem:call(TCU, {csl_open, DHA, CCO}),
	[{tcu, TCU}, {dha, DHA}, {cco, CCO} | Config].

-spec tc_send(Primitive, Config) -> ok
	when
		Primitive :: {'TC', Name, request, Parameters},
		Name :: 'UNI' | 'BEGIN' | 'CONTINUE' | 'END' | 'U-ABORT'
				| 'INVOKE' | 'RESULT-L' | 'U-ERROR' | 'U-CANCEL' | 'U-REJECT',
		Parameters :: #'TC-UNI'{} | #'TC-BEGIN'{} | #'TC-CONTINUE'{}
				| #'TC-END'{} | #'TC-U-ABORT'{} | #'TC-INVOKE'{}
				| #'TC-RESULT-L'{} | #'TC-U-ERROR'{}
				| #'TC-U-CANCEL'{} | #'TC-U-REJECT'{},
		Config :: [tuple()].
%% @doc Request TC-User to send a primitive to DHA.
%%
%% 	Starts a TC-User if a new dialogue is required.
%%
tc_send({'TC', _Name, request, _Parameters} = Primitive, Config) ->
	TCU = ?config(tcu, Config),
	gen_statem:cast(TCU, {tc_send, Primitive}).


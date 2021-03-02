%%% tcap.hrl
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
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
%%%
%%%  ITU-T recommendation Q.771 Functional Decsription of 
%%%  Transaction Capabilities describes the primitives and
%%%  their parameters used in the TC-Service and TR-Service
%%%  interfaces.  Each primitive has a record defined here
%%%  containing it's parameters.  Modules using these
%%%  services utilize these records to format messages:
%%%
%%%  {'TC', 'BEGIN', request, #'TC-BEGIN'{})
%%%  {'TR', 'UNI', indication, #TR-UNI'{})
%%%

%%%
%%%  TC-User
%%%

-type problemCode() :: {general,
			unrecognizedPDU | mistypedPDU | badlyStructuredPDU}
		| {invoke, duplicateInvocation | unrecognizedOperation
				| mistypedArgument | resourceLimitation
				| releaseInProgress | unrecognizedLinkedId
				| linkedResponseUnexpected | unexpectedLinkedOperation}
		| {returnResult, unrecognizedInvocation
				| resultResponseUnexpected | mistypedResult}
		| {returnError, unrecognizedInvocation | errorResponseUnexpected
				| unrecognizedError | unexpectedError | mistypedParameter}.

%% reference: Table 3/Q.771 - TC-UNI primitives
-record('TC-UNI',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		destAddress :: sccp:party_address(),
		appContextName :: tuple(),
		origAddress :: sccp:party_address(),
		dialogueID :: 0..4294967295,
		userInfo = asn1_NOVALUE :: [#'EXTERNAL'{}] | asn1_NOVALUE,
		componentsPresent :: boolean() | undefined}).

%% reference: Table 4/Q.771 - TC-BEGIN primitives
-record('TC-BEGIN',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		destAddress :: sccp:party_address(),
		appContextName :: tuple(),
		origAddress :: sccp:party_address(),
		dialogueID :: 0..4294967295,
		userInfo = asn1_NOVALUE :: [#'EXTERNAL'{}] | asn1_NOVALUE,
		componentsPresent :: boolean() | undefined}).

%% reference: Table 5/Q.771 - TC-CONTINUE primitives
-record('TC-CONTINUE',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		origAddress :: sccp:party_address(),
		appContextName :: tuple(),
		dialogueID :: 0..4294967295,
		userInfo = asn1_NOVALUE :: [#'EXTERNAL'{}] | asn1_NOVALUE,
		componentsPresent :: boolean() | undefined}).

%% reference: Table 7/Q.771 - TC-END primitives
-record('TC-END',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		dialogueID :: 0..4294967295,
		appContextName :: tuple(),
		componentsPresent :: boolean() | undefined,
		userInfo = asn1_NOVALUE :: [#'EXTERNAL'{}] | asn1_NOVALUE,
		termination :: prearranged | basic | abort}).

%% reference: Table 8/Q.771 - TC-U-ABORT primitives
-record('TC-U-ABORT',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		dialogueID :: 0..4294967295,
		abortReason :: applicationContextNotSupported | dialogueRefused | userSpecific,
		appContextName :: tuple(),
		userInfo = asn1_NOVALUE :: [#'EXTERNAL'{}] | asn1_NOVALUE}).

%% reference: Table 9/Q.771 - TC-NOTICE primitives
-record('TC-NOTICE',
		{dialogueID :: 0..4294967295,
		origAddress :: sccp:party_address(),
		destAddress :: sccp:party_address(),
		reportCause}).

%% reference: Table 10/Q.771 - Operation invocation primitives
-record('TC-INVOKE',
		{dialogueID :: 0..4294967295,
		class :: 1..4,
		invokeID :: -128..127,
		linkedID :: -128..127 | undefined,
		operation :: {local, integer()} | {global, tuple()},
		parameters :: binary() | undefined,
		lastComponent :: boolean(),
		timeout = 5000 :: pos_integer()}).

%% reference: Table 11/Q.771 - Report of success primitives
-record('TC-RESULT-L',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127,
		operation :: {local, integer()} | {global, tuple()},
		parameters :: binary() | undefined,
		lastComponent :: boolean()}).
-record('TC-RESULT-NL',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127,
		operation :: {local, integer()} | {global, tuple()},
		parameters :: binary() | undefined,
		lastComponent :: boolean()}).

%% reference: Table 12/Q.771 - Report of failure primitives
-record('TC-U-ERROR',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127,
		error :: {local, integer()} | {global, tuple()},
		parameters :: binary() | undefined,
		lastComponent :: boolean()}).

%% reference: Table 13/Q.771 - User rejection primitives
-record('TC-U-REJECT',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127,
		problemCode :: problemCode(),
		lastComponent :: boolean()}).

%% reference: Table 14/Q.771 - TC-CANCEL primitives
-record('TC-L-CANCEL',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127}).
-record('TC-U-CANCEL',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127}).

%% reference: Table 14bis/Q.771 TC-TIMER-RESET primitives
-record('TC-TIMER-RESET',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127}).

%% reference: Table 15/Q.771 - Component sublayer rejection primitives
-record('TC-L-REJECT',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127,
		problemCode :: problemCode(),
		lastComponent :: boolean()}).
-record('TC-R-REJECT',
		{dialogueID :: 0..4294967295,
		invokeID :: -128..127,
		problemCode :: problemCode(),
		lastComponent ::boolean()}).

%% reference: Table 16/Q.771 - Primitive for Abort
-record('TC-P-ABORT',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		dialogueID :: 0..4294967295,
		pAbort :: unrecognizedMessageType | unrecognizedTransactionID
				| badlyFormattedTransactionPortion
				| incorrectTransactionPortion | resourceLimitation
				| abnormalDialogue | noCommonDialoguePortion}).

%%%
%%%  TR-User
%%%
%%% userData is a 'TR-user-data' record
%%%
%%% Q.771 does not provide a distinction between the dialogue portion and 
%%% component portion within the user data parameter.  In 3.1.1 it says
%%%    "a dialogue portion is formatted and sent concatenated with the 
%%%     (sequence of) components(s)."
%%% This is probably due to dialogue handling being added after Q.771 was
%%% first written.  We will define a record for the user data.
-record('TR-user-data',
		{dialoguePortion = asn1_NOVALUE :: binary() | asn1_NOVALUE,
		componentPortion = asn1_NOVALUE :: binary() | asn1_NOVALUE}).

%% reference: Table 18/Q.771 - TR-UNI primitives
-record('TR-UNI',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		destAddress :: sccp:party_address(),
		origAddress :: sccp:party_address(),
		userData :: #'TR-user-data'{}}).

%% reference: Table 19/Q.771 - Primitives for transaction begin
-record('TR-BEGIN',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		destAddress :: sccp:party_address(),
		origAddress :: sccp:party_address(),
		transactionID :: 0..4294967295,
		userData :: #'TR-user-data'{}}).

%% reference: Table 20/Q.771 - Transaction continuation primitives
-record('TR-CONTINUE',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		origAddress :: sccp:party_address(),
		transactionID :: 0..4294967295,
		userData :: #'TR-user-data'{}}).

%% reference: Table 22/Q.771 - TR-END primitives
-record('TR-END',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		transactionID :: 0..4294967295,
		termination :: prearranged | basic | abort,
		userData :: #'TR-user-data'{}}).

%% reference: Table 23/Q.771 - TR-U-ABORT primitives
-record('TR-U-ABORT',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		transactionID :: 0..4294967295,
		userData :: #'TR-user-data'{}}).

%% reference: Table 24/Q.771 - Transaction sublayer abort primitive
-record('TR-P-ABORT',
		{qos :: {SequenceControl :: boolean(), ReturnOption :: boolean()},
		transactionID :: 0..4294967295,
		pAbort :: unrecognizedMessageType | unrecognizedTransactionID
				| badlyFormattedTransactionPortion
				| incorrectTransactionPortion | resourceLimitation}).

%% reference: Table 25/Q.771 - TR-NOTICE primitive
-record('TR-NOTICE',
		{transactionID :: 0..4294967295,
		origAddress :: sccp:party_address(),
		destAddress :: sccp:party_address(),
		reportCause}).

-record(tcap_tco_cb,
		{init = false :: fun() | false,
		handle_call = false :: fun() | false,
		handle_cast = false :: fun() | false,
		handle_info = false :: fun() | false,
		terminate = false :: fun() | false,
		handle_continue = false :: fun() | false,
		send_primitive = false :: fun() | false,
		start_aei = false :: fun() | false,
		code_change = false :: fun() | false,
		format_status = false :: fun() | false,
		extra = [] :: [Args :: term()]}).


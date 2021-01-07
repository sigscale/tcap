%%% tcap.erl
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
%%% @doc Transaction Capabilities Application Part
%%%	<p>This module implements the user API to the TCAP protocol
%%% 	stack application. Transaction Capabilities are ...</p>
%%%
%%% @reference <a href="index.html">TCAP User's Guide</a>
%%%
-module(tcap).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').

%% our published API functions
-export([start/0, stop/0]).
-export([open/3, close/1]).

-type tcap_options() :: [Option :: {variant, Variant :: itu | ansi}].
-export_type([tcap_options/0]).

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

-spec open(TSL, TCU, Args) -> CSL
	when
		TSL :: pid(),
		TCU :: pid(),
		Args :: [term()],
		CSL :: {DHA, CCO},
		DHA :: pid(),
		CCO :: pid().
%% @doc Start a new component sublayer (CSL).
%%
%% 	Called by the TC-User to initialize the TCAP layer for a new
%% 	dialogue.
%%
%% 	`TSL' is the pid returned from a previous call to
%% 	{@link start_tsl/3. start_tsl/3}.
%%
%% 	`TCU' is the pid of the `TC-User'.
%%
%% 	Returns `{DHA, CCO}', the pids of the dialogue handler
%% 	and component coordinator in the component sublayer.
%%
open(TSL, TCU, Args) ->
	gen_server:call(TSL, {start_dialogue, TCU, Args}).

-spec close(TSL) -> ok
	when
		TSL :: pid().
%% @doc Close a TCAP sublayer (TSL).
%%
%% 	`TSL' is the pid returned in a previous call to 
%% 	{@link start_tsl/3. start_tsl/3}.
%%
close(TSL) when is_pid(TSL) ->
	gen_server:call(TSL, close).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------


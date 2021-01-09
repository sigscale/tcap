%% tcap_test_user.erl
%%
-module(tcap_test_user).

-behaviour(tcap_tco_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		send_primitive/2, start_user/3]).

init(_Args) ->
	{ok, []}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'N', _, indication, _} = Primitive, State) ->
	{primitive, Primitive, State}.

send_primitive(_Primitive, _State) ->
	ok.

start_user(_CSL, _DialogueID, _State) ->
	F = fun F() ->
		receive
			_ ->
				F()
		end
	end,
	proc_lib:spawn(F).


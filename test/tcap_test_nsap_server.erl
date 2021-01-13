%% tcap_test_nsap_server.erl
%%
-module(tcap_test_nsap_server).

-behaviour(tcap_tco_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		send_primitive/2, start_user/3]).

-record(state, {ct :: pid()}).

init([CT]) ->
erlang:display({?MODULE, ?LINE, CT}),
	{ok, #state{ct = CT}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'N', _, indication, _} = Primitive, State) ->
	{primitive, Primitive, State};
handle_info(_, State) ->
	{noreply, State}.

send_primitive(Primitive, #state{ct = CT} = _State) ->
	CT ! Primitive.

start_user(_CSL, _DialogueID, #state{ct = CT} = _State) ->
erlang:display({?MODULE, ?LINE, _CSL, _DialogueID, CT}),
	{ok, Fsm} = gen_fsm:start_link(tcap_test_usap_fsm, [[CT]], []),
erlang:display({?MODULE, ?LINE, Fsm}),
	Fsm.


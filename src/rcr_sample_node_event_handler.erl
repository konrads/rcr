-module(rcr_sample_node_event_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event({Event, _}, State) ->
    lager:info("~p event: ~p", [?MODULE, Event]),
    {ok, State}.

handle_call(Call, State) ->
    lager:info("~p call: ~p", [?MODULE, Call]),
    {ok, ok, State}.

handle_info(Info, State) ->
    lager:info("~p info: ~p", [?MODULE, Info]),
    {ok, State}.

terminate(Reason, _State) ->
    lager:info("~p terminate: ~p", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

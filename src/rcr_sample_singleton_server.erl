%%% Sample singleton, handles messages aimed at both leader and non-leader.
-module(rcr_sample_singleton_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([
    start_link/0,
    ping_call/0,
    ping_call/1,
    ping_cast/1,
    ping_info/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(TIMEOUT, 2000).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    rcr_singleton_server:start_link(?MODULE, []).

ping_call() ->
    ping_call(?TIMEOUT).

ping_call(Timeout) ->
    rcr_singleton_server:call(?MODULE, ping, Timeout).

ping_cast(IncludeLeader) ->
    rcr_singleton_server:cast(?MODULE, {ping, IncludeLeader}).

ping_info(IncludeLeader) ->
    rcr_singleton_server:info(?MODULE, {ping, IncludeLeader}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, no_state}.

handle_call(ping, {FromPid, _FromTag}, State) ->
    % always exclude the originator (From node), as well as the leader, otherwise the call will block
    ExcludedNodes = lists:usort([node(), node(FromPid)]),
    lager:info("Call pinged leader ~p, pinging others, excluded nodes (cannot call leader or originator): ~p", [node(), ExcludedNodes]),
    Replies = rcr_singleton_server:broadcall(?MODULE, ping_non_leader, ?TIMEOUT, ExcludedNodes),
    lager:info("Replies = ~p", [Replies]),
    {reply, leader_pinged, State};
handle_call(ping_non_leader, _From, State) ->
    Node = node(),
    lager:info("Call pinged non leader ~p", [Node]),
    {reply, {non_leader_pinged, Node}, State}.

handle_cast({ping, IncludeLeader}, State) ->
    ExcludedNodes = case IncludeLeader of
        true -> [];
        false -> [node()]
    end,
    lager:info("Cast pinged leader ~p, pinging others, excluded nodes: ~p", [node(), ExcludedNodes]),
    rcr_singleton_server:broadcast(?MODULE, ping_non_leader, ExcludedNodes),
    {noreply, State};
handle_cast(ping_non_leader, State) ->
    Node = node(),
    lager:info("Cast pinged non leader ~p", [Node]),
    {noreply, State}.

handle_info({ping, IncludeLeader}, State) ->
    ExcludedNodes = case IncludeLeader of
        true -> [];
        false -> [node()]
    end,
    lager:info("Info pinged leader ~p, pinging others, excluded nodes: ~p", [node(), ExcludedNodes]),
    rcr_singleton_server:broadinfo(?MODULE, ping_non_leader, ExcludedNodes),
    {noreply, State};
handle_info(ping_non_leader, State) ->
    Node = node(),
    lager:info("Info pinged non leader ~p", [Node]),
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

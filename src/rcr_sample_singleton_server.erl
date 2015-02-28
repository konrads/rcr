%%% Sample singleton server, handles messages aimed at both leader and non-leader.
%%% Note: 'send_ping' reaches singleton, who distributes 'ping' to individual nodes
%%%       (including himself).
-module(rcr_sample_singleton_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([
    start_link/0,
    ping_call/0,
    ping_cast/0,
    ping_info/0]).

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
    rcr_singleton_server:start_link(?MODULE, ?MODULE, []).

ping_call() ->
    rcr_singleton_server:call(?MODULE, send_ping).

ping_cast() ->
    rcr_singleton_server:cast(?MODULE, send_ping).

ping_info() ->
    rcr_singleton_server:info(?MODULE, send_ping).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, no_state}.

handle_call(send_ping, From, State) ->
    spawn_link(fun() ->
        Nodes = rcr_util:get_cluster_nodes(),
        lager:info("Call pinged leader ~p, pinging nodes: ~p", [self(), Nodes]),
        Replies = rcr_singleton_server:broadcall(Nodes, ?MODULE, ping, ?TIMEOUT),
        lager:info("Replies = ~p", [Replies]),
        rcr_singleton_server:reply(From, Replies)
    end),
    {noreply, State};
handle_call(ping, _From, State) ->
    Node = node(),
    lager:info("Call pinged non leader ~p", [Node]),
    {reply, {pinged, Node}, State}.

handle_cast(send_ping, State) ->
    Nodes = rcr_util:get_cluster_nodes(),
    lager:info("Cast pinged leader ~p, pinging nodes: ~p", [self(), Nodes]),
    rcr_singleton_server:broadcast(Nodes, ?MODULE, ping),
    {noreply, State};
handle_cast(ping, State) ->
    lager:info("Cast pinged non leader ~p", [node()]),
    {noreply, State}.

handle_info(send_ping, State) ->
    Nodes = rcr_util:get_cluster_nodes(),
    lager:info("Info pinged leader ~p, pinging nodes: ~p", [self(), Nodes]),
    rcr_singleton_server:broadinfo(Nodes, ?MODULE, ping),
    {noreply, State};
handle_info(ping, State) ->
    lager:info("Info pinged non leader ~p", [node()]),
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

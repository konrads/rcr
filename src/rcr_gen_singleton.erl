%%% Cluster-wide singleton implementation, decorates another gen_server.
%%% Allows for singleton and cluster-wide operations.
%%% - decorates singleton requests/replies with singleton_req/broad_reply
%%% - decorates broadcast requests/replies with broad_req/broad_reply
%%% For singleton operations, funnels all ops to cluster leader.
%%% broadcast/broadcall/broadinfo contact all of the cluster nodes, does not check if they're reachable...
-module(rcr_gen_singleton).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    call/2,
    call/3,
    cast/2,
    info/2,
    broadcall/3,
    broadcall/4,
    broadcast/3,
    broadinfo/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(TIMEOUT, 5000).
-record(rcr_singleton_server_state, {cb_state, server_cb}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ServerCb, InitState) ->
    gen_server:start_link({local, ServerCb}, ?MODULE, [ServerCb, InitState], []).

call(ServerRef, Request) ->
    call(ServerRef, Request, ?TIMEOUT).
call(ServerRef, Request, Timeout) ->
    {singleton_reply, Reply} = gen_server:call(ServerRef, {singleton_req, Request, Timeout}, Timeout),
    Reply.

cast(ServerRef, Request) ->
    gen_server:cast(ServerRef, {singleton_req, Request}).

info(ServerRef, Request) ->
    ServerRef ! {singleton_req, Request},
    ok.

broadcall(ServerCb, Request, ExcludedNodes) ->
    broadcall(ServerCb, Request, ?TIMEOUT, ExcludedNodes).
broadcall(ServerCb, Request, Timeout, ExcludedNodes) ->
    [
        begin
            {broad_resp, Reply} = gen_server:call({ServerCb, Node}, {broad_req, Request}, Timeout),
            Reply
        end
        || Node <- get_cluster_nodes(ExcludedNodes)
    ].

broadcast(ServerCb, Request, ExcludedNodes) ->
    [ gen_server:cast({ServerCb, Node}, {broad_req, Request}) || Node <- get_cluster_nodes(ExcludedNodes) ].

broadinfo(ServerCb, Request, ExcludedNodes) ->
    [
        begin
            {ServerCb, Node} ! {broad_req, Request},
            ok
        end
        || Node <- get_cluster_nodes(ExcludedNodes)
    ].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ServerCb, CbState0]) ->
    case ServerCb:init(CbState0) of
        {ok, CbState} -> {ok, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}};
        Other -> Other
    end.

handle_call({singleton_req, Request, Timeout}=QfRequest, From, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State) ->
    ThisNode = node(),
    case riak_governor:get_cluster_leader() of
        {ok, ThisNode} ->
            case ServerCb:handle_call(Request, From, CbState) of
                {reply,Reply,CbState2} -> {reply,{singleton_reply,Reply},State#rcr_singleton_server_state{cb_state=CbState2}};
                {reply,Reply,CbState2,TimeoutOrHibernate} -> {reply,{singleton_reply,Reply},State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
                {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
                {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
                {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}};
                {stop,Reason,Reply,CbState2} -> {stop,Reason,Reply,State#rcr_singleton_server_state{cb_state=CbState2}}
            end;
        {ok, Leader} ->
            Reply = gen_server:call({ServerCb, Leader}, QfRequest, Timeout),
            {reply,Reply,State};
        {error, no_leader}=E ->
            {reply,E,State}
    end;
handle_call({broad_req, Request}, From, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State) ->
    case ServerCb:handle_call(Request, From, CbState) of
        {reply,Reply,CbState2} -> {reply,{broad_resp,Reply},State#rcr_singleton_server_state{cb_state=CbState2}};
        {reply,Reply,CbState2,TimeoutOrHibernate} -> {reply,{broad_resp,Reply},State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
        {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
        {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
        {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}};
        {stop,Reason,Reply,CbState2} -> {stop,Reason,Reply,State#rcr_singleton_server_state{cb_state=CbState2}}
    end.

handle_cast({singleton_req, Request}=QfRequest, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State) ->
    ThisNode = node(),
    case riak_governor:get_cluster_leader() of
        {ok, ThisNode} ->
            case ServerCb:handle_cast(Request, CbState) of
                {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
                {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
                {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}}
            end;
        {ok, Leader} ->
            gen_server:cast({ServerCb, Leader}, QfRequest),
            {noreply,State};
        {error, no_leader} ->
            {noreply,State}
    end;
handle_cast({broad_req, Request}, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State) ->
    case ServerCb:handle_cast(Request, CbState) of
        {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
        {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
        {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}}
    end.

handle_info({singleton_req, Request}=QfRequest, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State) ->
    ThisNode = node(),
    case riak_governor:get_cluster_leader() of
        {ok, ThisNode} ->
            case ServerCb:handle_info(Request, CbState) of
                {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
                {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
                {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}}
            end;
        {ok, Leader} ->
            {ServerCb, Leader} ! QfRequest,
            {noreply,State};
        {error, no_leader} ->
            {noreply,State}
    end;
handle_info({broad_req, Request}, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State) ->
    case ServerCb:handle_info(Request, CbState) of
        {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
        {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
        {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}}
    end.

terminate(Reason, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}) ->
    ServerCb:terminate(Reason, CbState).

code_change(OldVsn, #rcr_singleton_server_state{cb_state=CbState, server_cb=ServerCb}=State, Extra) ->
    case ServerCb:code_change(OldVsn, CbState, Extra) of
        {ok, CbState2} -> {ok, State#rcr_singleton_server_state{cb_state=CbState2}};
        {error, _R}=E -> E
    end.

%% internals
get_cluster_nodes(ExcludedNodes) ->
    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
    AllNodes = lists:usort(riak_core_ring:all_members(Ring)),
    AllNodes -- ExcludedNodes.

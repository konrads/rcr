%%% Cluster-wide server singleton implementation, decorates a gen_server.
%%% Allows for singleton and cluster-wide operations.
%%% - decorates singleton requests/replies with singleton_req/broad_reply
%%% - decorates broadcast requests/replies with broad_req/broad_reply
%%% For singleton operations, funnels all ops to cluster leader.
%%% broadcast/broadcall/broadinfo contact all of the cluster nodes, does not check if they're reachable...
-module(rcr_singleton_server).

-behaviour(gen_server).

%% API
-export([
    start_link/3,
    call/2,
    call/3,
    cast/2,
    info/2,
    broadcall/3,
    broadcall/4,
    broadcast/3,
    broadinfo/3,
    reply/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(TIMEOUT, 5000).
-record(rcr_singleton_server_state, {server_id, cb_state, server_cb}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ServerId, ServerCb, InitState) ->
    gen_server:start_link({local, ServerId}, ?MODULE, [ServerId, ServerCb, InitState], []).

call(ServerRef, Request) ->
    call(ServerRef, Request, ?TIMEOUT).
call(ServerRef, Request, Timeout) ->
    case gen_server:call(ServerRef, {singleton_req, Request, Timeout}, Timeout) of
        {singleton_reply, Reply} -> Reply;
        {error, no_leader}=E -> E
    end.

cast(ServerRef, Request) ->
    gen_server:cast(ServerRef, {singleton_req, Request}).

info(ServerRef, Request) ->
    ServerRef ! {singleton_req, Request},
    ok.

broadcall(Nodes, ServerId, Request) ->
    broadcall(Nodes, ServerId, Request, ?TIMEOUT).
broadcall(Nodes, ServerId, Request, Timeout) ->
    [
        begin
            {broad_resp, Reply} = gen_server:call({ServerId, Node}, {broad_req, Request}, Timeout),
            Reply
        end
        || Node <- Nodes
    ].

broadcast(Nodes, ServerId, Request) ->
    [ gen_server:cast({ServerId, Node}, {broad_req, Request}) || Node <- Nodes ].

broadinfo(Nodes, ServerId, Request) ->
    [
        begin
            {ServerId, Node} ! {broad_req, Request},
            ok
        end
        || Node <- Nodes
    ].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ServerId, ServerCb, CbState0]) ->
    case ServerCb:init(CbState0) of
        {ok, CbState} -> {ok, #rcr_singleton_server_state{server_id= ServerId, cb_state=CbState, server_cb=ServerCb}};
        Other -> Other
    end.

handle_call({singleton_req, Request, Timeout}=QfRequest, From, #rcr_singleton_server_state{server_id=ServerId, cb_state=CbState, server_cb=ServerCb}=State) ->
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
            % need to spawn another call to prevent this node blocking
            spawn_link(fun() ->
                Reply = gen_server:call({ServerId, Leader}, QfRequest, Timeout),
                gen_server:reply(From, Reply)
            end),
            {noreply,State};
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

handle_cast({singleton_req, Request}=QfRequest, #rcr_singleton_server_state{server_id=ServerId, cb_state=CbState, server_cb=ServerCb}=State) ->
    ThisNode = node(),
    case riak_governor:get_cluster_leader() of
        {ok, ThisNode} ->
            case ServerCb:handle_cast(Request, CbState) of
                {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
                {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
                {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}}
            end;
        {ok, Leader} ->
            gen_server:cast({ServerId, Leader}, QfRequest),
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

handle_info({singleton_req, Request}=QfRequest, #rcr_singleton_server_state{server_id=ServerId, cb_state=CbState, server_cb=ServerCb}=State) ->
    ThisNode = node(),
    case riak_governor:get_cluster_leader() of
        {ok, ThisNode} ->
            case ServerCb:handle_info(Request, CbState) of
                {noreply,CbState2} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2}};
                {noreply,CbState2,TimeoutOrHibernate} -> {noreply,State#rcr_singleton_server_state{cb_state=CbState2},TimeoutOrHibernate};
                {stop,Reason,CbState2} -> {stop,Reason,State#rcr_singleton_server_state{cb_state=CbState2}}
            end;
        {ok, Leader} ->
            {ServerId, Leader} ! QfRequest,
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

% for Leaders that want to delay the reply on call, ie. return {noreply,...} first, then call this function
reply(From, Reply) ->
    gen_server:reply(From, {singleton_reply, Reply}).

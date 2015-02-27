%%% Sample fsm singleton, handles messages aimed at both leader and non-leader.
-module(rcr_singleton_cmd).

-behaviour(gen_server).

%% api
-export([
    start_link/1,
    cmd/2,
    cmd/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(rcr_singleton_cmd_state, {cmd_cb}).

-define(TIMEOUT, 3000).

-callback prepare(any(), any()) -> ok | {ok, any()} | error | {error | any()}.
-callback commit(reference(), any()) -> ok | {ok, any()} | error | {error | any()}.
-callback rollback(reference(), any()) -> ok | {ok, any()} | error | {error | any()}.

%%%===================================================================
%%% API
%%%===================================================================
start_link(CmdCb) ->
    rcr_singleton_server:start_link(CmdCb, ?MODULE, [CmdCb]).

cmd(CmdCb, Cmd) ->
    cmd(CmdCb, Cmd, ?TIMEOUT).

cmd(CmdCb, Cmd, Timeout) ->
    rcr_singleton_server:call(CmdCb, {cmd, Cmd}, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([CmdCb]) ->
    {ok, #rcr_singleton_cmd_state{cmd_cb=CmdCb}}.

%% executed by the leader
handle_call({cmd, Cmd}, From, #rcr_singleton_cmd_state{cmd_cb=CmdCb}=State) ->
    gen_fsm:start_link(rcr_2_phase_commit, [From, Cmd, rcr_util:get_cluster_nodes(), CmdCb], []),
    {noreply, State};

%% executed by all nodes
handle_call({prepare, Ref, Cmd}, _From, #rcr_singleton_cmd_state{cmd_cb=CmdCb}=State) ->
    {reply, CmdCb:prepare(Ref, Cmd), State};
handle_call({commit, Ref, Cmd}, _From, #rcr_singleton_cmd_state{cmd_cb=CmdCb}=State) ->
    {reply, CmdCb:commit(Ref, Cmd), State};
handle_call({rollback, Ref, Cmd}, _From, #rcr_singleton_cmd_state{cmd_cb=CmdCb}=State) ->
    {reply, CmdCb:rollback(Ref, Cmd), State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

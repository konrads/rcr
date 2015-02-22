-module(rcr_sample_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-record(state, {partition, vnode_config}).

%% API
start_vnode(Index) ->
    rcr_util:get_vnode_pid(rcr_util:get_vnode_config(?MODULE), Index).

init([Partition]) ->
    VnodeConfig = rcr_util:get_vnode_config(?MODULE),
    lager:info("Starting ~s on partition ~p", [rcr_fmt:to_short_str(VnodeConfig), Partition]),
    State = #state {
        partition=Partition,
        vnode_config=VnodeConfig},
    {ok, State}.

handle_command(ping, _Sender, #state{partition=Partition, vnode_config=VnodeConfig}=State) ->
    lager:info("Pong ok ~s", [rcr_fmt:to_str(VnodeConfig)]),
    {reply, {pong, Partition, node()}, State};
handle_command(_Message, _Sender, State) ->
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    ok.

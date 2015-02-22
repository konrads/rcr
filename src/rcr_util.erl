%% Facade to hide riak_core's ugliness.
-module(rcr_util).

-include("rcr_int.hrl").

-export([
    command/6,
    get_vnode_pid/2,
    get_vnode_config/1
]).

command(#vnode_config{service_id=ServiceId, vnode_master=VnodeMaster},
        {Bucket, Key},
        N,
        NodeType,
        CommandType,
        Command) when is_binary(Bucket), is_binary(Key), is_integer(N) ->
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    PrefList = case NodeType of
        primary ->
            PrefList0 = riak_core_apl:get_primary_apl(DocIdx, N, ServiceId),
            [{Index, Node} || {{Index, Node}, _Type} <- PrefList0];
        all ->
            riak_core_apl:get_apl(DocIdx, N, ServiceId)
    end,
    case PrefList of
        [] -> throw(empty_preflist);
        _ -> ignore
    end,
    case CommandType of
        async -> riak_core_vnode_master:command(PrefList, Command, VnodeMaster);
        sync -> [riak_core_vnode_master:sync_command(IdxNode, Command, VnodeMaster) || IdxNode <- PrefList];
        sync_spawn -> [riak_core_vnode_master:sync_spawn_command(IdxNode, Command, VnodeMaster) || IdxNode <- PrefList]
    end.

get_vnode_pid(#vnode_config{vnode=Vnode}, Index) ->
    riak_core_vnode_master:get_vnode_pid(Index, Vnode).

get_vnode_config(VnodeModule) ->
    {ok, #rcr_config{vnode_configs=VnodeConfigs}} = application:get_env(rcr, config),
    lists:keyfind(VnodeModule, #vnode_config.vnode, VnodeConfigs).

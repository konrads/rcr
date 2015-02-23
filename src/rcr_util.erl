%% Facade to hide riak_core's ugliness.  Collects useful functionality.
-module(rcr_util).

-include("rcr_int.hrl").

-export([
    command/6,
    get_vnode_pid/2,
    get_vnode_config/1,
    validate/1,
    member_status/0
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

%% Potentially work in progress, perhaps other validation can be of use
validate(#vnode_config{vnode=Vnode, vnode_sup=VnodeSup}) ->
    ExpVnodeSup = list_to_atom(atom_to_list(Vnode) ++ "_sup"),
    case ExpVnodeSup of
        VnodeSup -> ok;
        _ -> throw({invalid_vnode_sup, {is, VnodeSup}, {must_be, ExpVnodeSup}})
    end.

member_status() ->
    riak_core_console:member_status([]).

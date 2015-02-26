%%% @doc Utils, grouped into:
%%% - rcr utils for dealing with #vnode_config{}
%%% - riak_core wrappers, hiding the complexity/inconsistencies
%%% - other utils for eg. node connect/reconnect, recon tracing
-module(rcr_util).

-include("rcr_int.hrl").

-export([
    command/6,
    get_vnode_pid/2,
    get_vnode_config/1,
    validate/1,
    disconnect/0,
    disconnect/1,
    reconnect/0,
    reconnect/2,
    member_status/0,
    get_cluster_nodes/0,
    recon/2,
    recon/3
]).

%%%===================================================================
%%% rcr utils - related to #vnode_config{}
%%%===================================================================
%% @doc Potentially work in progress, perhaps need other validations
validate(#vnode_config{vnode=Vnode, vnode_sup=VnodeSup}) ->
    ExpVnodeSup = list_to_atom(atom_to_list(Vnode) ++ "_sup"),
    case ExpVnodeSup of
        VnodeSup -> ok;
        _ -> throw({invalid_vnode_sup, {is, VnodeSup}, {must_be, ExpVnodeSup}})
    end.

get_vnode_config(VnodeModule) ->
    {ok, #rcr_config{vnode_configs=VnodeConfigs}} = application:get_env(rcr, config),
    lists:keyfind(VnodeModule, #vnode_config.vnode, VnodeConfigs).

%%%===================================================================
%%% riak_core wrappers - simplifications, utilizing #vnode_config
%%%===================================================================
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

member_status() ->
    riak_core_console:member_status([]).

get_cluster_nodes() ->
    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
    lists:usort(riak_core_ring:all_members(Ring)).

%%%===================================================================
%%% network utils - eg. disconnect/connect
%%% Works on the level of erlang's connectivity, not riak_core slustering.
%%% Defaults:
%%% Node='rcr1@127.0.0.1', Cookie=rcr.
%%%===================================================================
disconnect() ->
    disconnect('rcr1@127.0.0.1').

disconnect(Node) ->
    rpc:call(Node, error_logger, error_msg, ["Disconnect from ~p - lager loglevel => critical", [node()]]),
    % rpc:call(Node, gen_event, delete_handler, [error_logger, error_logger_lager_h, delete]),
    rpc:call(Node, lager, set_loglevel, [lager_console_backend, critical]),
    Cookie = erlang:get_cookie(),
    erlang:set_cookie(node(), fake_cookie),
    % gen_event:delete_handler(error_logger, error_logger_lager_h, delete),
    lager:set_loglevel(lager_console_backend, critical),
    erlang:disconnect_node(Node),
    Cookie.

reconnect() ->
    reconnect('rcr1@127.0.0.1', rcr).

reconnect(Node, Cookie) ->
    erlang:set_cookie(node(), Cookie),
    pong = net_adm:ping(Node),
    % gen_event:add_handler(error_logger, error_logger_lager_h, [info]),
    lager:set_loglevel(lager_console_backend, info),
    % rpc:call(Node, gen_event, add_handler, [error_logger, error_logger_lager_h, [info]]),
    rpc:call(Node, lager, set_loglevel, [lager_console_backend, info]),
    rpc:call(Node, error_logger, error_msg, ["Reconnect to ~p - lager loglevel => info", [node()]]).

%%%===================================================================
%%% rcon tracing/debugging utils
%%%===================================================================
recon(M, F) ->
    recon(M, F, '_').

recon(M, F, Args) ->
    recon_trace:calls({M, F, [{Args, [], [{return_trace}]}]}, {100, 10}).

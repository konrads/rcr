-module(rcr_sample_client).

-include("rcr_int.hrl").

-export([
    ping_vnode/0,
    ping_vnode/1,
    ping_vnode/3,
    ping_call_singleton_server/0,
    ping_cast_singleton_server/1,
    ping_info_singleton_server/1]).

%% could fetch that from sys.config?
-define(client_vnode_config, #vnode_config{service_id=rcr_sample, vnode_master=rcr_sample_vnode_master}).

%%%===================================================================
%%% vnode pings
%%%===================================================================
ping_vnode() -> ping_vnode(1).

ping_vnode(N) ->
    rcr_util:command(?client_vnode_config, {<<"ping">>, term_to_binary(now())}, N, primary, sync_spawn, ping).

ping_vnode(Bucket, Key, N) ->
    rcr_util:command(?client_vnode_config, {?format_bin("~p", [Bucket]), ?format_bin("~p", [Key])}, N, primary, sync_spawn, ping).


%%%===================================================================
%%% server singleton pings
%%%===================================================================
ping_call_singleton_server() ->
    rcr_sample_singleton_server:ping_call().

ping_cast_singleton_server(IncludeLeader) ->
    rcr_sample_singleton_server:ping_cast(IncludeLeader).

ping_info_singleton_server(IncludeLeader) ->
    rcr_sample_singleton_server:ping_info(IncludeLeader).

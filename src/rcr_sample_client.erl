-module(rcr_sample_client).

-include("rcr_int.hrl").

-export([
    ping_vnode/0,
    ping_vnode/1,
    ping_vnode/3,
    ping_singleton_call/0,
    ping_singleton_cast/1,
    ping_singleton_info/1]).

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
%%% singleton pings
%%%===================================================================
ping_singleton_call() ->
    rcr_sample_singleton:ping_call().

ping_singleton_cast(IncludeSelf) ->
    rcr_sample_singleton:ping_cast(IncludeSelf).

ping_singleton_info(IncludeSelf) ->
    rcr_sample_singleton:ping_info(IncludeSelf).

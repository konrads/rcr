-module(rcr_sample_client).

-include("rcr_int.hrl").

-export([ping/0, ping/1]).

%% could fetch that from sys.config?
-define(client_vnode_config, #vnode_config{service_id=rcr_sample_service, vnode_master=rcr_sample_vnode_master}).

ping() -> ping(1).

ping(N) ->
    rcr_util:command(?client_vnode_config, {<<"ping">>, term_to_binary(now())}, N, primary, sync_spawn, ping).

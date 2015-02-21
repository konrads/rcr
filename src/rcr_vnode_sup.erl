-module(rcr_vnode_sup).

-behaviour(supervisor).

-include("rcr_int.hrl").

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args),
    {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    Config = application:get_env(rcr, config, #rcr_config{}),
    start_link(Config#rcr_config.vnode_mappings).

start_link(VnodeMappings) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, VnodeMappings).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init(VnodeMappings) when is_list(VnodeMappings) ->
    VnodeMasters = [
        {VnodeMaster, {riak_core_vnode_master, start_link, [Vnode]}, permanent, 5000, worker, [riak_core_vnode_master]}
        ||
        #rcr_vnode_mapping{vnode_master=VnodeMaster, vnode=Vnode} <- VnodeMappings
    ],
    {ok, {{one_for_one, 5, 10},
          VnodeMasters % ++ SingletonMappings - TBD
         }}.

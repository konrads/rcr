-module(rcr_app).
-behaviour(application).

-include("rcr_int.hrl").

%% API
-export([start/1]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    % obtain the following settings from hardcoded, or env?
    Config = application:get_env(rcr, config, #rcr_config{}),
    start(Config).


start(#rcr_config{vnode_mappings=VnodeMappings, ring_event_handler=RingEventHandler, node_event_handler=NodeEventHandler}) ->
    lager:info("\nStarting VnodeMappings: ~p\nRing Event Handler: ~p\nNode Event Handler: ~p", [VnodeMappings, RingEventHandler, NodeEventHandler]),
    case rcr_vnode_sup:start_link(VnodeMappings) of
        {ok, Pid} ->
            % register vnodes
            [
                begin
                    ok = riak_core:register([{vnode_module, Vnode}]),
                    ok = riak_core_node_watcher:service_up(Id, self())
                end
                || #rcr_vnode_mapping{id=Id, vnode=Vnode} <- VnodeMappings
            ],
            % register ring handlers
            case RingEventHandler of
                undefined -> ignore;
                _ -> ok = riak_core_ring_events:add_guarded_handler(RingEventHandler, [])
            end,
            case NodeEventHandler of
                undefined -> ignore;
                _ -> ok = riak_core_node_watcher_events:add_guarded_handler(NodeEventHandler, [])
            end,
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.

stop(_State) ->
    ok.

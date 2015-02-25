%% Template application.  start/1 can be re-used other applications.  NOTE: validation step (to ensure riak_core assumptions are met).
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
    % FIXME: following is bit dodgy, but serves as an illustration
    case start(Config) of
        {ok, Pid} ->
            {ok, _AnotherPid} = rcr_sample_sup:start_link(),
            {ok, Pid};
        {error, _R}=E -> E
    end.


start(#rcr_config{vnode_configs=VnodeConfigs, ring_event_handler=RingEventHandler, node_event_handler=NodeEventHandler}) ->
    VnodeConfigsDesc = string:join(
        ["        - " ++ rcr_fmt:to_str(VC) || VC <- VnodeConfigs],
        "\n"),
    lager:info("Starting rcr:
    - vnode mappings:\n~s
    - ring Event Handler: ~p
    - node Event Handler: ~p", [VnodeConfigsDesc, RingEventHandler, NodeEventHandler]),
    case rcr_sup:start_link(VnodeConfigs) of
        {ok, Pid} ->
            % register vnodes
            [
                begin
                    ok = rcr_util:validate(VC),
                    ok = riak_core:register([{vnode_module, Vnode}]),
                    ok = riak_core_node_watcher:service_up(ServiceId, self())
                end
                || #vnode_config{service_id=ServiceId, vnode=Vnode}=VC <- VnodeConfigs
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

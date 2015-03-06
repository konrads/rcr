-record(vnode_config, {
    service_id   :: atom(),
    vnode        :: module(),
    vnode_master :: atom()
}).
-type vnode_config() :: #vnode_config{}.

-record(rcr_config, {
    vnode_configs = []  :: [#vnode_config{}],
    ring_event_handler  :: module(),
    node_event_handler  :: module()
}).
-type rcr_config() :: #rcr_config{}.

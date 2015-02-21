-record(rcr_vnode_mapping, {
    id           :: atom(),
    vnode        :: module(),
    vnode_sup    :: module(),
    vnode_master :: atom()
}).
-type rcr_vnode_mapping() :: #rcr_vnode_mapping{}.

-record(rcr_config, {
    vnode_mappings = [] :: [#rcr_vnode_mapping{}],
    ring_event_handler  :: module(),
    node_event_handler  :: module()
}).
-type rcr_config() :: #rcr_config{}.

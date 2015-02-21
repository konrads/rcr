-include("rcr.hrl").

-define(sample_config, #rcr_config{
    vnode_mappings = [#rcr_vnode_mapping{service=rcr_sample_service, vnode=rcr_sample_vnode, vnode_sup=rcr_sample_vnode_sup, vnode_master=rcr_sample_vnode_master}],
    ring_event_handler = undefined,
    node_event_handler = undefined
}).

-define(format(S, P), lists:flatten(io_lib:format(S, P))).
-define(format_bin(S, P), list_to_binary(lists:flatten(io_lib:format(S, P)))).
-define(stack_trace, try throw(stack_trace) catch throw:stack_trace -> erlang:get_stacktrace() end).
-define(recon(M,F), recon_trace:calls({M, F, [{'_', [], [{return_trace}]}]}, {100, 10})).

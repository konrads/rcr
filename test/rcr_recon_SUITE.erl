-module(rcr_recon_SUITE).

-compile(export_all).

all() -> [ {group, test_ops} ].

groups() ->
    [{test_ops, [], [
        test_ops
    ]}].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetramp, 2000}].

% using sleeps for visual inspection - not necessary for state checking
test_ops(_Config) ->
    ok = rcr_recon:clear(),
    % populate
    undefined = whereis(rcr_recon),
    ok = rcr_recon:add(sets, new),
    ok = rcr_recon:add(sets, union),
    ok = rcr_recon:add(ets, new),
    % attempt to add bogus call
    {error,{invalid_call,{ets,bogus,'_'}}} = rcr_recon:add(ets, bogus),
    [{ets,new,'_'},{sets,new,'_'},{sets,union,'_'}] = rcr_recon:list(),
    % for visual inspection, check if trace is showing...
    show_trace(trace_expected, "added"),
    % pause and resume
    rcr_recon:pause(),
    [{ets,new,'_'},{sets,new,'_'},{sets,union,'_'}] = rcr_recon:list(),
    show_trace(no_trace_expected, "paused"),
    rcr_recon:resume(),
    [{ets,new,'_'},{sets,new,'_'},{sets,union,'_'}] = rcr_recon:list(),
    show_trace(trace_expected, "resumed"),
    % remove and clear
    [{sets,new,'_'},{sets,union,'_'}] = rcr_recon:rm(sets),
    [{ets,new,'_'}] = rcr_recon:list(),
    ok = rcr_recon:clear(),
    [] = rcr_recon:list(),
    ok = rcr_recon:clear(),
    % wait till killed...
    show_trace(no_trace_expected, "removed"),
    undefined = whereis(rcr_recon).

show_trace(trace_expected, Action) ->
    ct:pal("Action: ~s, expecting trace calls...", [Action]),
    sets:union(sets:new(), sets:new()), 
    timer:sleep(10);
show_trace(no_trace_expected, Action) ->
    ct:pal("Action: ~s, expecting *NO* trace calls...", [Action]),
    sets:union(sets:new(), sets:new()), 
    timer:sleep(10).

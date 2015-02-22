%% Test of the in-process FSM, where the state machine is that of a door,
%% with 2 states: opened & closed, and 2 events: open & close
-module(rcr_fsm_SUITE).

-inslude("../include/rcr.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [ {group, test_fsm} ].

groups() ->
    [{test_fsm, [], [
        test_transitions_ok,
        test_invalid_event,
        test_invalid_params,
        test_badarity
        ]}].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 5000}].

init_per_testcase(_TestCase, Config) ->
    FsmState = rcr_fsm:new(
        [
            {closed, fun(Operations, open, [Key]) when is_atom(Key) -> {Operations++[{opened_by, Key}], opened, {opening_with, Key}};
                        (Operations, close, [Key]) when is_atom(Key) -> {Operations, closed, {error, cannot_close_closed}}
                     end},
            {opened, fun(Operations, open, [Key]) when is_atom(Key) -> {Operations, opened, {error, cannot_open_opened}};
                        (Operations, close, [Key]) when is_atom(Key) -> {Operations++[{closed_by, Key}], closed, {closing_with, Key}};
                        (_, explode, _) -> throw(booom)
                     end}
        ],
        []  %% empty operation list
    ),
    [{fsm_state, FsmState} | Config].

test_transitions_ok(Config) ->
    FsmState = ?config(fsm_state, Config),
    {FsmState2, {opening_with, my_key}} = rcr_fsm:transition(FsmState, open, [my_key]),
    {FsmState2, {error, cannot_open_opened}} = rcr_fsm:transition(FsmState2, open, [my_key]),
    {FsmState3, {closing_with, your_key}} = rcr_fsm:transition(FsmState2, close, [your_key]),
    {FsmState3, {error, cannot_close_closed}} = rcr_fsm:transition(FsmState3, close, [your_key]),
    [{opened_by, my_key}, {closed_by, your_key}] = rcr_fsm:get_ctx(FsmState3).

test_invalid_event(Config) ->
    FsmState = ?config(fsm_state, Config),
    try
        rcr_fsm:transition(FsmState, bogus_event, [a_key]),
        throw(shouldnt_succeed)
    catch error:function_clause -> ok
    end.

test_invalid_params(Config) ->
    FsmState = ?config(fsm_state, Config),
    try
        rcr_fsm:transition(FsmState, open, ["key as string"]),
        throw(shouldnt_succeed)
    catch error:function_clause -> ok
    end.

% not getting error:badarity because all params are represented as a list
test_badarity(Config) ->
    FsmState = ?config(fsm_state, Config),
    try
        rcr_fsm:transition(FsmState, open, [invalid, more, than, one, param]),
        throw(shouldnt_succeed)
    catch error:function_clause -> ok
    end.

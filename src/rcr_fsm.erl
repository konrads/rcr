%% Simple in-process fsm (unlike gen_fsm) that takes a map of {State, TransitionFun}, where TransitionFun
-module(rcr_fsm).

-include("rcr.hrl").

-export([
    new/2,
    transition/3,
    get_ctx/1]).

-type param_list()         :: list().
-type result()             :: any().
-type fsm_ctx()            :: any().
-type fsm_event()          :: atom().
-type fsm_state()          :: atom().
-type fsm_transition_fun() :: fun((fsm_ctx(), fsm_event(), param_list()) -> {fsm_ctx(), fsm_state(), result()}).

-record(rcr_fsm_state, {
    current     :: atom(),
    ctx         :: any(),
    transitions :: list()
}).
-type rcr_fsm_state() :: #rcr_fsm_state{}.

-spec new(Transitions, fsm_ctx()) -> rcr_fsm_state() when
    Transitions :: [{fsm_state(), fsm_transition_fun()}].
new(Transitions, Ctx) ->
    [{Current, _Fun} | _] = Transitions,
    #rcr_fsm_state{current=Current, ctx=Ctx, transitions=Transitions}.

-spec transition(rcr_fsm_state(), fsm_event(), param_list()) -> {rcr_fsm_state(), result()}.
transition(#rcr_fsm_state{current=Current, ctx=Ctx, transitions=Transitions}=FsmState, Event, Args) when is_list(Args) ->
    Fun = proplists:get_value(Current, Transitions),
    AllArgs = [Ctx, Event, Args],
    {Ctx2, Next, Result} = apply(Fun, AllArgs),
    FsmState2 = FsmState#rcr_fsm_state{current=Next, ctx=Ctx2},
    {FsmState2, Result}.

-spec get_ctx(rcr_fsm_state()) -> fsm_state().
get_ctx(#rcr_fsm_state{ctx=Ctx}) -> Ctx.

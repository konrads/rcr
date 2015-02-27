-module(rcr_2_phase_commit).

-behaviour(gen_fsm).

%% gen_fsm states
-export([prepare/2,
    commit/2,
    rollback/2,
    respond/2]).

%% gen_fsm callbacks
-export([init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-record(rcr_2_phase_commit_state, {originator, cmd, nodes, server_id, ref, prepare_result, commit_result, rollback_result}).

-define(TIMEOUT, 10000).
-define(singleton_reply(Reply), {singleton_reply, Reply}).

%% Callbacks
init([Originator, Cmd, Nodes, ServerId]) ->
    {ok, prepare, #rcr_2_phase_commit_state{originator=Originator, cmd=Cmd, nodes=Nodes, server_id=ServerId, ref=make_ref()}, 0}.

prepare(timeout, #rcr_2_phase_commit_state{ref=Ref, cmd=Cmd, nodes=Nodes, server_id=ServerId}=State) ->
    Results = rcr_singleton_server:broadcall(Nodes, ServerId, {prepare, Ref, Cmd}, ?TIMEOUT),
    Results2 = simplify_results(Results),
    State2 = State#rcr_2_phase_commit_state{prepare_result=Results2},
    case Results2 of
        ok -> {next_state,commit,State2,0};
        {ok, _} -> {next_state,commit,State2,0};
        error -> {next_state,rollback,State2,0};
        {error, _} -> {next_state,rollback,State2,0}
    end.

commit(timeout, #rcr_2_phase_commit_state{ref=Ref, cmd=Cmd, nodes=Nodes, server_id=ServerId}=State) ->
    Results = rcr_singleton_server:broadcall(Nodes, ServerId, {commit, Ref, Cmd}, ?TIMEOUT),
    Results2 = simplify_results(Results),
    State2 = State#rcr_2_phase_commit_state{commit_result=Results2},
    case Results2 of
        ok -> {next_state,respond,State2,0};
        {ok, _} -> {next_state,respond,State2,0};
        error -> {next_state,rollback,State2,0};
        {error, _} -> {next_state,rollback,State2,0}
    end.

rollback(timeout, #rcr_2_phase_commit_state{ref=Ref, cmd=Cmd, nodes=Nodes, server_id=ServerId}=State) ->
    Results = rcr_singleton_server:broadcall(Nodes, ServerId, {rollback, Ref, Cmd}, ?TIMEOUT),
    Results2 = simplify_results(Results),
    State2 = State#rcr_2_phase_commit_state{rollback_result=Results2},
    {next_state,respond,State2,0}.

respond(timeout, #rcr_2_phase_commit_state{originator=Originator,commit_result=ok}=State) ->
    gen_server:reply(Originator,?singleton_reply(ok)),
    {stop,normal,State};
respond(timeout, #rcr_2_phase_commit_state{originator=Originator,commit_result={ok,_}=Reply}=State) ->
    gen_server:reply(Originator,?singleton_reply(Reply)),
    {stop,normal,State};
respond(timeout, #rcr_2_phase_commit_state{originator=Originator,rollback_result=ok}=State) ->
    gen_server:reply(Originator,?singleton_reply({error,rolled_back})),
    {stop,normal,State};
respond(timeout, #rcr_2_phase_commit_state{originator=Originator,rollback_result={ok,Res}}=State) ->
    gen_server:reply(Originator,?singleton_reply({error,{rolled_back,Res}})),
    {stop,normal,State};
respond(timeout, #rcr_2_phase_commit_state{originator=Originator,rollback_result=error}=State) ->
    gen_server:reply(Originator,?singleton_reply({error,roll_back_failure})),
    {stop,normal,State};
respond(timeout, #rcr_2_phase_commit_state{originator=Originator,rollback_result={error,Res}}=State) ->
    gen_server:reply(Originator,?singleton_reply({error,{roll_back_failure,Res}})),
    {stop,normal,State}.

%% Callbacks - not utilized
handle_event(stop, _StateName, State) -> {stop, normal, State};
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(Event, _From, _StateName, State) -> {stop, {unsupported_sync_event, Event}, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% internals
%% @doc overcomplex way of merging all results to ok | {ok, AllResults} | error | {error, AnyErrors}
simplify_results(Results) ->
    simplify_results(Results, [], []).

simplify_results([], Oks, []) ->
    case lists:all(fun(X) -> X=:=ok end, Oks) of
        true -> ok;
        false -> {ok, Oks}
    end;
simplify_results([], _, Errors) ->
    case lists:all(fun(X) -> X=:=error end, Errors) of
        true -> error;
        false -> {error, Errors}
    end;
simplify_results([ok|R], Oks, Errors) ->
    simplify_results(R, [ok|Oks], Errors);
simplify_results([{ok,Ok}|R], Oks, Errors) ->
    simplify_results(R, [Ok|Oks], Errors);
simplify_results([error|R], Oks, Errors) ->
    simplify_results(R, Oks, [error|Errors]);
simplify_results([{error,E}|R], Oks, Errors) ->
    simplify_results(R, Oks, [E|Errors]).



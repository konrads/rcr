%% @doc Stateful trace (recon) server, allows for adding/removing calls, pausing and resuming.
-module(rcr_recon).

-export([add/1, add/2, add/3]).
-export([rm/1, rm/2, rm/3]).
-export([clear/0]).
-export([pause/0]).
-export([resume/0]).
-export([list/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(rcr_recon_state,{
    calls = [],
    num_matches = 0
}).

%% Public API
add(M) when is_atom(M) -> add2({M, '_', '_'}).
add(M, F) when is_atom(M), is_atom(F) -> add2({M, F, '_'}).
add(M, F, A) when is_atom(M), is_atom(F) -> add2({M, F, A}).

rm(M) when is_atom(M) -> rm2({M, '_', '_'}).
rm(M, F) when is_atom(M), is_atom(F) -> rm2({M, F, '_'}).
rm(M, F, A) when is_atom(M), is_atom(F) -> rm2({M, F, A}).

add2(Call) -> ensure_process_cmd({add, Call}).
rm2(Call) -> ensure_process_cmd({rm, Call}).
pause() -> ensure_process_cmd(pause).
resume() -> ensure_process_cmd(resume).
list() -> ensure_process_cmd(list).

clear() ->
    recon_trace:clear(),
    case whereis(?MODULE) of
        undefined -> ignore;
        Pid -> exit(Pid, kill)
    end,
    ok.

%% gen_server callbacks
init([]) -> {ok, #rcr_recon_state{}}.

handle_call({add, {_M, _F, _A}=Call}, _From, #rcr_recon_state{calls=Calls, num_matches=NumMatches}=State) ->
    Calls2 = lists:usort([ Call | Calls ]),
    NumMatches2 = recon_call(Calls2),
    case NumMatches2 of
        NumMatches ->
            % revert
            recon_call(Calls),
            {reply, {error, {invalid_call, Call}}, State};
        _ ->
            {reply, ok, State#rcr_recon_state{calls=Calls2, num_matches=NumMatches2}}
    end;

handle_call({rm, {_M, _F, _A}=Call}, _From, #rcr_recon_state{calls=Calls}=State) ->
    MatchedCalls = find_calls(Call, Calls),
    case MatchedCalls of
        [] -> {reply, [], State};
        _ ->
            Calls2 = Calls -- MatchedCalls,
            NumMatches = recon_call(Calls2),
            {reply, MatchedCalls, State#rcr_recon_state{calls=Calls2, num_matches=NumMatches}}
    end;

handle_call(pause, _From, State) ->
    recon_trace:clear(),
    {reply, ok, State};

handle_call(resume, _From, #rcr_recon_state{calls=Calls}=State) ->
    recon_call(Calls),
    {reply, ok, State};

handle_call(list, _From, #rcr_recon_state{calls=Calls}=State) ->
    {reply, Calls, State}.

handle_info(_, State) -> {noreply, State}.
handle_cast(_, State) -> {noreply, State}.
terminate(_, _State) -> recon_trace:clear().
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% internals
ensure_process_cmd(Cmd) ->
    % ensure process exists
    case whereis(?MODULE) of
        undefined -> {ok, _} = gen_server:start({local, ?MODULE}, ?MODULE, [], []);
        _ -> ignore
    end,
    gen_server:call(?MODULE, Cmd).

recon_call([]) ->
    recon_trace:clear();
recon_call(Calls) ->
    ReconCalls = [ {M, F, [{A, [], [{return_trace}]}]} || {M, F, A} <- Calls ],
    recon_trace:calls(ReconCalls, {100, 10}, [{scope, local}]).

find_calls({M, F, A}, Calls) ->
    % as per: ets:fun2ms(fun ({M2,F2,A2}) when M2=:=M, F2=:=F, A2=:=A -> true end)
    % where M2=:=M, F2=:=F, A2=:=A are optional
    Conds =
        case M of '_' -> []; _ -> [{'=:=','$1',M}] end ++
        case F of '_' -> []; _ -> [{'=:=','$2',F}] end ++
        case A of '_' -> []; _ -> [{'=:=','$3',A}] end,
    MatchSpec = [{{'$1','$2','$3'},Conds,['$_']}],
    ets:match_spec_run(Calls, ets:match_spec_compile(MatchSpec)).

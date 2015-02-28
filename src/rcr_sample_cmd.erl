-module(rcr_sample_cmd).

-behaviour(rcr_singleton_cmd).

-export([start_link/0,
    cmd/1,
    cmd/2]).

-export([prepare/2,
    commit/2,
    rollback/2]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    rcr_singleton_cmd:start_link(?MODULE).

cmd(Cmd) ->
    rcr_singleton_cmd:cmd(?MODULE, Cmd).

cmd(Cmd, Timeout) ->
    rcr_singleton_cmd:cmd(?MODULE, Cmd, Timeout).

%%%===================================================================
%%% Internal CMD states
%%%===================================================================
prepare(Ref, pwd) ->
    Reply = random(ok, {ok,prepared}, error, {error,not_prepared}),
    lager:info("cmd pwd (~p): prepare: ~p", [Ref, Reply]),
    Reply.

commit(Ref, pwd) ->
    Reply = random(ok, file:get_cwd(), error, {error,not_commited}),
    lager:info("cmd pwd (~p): commit: ~p", [Ref, Reply]),
    Reply.

rollback(Ref, pwd) ->
    Reply = random(ok, {ok,rolled_back}, error, {error,not_rolled_back}),
    lager:info("cmd pwd (~p): rollback: ~p", [Ref, Reply]),
    Reply.

%% internals
random(Ok, OkFull, Error, ErrorFull) ->
    random:seed(now()),
    {_,_,Y} = random:seed(now()),
    Random = (Y rem 1000) / 1000.0,
    case Random of
        X when X > 0.5 -> Ok;
        X when X > 0.3 -> OkFull;
        X when X > 0.2 -> Error;
        _ -> ErrorFull
    end.

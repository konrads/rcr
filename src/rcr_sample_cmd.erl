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
    Reply = case random() of
        X when X > 0.5 -> ok;
        X when X > 0.3 -> {ok,prepared};
        X when X > 0.2 -> error;
        _ -> {error,not_prepared}
    end,
    lager:info("cmd pwd (~p): prepare: ~p", [Ref, Reply]),
    Reply.

commit(Ref, pwd) ->
    Reply = case random() of
        X when X > 0.5 -> ok;
        X when X > 0.3 -> file:get_cwd();
        X when X > 0.2 -> error;
        _ -> {error,not_commited}
    end,
    lager:info("cmd pwd (~p): commit: ~p", [Ref, Reply]),
    Reply.

rollback(Ref, pwd) ->
    Reply = case random() of
        X when X > 0.5 -> ok;
        X when X > 0.3 -> {ok,rolled_back};
        X when X > 0.2 -> error;
        _ -> {error,not_rolled_back}
    end,
    lager:info("cmd pwd (~p): rollback: ~p", [Ref, Reply]),
    Reply.

random() ->
    random:seed(now()),
    {_,_,Y} = random:seed(now()),
    (Y rem 1000) / 1000.0.

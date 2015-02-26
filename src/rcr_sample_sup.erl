-module(rcr_sample_sup).

-behaviour(supervisor).

-include("rcr_int.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Eng), {I, {I, start_link, []}, permanent, 5000, worker, [I, Eng]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [?CHILD(rcr_sample_singleton_server, rcr_singleton_server)]
         }}.

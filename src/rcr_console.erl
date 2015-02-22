-module(rcr_console).

-include("rcr_int.hrl").

-export([join/1,
         leave/1,
         remove/1,
         ringready/1]).

join([NodeStr]) ->
    wrap(
        fun() ->
            riak_core:join(NodeStr),
            io:format("Sent join request to ~s~n", [NodeStr])
        end,
        NodeStr,
        "Join failed").

leave([]) ->
    wrap(
        fun() ->
            riak_core:leave(),
            io:format("Success: ~p will shutdown after handing off its data~n", [node()])
        end,
        undefined_node,
        "Leave failed").

remove([NodeStr]) ->
    wrap(
        fun() ->
            riak_core:remove(list_to_atom(NodeStr)),
            io:format("Success: ~p removed from the cluster~n", [NodeStr])
        end,
        NodeStr,
        "Remove failed").

ringready([]) ->
    wrap(
        fun() ->
            case riak_core_status:ringready() of
                {ok, Nodes} ->
                    io:format("TRUE All nodes agree on the ring ~p\n", [Nodes]);
                {error, _}=E -> E
            end
        end,
        undefined_node,
        "Ringready failed").

%% internals
wrap(Fun, NodeStr, ExcMsg) when is_function(Fun, 0) ->
    try Fun() of
        ok -> ok;
        {error, R} ->
            ErrorMsg = case R of
                not_reachable -> ?format("Node ~s is not reachable!", [NodeStr]);
                different_ring_sizes -> ?format("Failed: ~s has a different ring_creation_size", [NodeStr]);
                unable_to_get_join_ring -> ?format("Failed: Unable to get ring from ~s", [NodeStr]);
                not_single_node -> "Failed: This node is already a member of a cluster";
                already_leaving -> ?format("~p is already in the process of leaving the cluster.", [node()]);
                not_member -> ?format("Failed: ~p is not a member of the cluster.", [node()]);
                only_member -> ?format("Failed: ~p is the only member.", [node()]);
                {different_owners, N1, N2} -> ?format("FALSE Node ~p and ~p list different partition owners\n", [N1, N2]);
                {nodes_down, Down} -> ?format("FALSE ~p down.  All nodes need to be up to check.\n", [Down]);
                _ -> ?format("Failed due to ~p~n", [R])
            end,
            io:format(ErrorMsg++"~n"),
            error(R)
    catch
        error:R ->
            lager:error(ExcMsg ++ " error:~p\nstacktrace: ~p", [R, erlang:get_stacktrace()]),
            io:format(ExcMsg ++ ", see log for details~n"),
            error(R);
        throw:R ->
            lager:error(ExcMsg ++ " throw:~p\nstacktrace: ~p", [R, erlang:get_stacktrace()]),
            io:format(ExcMsg ++ ", see log for details~n"),
            throw(R)
    end.

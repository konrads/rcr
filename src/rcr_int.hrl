-include("rcr.hrl").

-define(format(S, P), lists:flatten(io_lib:format(S, P))).
-define(format_bin(S, P), list_to_binary(lists:flatten(io_lib:format(S, P)))).
-define(stack_trace, try throw(stack_trace) catch throw:stack_trace -> erlang:get_stacktrace() end).
-define(recon(M,F), recon_trace:calls({M, F, [{'_', [], [{return_trace}]}]}, {100, 10})).

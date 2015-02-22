-module(rcr_fmt).

-include("rcr_int.hrl").

-export([
    to_str/1,
    to_short_str/1]).

to_str(#vnode_config{service_id=ServiceId, vnode=Vnode, vnode_sup=VnodeSup, vnode_master=VnodeMaster}) ->
    ?format("Vnode ~p: sup=~p, master=~p, service=~p", [Vnode, VnodeSup, VnodeMaster, ServiceId]).

to_short_str(#vnode_config{vnode=Vnode}) ->
    ?format("Vnode ~p", [Vnode]).

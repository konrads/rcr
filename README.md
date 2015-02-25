**RCR**

Currently, playground for riak_core app development.  Aiming for riak_core on rails
type of simplifications, with the onus on:

* configuration of service/vnode/vnode_sup/vnode_master
* wrapping of riak_core api
* introduction of useful concepts such as:
  * cluster-wide singleton 
  * in-process fsm
* other utils, eg. node connect/disconnect (for testing)


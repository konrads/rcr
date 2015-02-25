**RCR**

Currently, playground for riak_core app development.  Aiming for framework/simplifications
along the lines of riak_core on rails, with the onus on:

* configuration of service/vnode/vnode_sup/vnode_master
* wrapping of riak_core api
* introduction of useful concepts such as:
  * cluster-wide singleton 
  * in-process fsm
* other utils, eg. node connect/disconnect (for testing)

To run:

In shell 1, build the project and start node rcr1:

```bash
$ make full
$ # or
$ make dev
$ _cluster/n1/bin/rcr console
```

In shell 2, start node rcr2:

```bash
$ _cluster/n2/bin/rcr console
```
In shell 3, start node rcr3:

```bash
$ _cluster/n3/bin/rcr console
```

In shell 4, link up the cluster by rcr2 -> rcr1 and rcr3 -> rcr1:

```bash
$ _cluster/n2/bin/rcr_console join rcr1@127.0.0.1
$ _cluster/n3/bin/rcr_console join rcr1@127.0.0.1
```

In any of the nodes (shell1/2/3), try out few singleton operations:

```bash
(rcr1@127.0.0.1)1> rcr_sample_client:ping_singleton_call().
(rcr1@127.0.0.1)2> rcr_sample_client:ping_singleton_cast(true).
(rcr1@127.0.0.1)2> rcr_sample_client:ping_singleton_cast(false).
```

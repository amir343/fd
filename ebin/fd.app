{application, fd,
 [{description, "Phi Accrual FD"},
  {vsn, "0.0.1"},
  {modues, [fd_app, fd_sup, fd_server, node_histories, heartbeat_history]},
  {registered, [fd]},
  {applications, [kernel, stdlib]},
  {mod, {fd_app, []}}
 ]
}.
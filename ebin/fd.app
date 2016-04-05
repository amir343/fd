{application,fd,
             [{description,"Phi Accrual FD"},
              {vsn,"0.0.1"},
              {modues,[fd_api,fd_app,fd_sup,fd_server,heartbeat_history]},
              {registered,[fd]},
              {applications,[kernel,stdlib]},
              {mod,{fd_app,[]}},
              {modules,[fd,fd_app,fd_server,fd_sup,heartbeat_history]}]}.

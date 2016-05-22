%%%-------------------------------------------------------------------
%%% @author Amir Moulavi
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fd_sup).
-author("Amir Moulavi").

-behavior(supervisor).

%% API
-export([init/1]).



init([]) ->
  Server = {fd_server,                     % ID
            {fd_server, init, []},         % Server
            permanent,                     % Restart
            2000,                          % Shutdown
            worker,                        % Type
            [fd_server]                    % Modules
           },

  Children = [Server],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok, RestartStrategy, Children}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

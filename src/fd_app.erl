%%%-------------------------------------------------------------------
%%% @author Amir Moulavi
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fd_app).
-author("Amir Moulavi").

-behavior(application).

%% API
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  fd_server:start_link().

stop(State) ->
  fd_server:terminate(normal, State).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

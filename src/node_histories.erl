%%%-------------------------------------------------------------------
%%% @author Amir Moulavi
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(node_histories).
-author("Amir Moulavi").

%% API
-export([new/1, update/3, from/3, find/2, delete/2]).

-include("history_rec.hrl").

%%
%%
%%
-spec new/1 :: (node()) -> [#node_history{}].
new(Node) ->
  [#node_history{node = Node, latest_timestamp = 0, history = heartbeat_history:new()}].

%%
%%
%%
-spec find/2 :: (node(), [#node_history{}]) -> #node_history{} | false.
find(Node, Histories) ->
  lists:keyfind(Node, #node_history.node, Histories).

%%
%%
%%
-spec update/3 :: (node(), [#node_history{}], #node_history{}) -> [#node_history{}].
update(Node, OldHistories, NewNodeHistory=#node_history{}) ->
  lists:keyreplace(Node, #node_history.node, OldHistories, NewNodeHistory).

%%
%%
%%
-spec from/3 :: (node(), integer(), #history{}) -> #node_history{}.
from(Node, LatestTimestamp, History) ->
  #node_history{node = Node, latest_timestamp = LatestTimestamp, history = History}.

%%
%%
%%
-spec delete/2 :: (node(), [#node_history{}]) -> [#node_history{}].
delete(Node, Histories) ->
  lists:keydelete(Node, #node_history.node, Histories).
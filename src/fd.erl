%%%-------------------------------------------------------------------
%%% @author Amir Moulavi
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fd).
-author("Amir Moulavi").

-include("history_rec.hrl").

%% API
-export([ register_node/1,
          deregister_node/1,
          get_nodes/0,
          is_alive/1,
          get_histories/1,
          set_heartbeat_interval_ms/1,
          set_threshold/1,
          set_max_sample_size/1,
          get_heartbeat_interval_ms/0,
          get_threshold/0,
          get_max_sample_size/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Registers a new node to be monitored by this Failure Detector
%% @end
-spec register_node/1 :: (node()) -> ok;
                         ([]) -> ok;
                         ([node()]) -> ok.
register_node([]) ->
    ok;
register_node(Nodes) when is_list(Nodes) ->
    gen_server:cast({fd, node()}, {register, Nodes});
register_node(Node) when is_atom(Node) ->
    register_node([Node]).

%% @doc
%% De-registers an existing node from been monitoring by this Failure Detector
%% @end
-spec deregister_node/1 :: (node()) -> ok.
deregister_node(Node) ->
    gen_server:cast({fd, node()}, {deregister, Node}).

%% @doc
%% This method returns nodes that are under the control of this Failure Detector
%% @end
-spec get_nodes/0 :: () -> [node()].
get_nodes() ->
    gen_server:call(fd, {get_nodes}).

%% @doc
%% Checks if given Node is alive according to this Failure Detector and returns
%% the calculated Phi for the given Node.
%% @end
-spec is_alive/1 :: (node()) -> { node_available | node_unavailable, float()}.
is_alive(Node) when is_atom(Node) ->
    gen_server:call(fd, {is_alive, Node}).

%% @doc
%% Returns the histories kept for a node or 'not_found'
%% @end
-spec get_histories/1 :: (node()) -> history() | not_found.
get_histories(Node) ->
    gen_server:call({fd, node()}, {get_histories, Node}).

-spec set_heartbeat_interval_ms/1 :: (pos_integer()) -> ok.
set_heartbeat_interval_ms(Interval) when is_integer(Interval), Interval >= 0 ->
    gen_server:cast({fd, node()}, {set_heartbeat_interval, Interval}).

-spec get_heartbeat_interval_ms/0 :: () -> pos_integer().
get_heartbeat_interval_ms() ->
    gen_server:call({fd, node()}, {get_heartbeat_interval}).

-spec set_threshold/1 :: (float()) -> ok.
set_threshold(Threshold) when is_float(Threshold) ->
    gen_server:cast({fd, node()}, {set_threshold, Threshold}).

-spec get_threshold/0 :: () -> float().
get_threshold() ->
    gen_server:call({fd, node()}, {get_threshold}).

-spec set_max_sample_size/1 :: (pos_integer()) -> ok.
set_max_sample_size(Size) when is_integer(Size), Size > 0 ->
    gen_server:cast({fd, node()}, {set_max_sample_size, Size}).

-spec get_max_sample_size/0 :: () -> pos_integer().
get_max_sample_size() ->
    gen_server:call({fd, node()}, {get_max_sample_size}).

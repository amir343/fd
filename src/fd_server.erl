%%%-------------------------------------------------------------------
%%% @author Amir Moulavi
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fd_server).
-author("Amir Moulavi").

-behavior(gen_server).

%% gen_server
-export([ init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

% API
-export([ start_link/0,
          register_node/1,
          deregister_node/1,
          get_nodes/0,
          is_alive/1]).

-include("history_rec.hrl").

%% How often send heartbeats to other nodes
-define(HEARTBEAT_INTERVAL, 10000).

%% If the calculated phi for a node is < THRESHOLD then the node is alive
-define(THRESHOLD, 1.0).

%% Using exponential probability distribution ie. putting higher weights to recent heartbeats
-define(PHI_FACTOR, (1.0/ math:log(10.0))).

%% Keeps internal state of this failure detector
-record(state,
        {nodes = [] :: [term()],
        node_histories = [] :: [#node_history{}]}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Registers a new node to be monitored by this Failure Detector
%% @end
-spec register_node/1 :: (node()) -> ok.
register_node(Node) ->
    gen_server:cast({fd, node()}, {register, Node}),
    ok.

%% @doc
%% De-registers an existing node from been monitoring by this Failure Detector
%% @end
-spec deregister_node/1 :: (node()) -> ok.
deregister_node(Node) ->
    gen_server:cast({fd, node()}, {deregister, Node}),
    ok.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      GEN_SERVER METHODS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, fd}, ?MODULE, [], []).

init([]) ->
    timer:apply_after(1000, gen_server, cast, [{fd, node()}, send_heartbeat]),
    {ok, #state{}}.

handle_call({is_alive, Node}, _From, State=#state{node_histories = Hs}) ->
    {reply, is_available(Node, Hs), State};
handle_call({get_histories, Node}, _From, State=#state{node_histories = Hs}) ->
    Result = case node_histories:find(Node, Hs) of
        #node_history{node = _, latest_timestamp = _, history = H}       -> H;
        false                                                            -> not_found
    end,
    {reply, Result, State};
handle_call({get_nodes}, _From, State=#state{nodes = Ns}) ->
    {reply, Ns, State}.

handle_cast(send_heartbeat, State=#state{nodes = Ns}) ->
    send_heartbeat_to_nodes(Ns),
    {noreply, State};
handle_cast({heartbeat, Node}, State) ->
    gen_server:cast({fd, Node}, {alive, node()}),
    {noreply, State};
handle_cast({alive, Node}, #state{nodes = Ns, node_histories = Hs}) ->
    NHistories = received_alive(Node, Hs),
    {noreply, #state{nodes = Ns, node_histories = NHistories}};
handle_cast({register, Node}, #state{nodes = Nodes, node_histories = Hs}) ->
    io:format("Node ~p is registered~n", [Node]),
    {noreply, #state{nodes = [Node | Nodes], node_histories = Hs}};
handle_cast({deregister, Node}, #state{nodes = Ns, node_histories = H}) ->
    {NewNs, NHistory} = deregister_node(Node, Ns, H),
    io:format("Node ~p is deregistered~n", [Node]),
    {noreply, #state{nodes = NewNs, node_histories = NHistory}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      INTERNAL METHODS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
-spec send_heartbeat_to_nodes/1 :: ([node()]) -> any().
send_heartbeat_to_nodes(Nodes) ->
    [gen_server:cast({fd, Node}, {heartbeat, node()}) || Node <- Nodes],
    timer:apply_after(?HEARTBEAT_INTERVAL, gen_server, cast, [{fd, node()}, send_heartbeat]).

%%
%% @private
-spec deregister_node/3 :: (node(), [node()], [#node_history{}]) -> {[node()], [#node_history{}]}.
deregister_node(Node, Nodes, Histories) ->
    {lists:delete(Node, Nodes), node_histories:delete(Node, Histories)}.

%%
%% @private
-spec received_alive/2 :: (node(), [#node_history{}]) -> [#node_history{}].
received_alive(Node, Histories) ->
    io:format("Received heartbeat from ~p~n", [Node]),

    Now = timestamp_millis(),
    case node_histories:find(Node, Histories) of
        #node_history{node = Node, latest_timestamp = LatestTs, history = History} ->
            case LatestTs =:= 0 of
                true ->
                    NHistory = heartbeat_history:append(History, ?HEARTBEAT_INTERVAL div 2),
                    node_histories:update(Node, Histories, node_histories:from(Node, Now, NHistory));
                false ->
                    Interval = Now - LatestTs,
                    NHistory = heartbeat_history:append(History, Interval),
                    node_histories:update(Node, Histories, node_histories:from(Node, Now, NHistory))
            end;
        false ->
            node_histories:new(Node)
    end.

%%
%% @private
-spec is_available/2 :: (node(), [#node_history{}]) -> { node_available | node_unavailable, float()}.
is_available(Node, Histories) ->
    Phi = phi(Node, Histories),
    case Phi < ?THRESHOLD of
        true  -> {node_available, Phi};
        false -> {node_unavailable, Phi}
    end.

%%
%% @private
-spec phi/2 :: (node(), [#node_history{}]) -> float() | infinity.
phi(Node, Histories) ->
    case node_histories:find(Node, Histories) of
        #node_history{node = Node, latest_timestamp = LatestTs,
                      history = #history{intervals = L, interval_sum = _} = H} ->
            Now = timestamp_millis(),
            TimeDiff = case Now - LatestTs =:= 0 of
                           true  -> ?HEARTBEAT_INTERVAL div 2;
                           false -> Now - LatestTs
                       end,
            case length(L) > 0 of
                true  -> ?PHI_FACTOR * TimeDiff / heartbeat_history:mean(H);
                false -> 0.0
            end;
        _ ->
            infinity
    end.

%%
%% @private
-spec timestamp_millis/0 :: () -> pos_integer().
timestamp_millis() ->
    {Mega, Sec, Micro} = erlang:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro/1000).


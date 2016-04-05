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
          is_alive/1,
          get_histories/1,
          set_heartbeat_interval_ms/1,
          set_threshold/1,
          set_max_sample_size/1,
          get_heartbeat_interval_ms/0,
          get_threshold/0,
          get_max_sample_size/0]).

-include("history_rec.hrl").

%% How often send heartbeats to other nodes
-define(HEARTBEAT_INTERVAL, 10000).

%% If the calculated phi for a node is < THRESHOLD then the node is alive
-define(THRESHOLD, 1.0).

%% Using exponential probability distribution ie. putting higher weights to recent heartbeats
-define(PHI_FACTOR, (1.0/ math:log(10.0))).

%% Maximum number of interval history to keep
-define(MAX_SAMPLE_SIZE, 100).

-record(config,
        {heartbeat_interval = ?HEARTBEAT_INTERVAL :: pos_integer(),
        threshold           = ?THRESHOLD          :: float(),
        max_sample_size     = ?MAX_SAMPLE_SIZE    :: pos_integer()}).

-type config() :: #config{}.

%% Keeps internal state of this failure detector
-record(state,
        {nodes         = []         :: [term()],
        node_histories = dict:new() :: dict(),
        config         = #config{}  :: config()}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      GEN_SERVER METHODS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, fd}, ?MODULE, [], []).

init([]) ->
    timer:apply_after(1000, gen_server, cast, [{fd, node()}, send_heartbeat]),
    {ok, #state{}}.

handle_call({is_alive, Node}, _From, State=#state{node_histories = Hs, config = C}) ->
    {reply, is_available(Node, Hs, C), State};

handle_call({get_histories, Node}, _From, State=#state{node_histories = Hs}) ->
    Result = case dict:find(Node, Hs) of
               {ok, #node_history{node = _, latest_timestamp = _, history = H}}  -> H;
               _                                                                 -> not_found
    end,
    {reply, Result, State};

handle_call({get_nodes}, _From, State=#state{nodes = Ns}) ->
    {reply, Ns, State};

handle_call({get_heartbeat_interval}, _From, State=#state{config = C}) ->
    {reply, C#config.heartbeat_interval, State};

handle_call({get_threshold}, _From, State=#state{config = C}) ->
    {reply, C#config.threshold, State};

handle_call({get_max_sample_size}, _From, State=#state{config = C}) ->
    {reply, C#config.max_sample_size, State}.



handle_cast(send_heartbeat, State=#state{nodes = Ns, config = C}) ->
    send_heartbeat_to_nodes(Ns, C#config.heartbeat_interval),
    {noreply, State};

handle_cast({heartbeat, Node}, State) ->
    gen_server:cast({fd, Node}, {alive, node()}),
    {noreply, State};

handle_cast({alive, Node}, State=#state{nodes = Ns, node_histories = Hs, config = C}) ->
    NHistories = received_alive(Node, Hs, C),
    {noreply, State#state{nodes = Ns, node_histories = NHistories}};

handle_cast({register, Nodes}, State=#state{nodes = Ns, node_histories = Hs}) ->
    Diff = Nodes -- Ns,
    NNs = Diff ++ Ns,
    {noreply, State#state{nodes = NNs, node_histories = Hs}};

handle_cast({deregister, Node}, State=#state{nodes = Ns, node_histories = H}) ->
    {NewNs, NHistory} = deregister_node(Node, Ns, H),
    io:format("Node ~p is deregistered~n", [Node]),
    {noreply, State#state{nodes = NewNs, node_histories = NHistory}};

handle_cast({set_heartbeat_interval, Interval}, State=#state{config = C}) ->
    {noreply, State#state{config = C#config{heartbeat_interval = Interval}}};

handle_cast({set_threshold, Threshold}, State=#state{config = C}) ->
    {noreply, State#state{config = C#config{threshold = Threshold}}};

handle_cast({set_max_sample_size, Size}, State=#state{config = C}) ->
    {noreply, State#state{config = C#config{max_sample_size = Size}}}.



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
-spec send_heartbeat_to_nodes/2 :: ([node()], pos_integer()) -> any().
send_heartbeat_to_nodes(Nodes, HeartbeatInt) ->
    catch([gen_server:cast({fd, Node}, {heartbeat, node()}) || Node <- Nodes]),
    timer:apply_after(HeartbeatInt, gen_server, cast, [{fd, node()}, send_heartbeat]).

%%
%% @private
-spec deregister_node/3 :: (node(), [node()], dict()) -> {[node()], dict()}.
deregister_node(Node, Nodes, Histories) ->
    {lists:delete(Node, Nodes), dict:erase(Node, Histories)}.

%%
%% @private
-spec received_alive/3 :: (node(), dict(), config()) -> dict().
received_alive(Node, Histories, C) ->
    Now = timestamp_millis(),

    case dict:find(Node, Histories) of
        {ok, #node_history{node = Node, latest_timestamp = LatestTs, history = History}} ->
            case LatestTs =:= 0 of
                true ->
                    NHistory = heartbeat_history:append(History, C#config.max_sample_size div 2, C#config.max_sample_size),
                    dict:store(Node, to_node_history(Node, Now, NHistory), Histories);
                false ->
                    Interval = Now - LatestTs,
                    NHistory = heartbeat_history:append(History, Interval, C#config.max_sample_size),
                    dict:store(Node, to_node_history(Node, Now, NHistory), Histories)
            end;
        error ->
            dict:store(Node, #node_history{node = Node, latest_timestamp = 0, history = heartbeat_history:new()}, Histories)
    end.

%%
%% @private
-spec is_available/3 :: (node(), dict(), config()) -> { node_available | node_unavailable, float()}.
is_available(Node, Histories, Config=#config{}) ->
    Phi = phi(Node, Histories, Config#config.heartbeat_interval),
    case Phi < Config#config.threshold of
        true  -> {node_available, Phi};
        false -> {node_unavailable, Phi}
    end.

%%
%% @private
-spec phi/3 :: (node(), dict(), pos_integer()) -> float() | infinity.
phi(Node, Histories, HeartbeatInt) ->
    case dict:find(Node, Histories) of
        {ok, #node_history{node = Node, latest_timestamp = LatestTs,
                      history = #history{intervals = L, interval_sum = _} = H}} ->
            Now = timestamp_millis(),
            TimeDiff = case Now - LatestTs =:= 0 of
                           true  -> HeartbeatInt div 2;
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

%%
%% @private
-spec to_node_history/3 :: (node(), integer(), history()) -> node_history().
to_node_history(Node, LatestTimestamp, History) ->
  #node_history{node = Node, latest_timestamp = LatestTimestamp, history = History}.

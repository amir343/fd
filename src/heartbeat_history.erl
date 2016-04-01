%%%-------------------------------------------------------------------
%%% @author Amir Moulavi
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(heartbeat_history).
-author("Amir Moulavi").

%% API
-export([new/0, append/2, mean/1]).

%% Maximum number of interval history to keep
-define(MAX_SAMPLE_SIZE, 100).

-include("history_rec.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%
%%
-spec new/0 :: () -> #history{}.
new() ->
    #history{}.

%%
%%
%%
-spec append/2 :: (#history{}, integer()) -> #history{}.
append(History=#history{}, Interval) ->
    case length(History#history.intervals) < ?MAX_SAMPLE_SIZE of
        true ->
            #history {
               intervals = [Interval | History#history.intervals],
               interval_sum = History#history.interval_sum + Interval};
        _ ->
            append(drop_oldest(History), Interval)
    end.

%%
%%
%%
-spec mean/1 :: (#history{}) -> float().
mean(History=#history{}) ->
    case length(History#history.intervals) of
        0 -> 0.0;
        N -> History#history.interval_sum / N
    end.

%%
%%
%% @private
-spec drop_oldest/1 :: (#history{}) -> #history{}.
drop_oldest(History=#history{}) ->
    OldestValue = lists:last(History#history.intervals),
    #history {
       intervals = lists:droplast(History#history.intervals),
       interval_sum = History#history.interval_sum - OldestValue}.


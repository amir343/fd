
-ifndef(_HISTORY_REC_).
-define(_HISTORY_REC_, true).

-record(history,
        {intervals = [] :: list(),
         interval_sum = 0 :: pos_integer()}).
-endif.

-ifndef(_NODE_HISTORY_REC_).
-define(_NODE_HISTORY_REC_, true).

-record(node_history,
        {node :: term(),
         latest_timestamp = 0 :: pos_integer(),
         history = #history{}}).
-endif.

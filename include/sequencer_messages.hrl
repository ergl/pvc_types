-define(SEQUENCER_VERSION, 1).
-define(SEQUENCER_VERSION_BITS, 8).
-define(SEQUENCER_MSG(Payload),
   <<?SEQUENCER_VERSION:?SEQUENCER_VERSION_BITS, Payload/binary>>
).

-record(ping, {
    partitions :: [non_neg_integer()]
}).

-record(redblue_prepare_request, {
    tx_id :: term(),
    tx_label :: term(),
    readset :: #{non_neg_integer() := [term()]},
    writeset :: #{non_neg_integer() := #{}},
    snapshot_vc :: #{}
}).

-record(redblue_prepare_response, {
    tx_id :: term(),
    %% abort reason, or commit vector
    outcome :: atom() | {ok, #{}}
}).

-record(redblue_deliver, {
    timestamp :: non_neg_integer(),
    %% writeset and commit vector
    %% the writeset contains the partitions where this should be routed
    transactions :: [{term(), #{non_neg_integer() := #{}}, #{}}]
}).

-record(reblue_put_conflicts, {
    conflicts :: term()
}).

-type sequencer_message() :: #ping{}
                           | #redblue_prepare_request{}
                           | #redblue_prepare_response{}
                           | #redblue_deliver{}
                           | #reblue_put_conflicts{}.

-export_type([
    sequencer_message/0
]).

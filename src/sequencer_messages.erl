-module(sequencer_messages).
-include("sequencer_messages.hrl").

-define(header(Packet, Size), (Size):(Packet)/unit:8-integer-big-unsigned).
-define(ping, 0).
-define(pepare_request, 1).
-define(prepare_response, 2).
-define(deliver, 3).
-define(conflicts, 4).

-export([frame/2]).

-export([ping/1,
         prepare_request/5,
         prepare_response/2,
         deliver/2,
         put_conflicts/1]).

-export([decode/1]).

frame(HeaderSize, Data) ->
    Size = erlang:iolist_size(Data),
    [<<?header(HeaderSize, Size)>>, Data].

-spec ping([non_neg_integer()]) -> binary().
ping(Partitons) ->
    Payload = term_to_binary(Partitons),
    <<?SEQUENCER_VERSION:?SEQUENCER_VERSION_BITS, ?ping:8, Payload/binary>>.

-spec prepare_request(term(), term(), #{non_neg_integer() := [term()]}, #{non_neg_integer() := #{}}, #{}) -> binary().
prepare_request(TxId, Label, RS, WS, VC) ->
    Payload = term_to_binary({TxId, Label, RS, WS, VC}),
    <<?SEQUENCER_VERSION:?SEQUENCER_VERSION_BITS, ?pepare_request:8, Payload/binary>>.

-spec prepare_response(term(), {abort, atom()} | {ok, #{}}) -> binary().
prepare_response(TxId, Outcome) ->
    Payload = term_to_binary({TxId, Outcome}),
    <<?SEQUENCER_VERSION:?SEQUENCER_VERSION_BITS, ?prepare_response:8, Payload/binary>>.

-spec deliver(non_neg_integer(), [{term(), #{non_neg_integer() := #{}}, #{}}]) -> binary().
deliver(Ts, Transactions) ->
    Payload = <<Ts:8/unit:8-integer-big-unsigned, (term_to_binary(Transactions))/binary>>,
    <<?SEQUENCER_VERSION:?SEQUENCER_VERSION_BITS, ?deliver:8, Payload/binary>>.

-spec put_conflicts(term()) -> binary().
put_conflicts(Conflicts) ->
    <<?SEQUENCER_VERSION:?SEQUENCER_VERSION_BITS, ?conflicts:8, (term_to_binary(Conflicts))/binary>>.

decode(<<?ping:8, Payload/binary>>) ->
    #ping{partitions=binary_to_term(Payload)};

decode(<<?pepare_request:8, Payload/binary>>) ->
    {TxId, Label, RS, WS, VC} = binary_to_term(Payload),
    #redblue_prepare_request{
        tx_id=TxId,
        tx_label=Label,
        readset=RS,
        writeset=WS,
        snapshot_vc=VC
    };

decode(<<?prepare_response:8, Payload/binary>>) ->
    {TxId, Outcome} = binary_to_term(Payload),
    #redblue_prepare_response{
        tx_id=TxId,
        outcome=Outcome
    };

decode(<<?deliver:8, Ts:8/unit:8-integer-big-unsigned, Payload/binary>>) ->
    #redblue_deliver{
        timestamp=Ts,
        transactions=binary_to_term(Payload)
    };

decode(<<?conflicts:8, Payload/binary>>) ->
    #reblue_put_conflicts{
        conflicts=binary_to_term(Payload)
    }.

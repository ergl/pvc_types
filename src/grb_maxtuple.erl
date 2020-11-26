-module(grb_maxtuple).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: {non_neg_integer(), term()}.
-opaque op() :: {assign, {non_neg_integer(), term()}}.
-export_type([t/0, op/0]).

%% A maxtuple is like a map with a number as key, and whoever assigns it the biggest
%% number, wins.

%% API
-export([new/0,
         value/1,
         merge_ops/2,
         make_op/1,
         apply_op_raw/2,
         apply_op/4]).

-spec new() -> t().
new() ->
    {0, <<>>}.

-spec value(t()) -> {non_neg_integer(), term()}.
value(T) ->
    T.

-spec merge_ops(op(), op()) -> op().
merge_ops({assign, {N, L}}, {assign, {M, _}}) when N > M ->
    {assign, {N, L}};
merge_ops({assign, {_, _}}, {assign, {M, R}}) ->
    {assign, {M, R}}.

-spec make_op({non_neg_integer(), term()}) -> op().
make_op({S, X}) ->
    {assign, {S, X}}.

-spec apply_op_raw(op(), t()) -> t().
apply_op_raw({assign, {S, X}}, {S0, _}=R) ->
    if
        S >= S0 ->
            {S, X};
        true ->
            R
    end.

-spec apply_op(op(), [term()], grb_vclock:vc(), t()) -> t().
apply_op(Op, _, _, T) ->
    apply_op_raw(Op, T).

-ifdef(TEST).
grb_maxtuple_test() ->
    OpList = [make_op({10, a}), make_op({30, b}), make_op({0, c})],
    Final = lists:foldl(fun(Op, R) ->
        apply_op(Op, ignore, ignore, R)
    end, new(), shuffle(OpList)),
    CompressedOpList = lists:foldl(fun(Op, AccOp) ->
        merge_ops(Op, AccOp)
    end, hd(OpList), tl(OpList)),
    ?assertMatch({30, b}, Final),
    ?assertMatch({30, b}, apply_op(CompressedOpList, ignore, ignore, new())).

shuffle([]) -> [];
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
			randomize(Acc)
		end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
			  {rand:uniform_real(), A}
		  end, List),

    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

-endif.

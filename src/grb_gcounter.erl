-module(grb_gcounter).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: non_neg_integer().
-opaque op() :: {increment, non_neg_integer()}.
-export_type([t/0, op/0]).

%% API
-export([new/0,
         value/1,
         merge_ops/2,
         make_op/1,
         apply_op_raw/2,
         apply_op/4]).

-spec new() -> t().
new() ->
    0.

-spec value(t()) -> non_neg_integer().
value(M) ->
    M.

-spec merge_ops(op(), op()) -> op().
merge_ops({increment, X}, {increment, Y}) ->
    {increment, X + Y}.

-spec make_op(non_neg_integer()) -> op().
make_op(X) ->
    {increment, X}.

-spec apply_op_raw(op(), t()) -> t().
apply_op_raw({increment, X}, M) ->
    M + X.

-spec apply_op(op(), _, _, t()) -> t().
apply_op(Op, _, _, M) ->
    %% we don't care about time for sets
    apply_op_raw(Op, M).

-ifdef(TEST).
grb_gcounter_test() ->
    OpList = [make_op(10), make_op(30), make_op(0)],
    Final = lists:foldl(fun(Op, R) ->
        apply_op(Op, ignore, ignore, R)
    end, new(), shuffle(OpList)),
    CompressedOpList = lists:foldl(fun(Op, AccOp) ->
        merge_ops(Op, AccOp)
    end, hd(OpList), tl(OpList)),
    ?assertMatch(40, Final),
    ?assertMatch(40, apply_op(CompressedOpList, ignore, ignore, new())).

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

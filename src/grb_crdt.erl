-module(grb_crdt).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: {non_neg_integer(), grb_lww:t() | grb_gset:t()}.
-opaque op() :: {non_neg_integer(), grb_lww:op() | grb_gset:op()}.
-type crdt() :: grb_lww | grb_gset.

-export_type([t/0, op/0, crdt/0]).

-export([new/1,
         value/1,
         make_op/2,
         merge_ops/2,
         apply_op/4,
         apply_op_raw/2]).

-spec new(crdt()) -> t().
new(Mod) ->
    {type(Mod), Mod:new()}.

-spec value(t()) -> term().
value({Type, Val}) ->
    (module(Type)):value(Val).

-spec make_op(crdt(), term()) -> op().
make_op(Mod, X) ->
    {type(Mod), Mod:make_op(X)}.

-spec merge_ops(op(), op()) -> op().
merge_ops({Type, OpL}, {Type, OpR}) ->
    {Type, (module(Type)):merge_ops(OpL, OpR)}.

-spec apply_op(op(), [term()], grb_vclock:vc(), t()) -> t().
apply_op({Type, Op}, Actors, CT, {Type, Base}) ->
    {Type, (module(Type)):apply_op(Op, Actors, CT, Base)}.

-spec apply_op_raw(op(), t()) -> t().
apply_op_raw({Type, Op}, {Type, Base}) ->
    {Type, (module(Type)):apply_op_raw(Op, Base)}.

-spec type(crdt()) -> non_neg_integer().
type(grb_lww) -> 0;
type(grb_gset) -> 1.

-spec module(non_neg_integer()) -> crdt().
module(0) -> grb_lww;
module(1) -> grb_gset.

-ifdef(TEST).
grb_crdt_lww_test() ->
    Actors = [a,b],
    VC = fun(L) -> grb_vclock:from_list(L) end,
    Fresh = grb_crdt:new(grb_lww),
    OpList = [
        {grb_crdt:make_op(grb_lww, X), T}
    || {X, T} <- [ {10, VC([{a, 0}, {b, 1}])},
                    {30, VC([{a, 1}, {b, 1}])},
                    {0, VC([{a, 2}, {b, 3}])} ]],

    {LastOp, LastCT} = lists:last(OpList),
    Max = grb_crdt:value(grb_crdt:apply_op(LastOp, Actors, LastCT, Fresh)),

    Final = lists:foldl(fun({Op, CT}, R) ->
        grb_crdt:apply_op(Op, Actors, CT, R)
    end, Fresh, shuffle(OpList)),

    [{FirstOp, _} | RestOpsAndCTs] = OpList,
    CompressedOpList = lists:foldl(fun({Op, _}, AccOp) ->
        grb_crdt:merge_ops(AccOp, Op)
    end, FirstOp, RestOpsAndCTs),

    ?assertEqual(Max, grb_crdt:value(Final)),
    ?assertEqual(Max, grb_crdt:value(grb_crdt:apply_op(CompressedOpList, Actors, LastCT, Fresh))).

grb_crdt_gset_test() ->
    Fresh = grb_crdt:new(grb_gset),
    OpList = [ grb_crdt:make_op(grb_gset, X) || X <- [10, 30, 0]],
    Final = lists:foldl(fun(Op, R) ->
        grb_crdt:apply_op(Op, ignore, ignore, R)
    end, Fresh, shuffle(OpList)),
    CompressedOpList = lists:foldl(fun(Op, AccOp) ->
        grb_crdt:merge_ops(Op, AccOp)
    end, hd(OpList), tl(OpList)),

    ?assertMatch(#{10 := _, 30 := _, 0 := _},
                 grb_crdt:value(Final)),

    ?assertMatch(#{10 := _, 30 := _, 0 := _},
                 grb_crdt:value(apply_op(CompressedOpList, ignore, ignore, Fresh))).

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


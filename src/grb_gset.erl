-module(grb_gset).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: #{term() := nil}.
-opaque op() :: {add, term()} | {add_all, t()} | count_members | {limit, non_neg_integer()} | {member, term()}.
-export_type([t/0, op/0]).

%% API
-export([new/0,
         value/1,
         merge_ops/2,
         make_op/1,
         apply_op_raw/2,
         apply_op/4,
         apply_read_op/2]).

%% G-set specific API
-export([count_op/0,
         limit_op/1,
         member_op/1]).

-spec new() -> t().
new() ->
    #{}.

-spec value(t()) -> #{term() := nil}.
value(M) ->
    M.

-spec merge_ops(op(), op()) -> op().
merge_ops({add, X}, {add, Y}) ->
    {add_all, #{X => nil, Y => nil}};

merge_ops(L, {add_all, M}) ->
    {add_all, apply_op(L, ignore, ignore, M)};

merge_ops({add_all, M}, R) ->
    {add_all, apply_op(R, ignore, ignore, M)}.

-spec make_op(term()) -> op().
make_op(X) ->
    {add, X}.

-spec count_op() -> op().
count_op() -> count_members.

-spec limit_op(non_neg_integer()) -> op().
limit_op(X) when X >= 0 -> {limit, X}.

-spec member_op(term()) -> op.
member_op(X) -> {member, X}.

-spec apply_op_raw(op(), t()) -> t().
apply_op_raw({add, X}, M) ->
    M#{X => nil};

apply_op_raw({add_all, M0}, M) ->
    maps:merge(M0, M).

-spec apply_op(op(), _, _, t()) -> t().
apply_op(Op, _, _, M) ->
    %% we don't care about time for sets
    apply_op_raw(Op, M).

-spec apply_read_op(op(), t()) -> term().
apply_read_op(count_members, M) ->
    map_size(M);
apply_read_op({member, X}, M) ->
    maps:is_key(X, M);
apply_read_op({limit, N}, M) when N >= 0 ->
    limit_(N, maps:next(maps:iterator(M)), []).

-spec limit_(non_neg_integer(), maps:iterator(), [term()]) -> [term()].
limit_(_, none, Acc) -> Acc;
limit_(0, _, Acc) -> Acc;
limit_(N, {K, _, It}, Acc) ->
    limit_(N - 1, maps:next(It), [K | Acc]).

-ifdef(TEST).
grb_gset_test() ->
    OpList = [make_op(10), make_op(30), make_op(0)],
    Final = lists:foldl(fun(Op, R) ->
        apply_op(Op, ignore, ignore, R)
    end, new(), shuffle(OpList)),
    CompressedOpList = lists:foldl(fun(Op, AccOp) ->
        merge_ops(Op, AccOp)
    end, hd(OpList), tl(OpList)),
    ?assertMatch(#{10 := _, 30 := _, 0 := _}, Final),
    ?assertMatch(#{10 := _, 30 := _, 0 := _}, apply_op(CompressedOpList, ignore, ignore, new())),

    ?assertNot(apply_read_op(member_op(20), Final)),
    ?assertEqual(3, apply_read_op(count_op(), Final)),

    ?assertNot(apply_read_op(member_op(20), apply_op(CompressedOpList, ignore, ignore, new()))),
    ?assertEqual(3, apply_read_op(count_op(), apply_op(CompressedOpList, ignore, ignore, new()))),

    [
        ?assert(apply_read_op(member_op(X), Final))
        || X <- [10, 30, 0]
    ],

    [
        ?assert(apply_read_op(member_op(X), apply_op(CompressedOpList, ignore, ignore, new())))
        || X <- [10, 30, 0]
    ],

    [
        ?assertEqual(X, length(apply_read_op(limit_op(X), Final)))
        || X <- lists:seq(0, 3)
    ],

    [
        ?assertEqual(X, length(apply_read_op(limit_op(X), apply_op(CompressedOpList, ignore, ignore, new()))))
        || X <- lists:seq(0, 3)
    ].

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

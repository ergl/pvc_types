-module(grb_lww).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: {grb_vclock:vc(), term()}.
-opaque op() :: {assign, term()}.
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
    {grb_vclock:new(), <<>>}.

-spec value(t()) -> term().
value({_, Val}) ->
    Val.

-spec merge_ops(op(), op()) -> op().
merge_ops(_L, R) ->
    R.

-spec make_op(term()) -> op().
make_op(X) ->
    {assign, X}.

-spec apply_op_raw(op(), t()) -> t().
apply_op_raw({assign, X}, {Time, _}) ->
    {Time, X}.

-spec apply_op(op(), [term()], grb_vclock:vc(), t()) -> t().
apply_op({assign, X}, Actors, CT, {OldCT, _}=R) ->
    case grb_vclock:lt_at_keys(Actors, OldCT, CT) of
        true ->
            {grb_vclock:max_at_keys(Actors, CT, OldCT), X};
        false ->
            R
    end.

-ifdef(TEST).
grb_lww_test() ->
    Actors = [a,b],
    VC = fun(L) -> grb_vclock:from_list(L) end,

    Max = apply_op(
        make_op(0),
        Actors,
        VC([{a, 2}, {b, 3}]),
        new()
    ),
    OpList = [
        {make_op(10), VC([{a, 0}, {b, 1}])},
        {make_op(30), VC([{a, 1}, {b, 1}])},
        {make_op(0), VC([{a, 2}, {b, 3}])}
    ],
    Final = lists:foldl(fun({Op, CT}, R) ->
        apply_op(Op, Actors, CT, R)
    end, new(), shuffle(OpList)),
    CompressedOpList = lists:foldl(fun({Op, _}, AccOp) ->
        merge_ops(AccOp, Op)
    end, element(2,hd(OpList)), tl(OpList)),
    ?assertEqual(Max, Final),
    ?assertEqual(Max, apply_op(
        CompressedOpList,
        Actors,
        VC([{a, 2}, {b, 3}]),
        new())
    ).

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

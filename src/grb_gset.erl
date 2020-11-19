-module(grb_gset).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: #{term() := nil}.
-opaque op() :: {add, term()}.
-export_type([t/0, op/0]).

%% API
-export([new/0,
         value/1,
         merge/2,
         make_op/1,
         apply_op/3]).

-spec new() -> t().
new() ->
    #{}.

-spec value(t()) -> #{term() := nil}.
value(M) ->
    M.

-spec merge(t(), t()) -> t().
merge(L, R) ->
    maps:merge(L, R).

-spec make_op(term()) -> op().
make_op(X) ->
    {add, X}.

-spec apply_op(op(), non_neg_integer(), t()) -> t().
apply_op({add, X}, _, M) ->
    M#{X => nil}.

-ifdef(TEST).
grb_gset_test() ->
    OpList = [{1, make_op(10)}, {2, make_op(30)}, {5, make_op(0)}],
    Final = lists:foldl(fun({Time, Op}, R) ->
        apply_op(Op, Time, R)
    end, new(), shuffle(OpList)),
    ?assertMatch(#{10 := _, 30 := _, 0 := _}, Final).

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

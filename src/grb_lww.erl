-module(grb_lww).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: {non_neg_integer(), term()}.
-opaque op() :: {assign, term()}.
-export_type([t/0, op/0]).

%% API
-export([new/0,
         value/1,
         merge/2,
         make_op/1,
         apply_op/3]).

-spec new() -> t().
new() ->
    {0, <<>>}.

-spec value(t()) -> term().
value({_, Val}) ->
    Val.

-spec merge(t(), t()) -> t().
merge(L, R) ->
    erlang:max(L, R).

-spec make_op(term()) -> op().
make_op(X) ->
    {assign, X}.

-spec apply_op(op(), non_neg_integer(), t()) -> t().
apply_op(_, Time, {OldTime, _}=R)
    when Time < OldTime ->
        R;
apply_op({assign, X}, Time, {OldTime, _}) ->
    {erlang:max(Time, OldTime + 1), X}.

-ifdef(TEST).
grb_lww_test() ->
    Max = apply_op(make_op(0), 5, new()),
    OpList = [{1, make_op(10)}, {2, make_op(30)}, {5, make_op(0)}],
    Final = lists:foldl(fun({Time, Op}, R) ->
        apply_op(Op, Time, R)
    end, new(), shuffle(OpList)),
    ?assertEqual(Max, Final).

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

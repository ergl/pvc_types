
-module(pvc_writeset).

-type ws(K, V) :: orddict:orddict(K, V).

-export_type([ws/2]).

%% API
-export([new/0,
         to_list/1,
         intersect/2,
         get/2,
         get/3,
         put/3]).

-spec new() -> ws(_, _).
new() ->
    orddict:new().

-spec to_list(ws(K, V)) -> [{K, V}].
to_list(WS) ->
    orddict:to_list(WS).

-spec intersect(ws(K, _), ws(K, _)) -> boolean().
intersect(_, []) ->
    false;

intersect([], _) ->
    false;

intersect([{K, _} | Rest], Other) ->
    case orddict:find(K, Other) of
        {ok, _} ->
            true;
        error ->
            intersect(Rest, Other)
    end.

-spec get(K, ws(K, V)) -> {ok, V} | error.
get(K, WS) ->
    orddict:find(K, WS).

-spec get(K, ws(K, V), V) -> V.
get(K, WS, Default) ->
    case orddict:find(K, WS) of
        {ok, Value} -> Value;
        error -> Default
    end.

-spec put(K, V, ws(K, V)) -> ws(K, V).
put(K, V, WS) ->
    orddict:store(K, V, WS).

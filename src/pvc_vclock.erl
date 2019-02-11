%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(pvc_vclock).

%% API
-export([new/0,
         get_time/2,
         set_time/3,
         eq/2,
         max/2]).

%% Debug API
-export([from_list/1]).

-type vc() :: vc(term()).
-opaque vc(T) :: orddict:orddict(T, non_neg_integer()).

-export_type([vc/1]).

-spec new() -> vc().
new() -> orddict:new().

-spec get_time(T, vc(T)) -> non_neg_integer().
get_time(Key, VectorClock) ->
    case orddict:find(Key, VectorClock) of
        {ok, Value} -> Value;
        error -> 0
    end.

-spec set_time(T, non_neg_integer(), vc(T)) -> vc(T).
set_time(Key, Value, VectorClock) ->
    orddict:store(Key, Value, VectorClock).

-spec eq(vc(T), vc(T)) -> boolean().
eq(VC, VC) -> true;

eq(Left, Right) ->
    orddict:fold(fun(Key, Value, Acc) ->
        case Acc of
            false ->
                false;
            true ->
                {ok, Value} =:= orddict:find(Key, Right)
        end
    end, true, Left).

-spec max(vc(T), vc(T)) -> vc(T).
max(Left, Right) ->
    orddict:merge(fun(_Key, V1, V2) -> erlang:max(V1, V2) end, Left, Right).

from_list(List) ->
    orddict:from_list(List).

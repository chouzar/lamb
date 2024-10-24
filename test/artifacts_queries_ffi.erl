-module(artifacts_queries_ffi).

-export([query_user/0, query_client/0, query_admin/0, query_where_name_is/1]).

-include_lib("stdlib/include/ms_transform.hrl").

query_user() ->
    ets:fun2ms(fun({_Id, {user, A, B, C, D}}) -> {user, A, B, C, D} end).

query_client() ->
    ets:fun2ms(fun({_Id, {client, A, B, C}}) -> {client, A, B, C} end).

query_admin() ->
    ets:fun2ms(fun({_Id, {admin, A}}) -> {admin, A} end).

query_where_name_is(Name) ->
    ets:fun2ms(fun({_, {user, A, B, C, D}}) when B == Name -> {user, A, B, C, D} end).

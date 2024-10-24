-module(lamb_erlang_queries_ffi).

-export([query_count/0, query_record/0]).

% https://www.erlang.org/doc/apps/stdlib/ets.html#fun2ms/1
%
% The parse transform is provided in the ms_transform module and the source must include file
% ms_transform.hrl in STDLIB for this pseudo function to work. Failing to include the hrl file
% in the source results in a runtime error, not a compile time error. The include file is easiest
% included by adding line . to the source file.
-include_lib("stdlib/include/ms_transform.hrl").

query_count() ->
    ets:fun2ms(fun(Record) -> true end).

query_record() ->
    ets:fun2ms(fun({_, Record}) -> Record end).

-module(lamb_query_erlang_ffi).

-export([new/0, update_head/2, update_condition/2, update_body/2, test_query/2]).

new() ->
   Head = {'_', '$1'},
   Cond = [],
   Body = ['$1'],
   {Head, Cond, Body}.

update_head({_Head, Conditions, Body}, NewHead) ->
   {NewHead, Conditions, Body}.

update_condition({Head, Conditions, Body}, Condition) ->
   {Head, [Condition | Conditions], Body}.

update_body({Head, Conditions, _Body}, NewBody) ->
    {Head, Conditions, [interpret_body(NewBody)]}.

interpret_body(Body) when is_tuple(Body) -> {Body};
interpret_body(Body) -> Body.

test_query(Spec, Tuple) ->
    case ets:test_ms(Tuple, [Spec]) of
      {ok, false} -> no_match;
      {ok, Result} -> {spec, Result};
      {error, Errors} ->
        {invalid, lists:map(fun error/1, Errors)}
    end.

error({error, Message}) -> Message;
error({warning, Message}) -> Message.

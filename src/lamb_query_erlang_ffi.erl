-module(lamb_query_erlang_ffi).

-export([new/0, update_head/2, update_condition/2, update_body/2, test_query/2, build_matchhead/1, build_matchbody/1]).

new() ->
   Head = {'$0', '$1'},
   Cond = [],
   Body = ['$1'],
   {Head, Cond, Body}.

update_head({_Head, Conditions, Body}, NewHead) ->
   {NewHead, Conditions, Body}.

update_condition({Head, Conditions, Body}, Condition) ->
   {Head, [Condition | Conditions], Body}.

update_body({Head, Conditions, _Body}, NewBody) ->
    {Head, Conditions, [wrap_tuple(NewBody)]}.

% TODO: The traversal algorithm needs more basic datastructures.
% - Rename top function to traversal and localize wrap tuple. 
% - Will need a stronger test-suite, to understand what is allowed.
% - Will need to validate queries. On error, fail gracefully but log-out a message.  
wrap_tuple(Data) when is_tuple(Data) ->
    % When a tuple is found, traverse and wrap in tuple {}
    NewData = traverse(Data, 0, tuple_size(Data)),
    {NewData};
wrap_tuple(Data) ->
    Data.

traverse(Data, Size, Size) ->
    Data;
traverse(Data, Index, Size) when Size > Index ->
    Element = element(Index + 1, Data),
    NewData = setelement(Index + 1, Data, wrap_tuple(Element)),
    traverse(NewData, Index + 1, Size).

test_query(Spec, Tuple) ->
    case ets:test_ms(Tuple, [Spec]) of
      {ok, false} -> no_match;
      {ok, Result} -> {spec, Result};
      {error, Errors} ->
        {invalid, lists:map(fun error/1, Errors)}
    end.

error({error, Message}) -> Message;
error({warning, Message}) -> Message.

build_matchhead(Constructor) ->
  case Constructor of
      F when is_function(F, 0) ->
          F();
      F when is_function(F, 1) ->
          F('$1');
      F when is_function(F, 2) ->
          F('$1', '$2');
      F when is_function(F, 3) ->
          F('$1', '$2', '$3');
      F when is_function(F, 4) ->
          F('$1', '$2', '$3', '$4');
      F when is_function(F, 5) ->
          F('$1', '$2', '$3', '$4', '$5');
      F when is_function(F, 6) ->
          F('$1', '$2', '$3', '$4', '$5', '$6');
      F when is_function(F, 7) ->
          F('$1', '$2', '$3', '$4', '$5', '$6', '$7');
      F when is_function(F, 8) ->
          F('$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8');
      F when is_function(F, 9) ->
          F('$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9')
  end.

build_matchbody(Constructor) ->
    case Constructor of
    F when is_function(F, 1) ->
        F('$0');
    F when is_function(F, 2) ->
        F('$0','$1');
    F when is_function(F, 3) ->
        F('$0','$1', '$2');
    F when is_function(F, 4) ->
        F('$0','$1', '$2', '$3');
    F when is_function(F, 5) ->
        F('$0','$1', '$2', '$3', '$4');
    F when is_function(F, 6) ->
        F('$0','$1', '$2', '$3', '$4', '$5');
    F when is_function(F, 7) ->
        F('$0','$1', '$2', '$3', '$4', '$5', '$6');
    F when is_function(F, 8) ->
        F('$0','$1', '$2', '$3', '$4', '$5', '$6', '$7');
    F when is_function(F, 9) ->
        F('$0','$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8');
    F when is_function(F, 10) ->
        F('$0','$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9')
end.

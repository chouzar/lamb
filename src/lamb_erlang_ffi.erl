-module(lamb_erlang_ffi).

-export([create_table/4, table_id/1, table_info/1, lookup_set/2, lookup_bag/2, batch/3,
         continue/1, wrap_tuple/1, test_query/2]).

create_table(Name, Access, Kind, false)
    when is_atom(Name), is_atom(Access), is_atom(Kind) ->
    ets:new(Name, [Access, Kind]);
create_table(Name, Access, Kind, true)
    when is_atom(Name), is_atom(Access), is_atom(Kind) ->
    Name = ets:new(Name, [Access, Kind, named_table]),
    ets:whereis(Name).

table_id(Name) when is_atom(Name) ->
    case ets:whereis(Name) of
        undefined ->
            {error, nil};
        TableId ->
            {ok, TableId}
    end.

table_info(Table) ->
    case ets:info(Table) of
        undefined ->
            {error, nil};
        Info ->
            {ok, extract_info(Info)}
    end.

extract_info(Params) ->
    [{id, _},
     {decentralized_counters, _},
     {read_concurrency, _},
     {write_concurrency, _},
     {compressed, _},
     {memory, _},
     {owner, _},
     {heir, _},
     {name, Name},
     {size, _},
     {node, _},
     {named_table, NamedTable},
     {type, Type},
     {keypos, _},
     {protection, Protection}] =
        Params,
    {Name, Type, Protection, NamedTable}.

lookup_set(TableId, Index) ->
    case ets:lookup_element(TableId, Index, 2, error) of
        error ->
            [];
        Record ->
            [Record]
    end.

lookup_bag(TableId, Index) ->
    ets:lookup_element(TableId, Index, 2, []).

batch(TableId, Limit, MatchExpression) ->
    Result = ets:select(TableId, MatchExpression, Limit),
    partial_handle(Result).

continue(Step) ->
    Result = ets:select(Step),
    partial_handle(Result).

partial_handle(Result) ->
    case Result of
        {Objects, '$end_of_table'} ->
            {'end', Objects};
        {Objects, Continuation} ->
            {records, Objects, Continuation};
        '$end_of_table' ->
            {'end', []}
    end.

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
        {ok, false} ->
            no_match;
        {ok, Result} ->
            {spec, Result};
        {error, Errors} ->
            {invalid, lists:map(fun error/1, Errors)}
    end.

error({error, Message}) ->
    Message;
error({warning, Message}) ->
    Message.

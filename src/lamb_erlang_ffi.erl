-module(lamb_erlang_ffi).

-export([create_table/4, table_id/1, search/1, search/3]).

create_table(Name, Access, Kind, false) when is_atom(Name), is_atom(Access), is_atom(Kind) ->
    ets:new(Name, [Access, Kind]);
create_table(Name, Access, Kind, true) when is_atom(Name), is_atom(Access), is_atom(Kind) ->
    ets:new(Name, [Access, Kind, named_table]).

table_id(Table) ->
    case ets:info(Table, id) of
        undefined ->
            {error, nil};
        TableId ->
            {ok, TableId}
    end.

search(Step) ->
    Result = ets:select(Step),
    partial_handle(Result).

search(TableId, Limit, MatchExpression) ->
    Result = ets:select(TableId, MatchExpression, Limit),
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

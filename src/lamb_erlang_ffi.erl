-module(lamb_erlang_ffi).

-export([create_table/3, table_id/1, get/2, search/1, search/3]).

create_table(Store, Access, Name) when is_atom(Access), is_atom(Name) ->
    Options =
        case Access of
            private ->
                [Store, private];
            protected ->
                [Store, protected, named_table];
            public ->
                [Store, public, named_table]
        end,

    ets:new(Name, Options).

table_id(Table) ->
    case ets:info(Table, id) of
        undefined ->
            {error, nil};
        TableId ->
            {ok, TableId}
    end.

get(TableId, Index) ->
    case ets:lookup(TableId, Index) of
        [{_, Record}] ->
            {ok, Record};
        [] ->
            {error, nil}
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

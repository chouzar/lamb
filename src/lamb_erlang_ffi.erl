-module(lamb_erlang_ffi).

-export([create_table/2, table_id/1, get/2, search/1, search/2, search/3, count/2]).

create_table(Access, Name) when is_atom(Access), is_atom(Name) ->
    DefaultOptions = [set],

    Options =
        case Access of
            private ->
                [private | DefaultOptions];
            protected ->
                [protected, named_table | DefaultOptions];
            public ->
                [public, named_table | DefaultOptions]
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
        [Record] ->
            {ok, Record};
        [] ->
            {error, nil}
    end.

search(Step) ->
    Result = ets:select(Step),
    partial_handle(Result).

search(Step, MatchExpression) ->
    ets:select(Step, MatchExpression).

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

count(TableId, MatchExpression) ->
    NewMatchExpression =
        lists:map(fun({Head, Conditions, _Body}) -> {Head, Conditions, [true]} end,
                  MatchExpression),
    ets:select_count(TableId, NewMatchExpression).

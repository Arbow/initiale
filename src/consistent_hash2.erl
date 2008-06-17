-module(consistent_hash2).

-define(MAX_INT, 16#FFFFFFFF).
%-export([new/0,add/2, lookup/2]).
-export([new_ets/1, free_ets/1, insert_ets/3, delete_ets/2, lookup_nearest/2, lookup_or_next/2]).
-export([test/0]).

test() ->
    consistent_hash2:new_ets(t),
    lists:foreach(fun(Num) -> consistent_hash2:insert_ets(t, Num, void) end, 
        lists:seq(10, 16#FFFFFFFF, 10000)),
    %test lookup_nearest/2
    {10010,void} = consistent_hash2:lookup_nearest(t, 10000),
    {10,void} = consistent_hash2:lookup_nearest(t, 5010),
    {10010,void} = consistent_hash2:lookup_nearest(t, 5011),
    {10,void} = consistent_hash2:lookup_nearest(t, 0),
    {4294960010,void} = consistent_hash2:lookup_nearest(t, 16#FFFFFFFF-10000),
    %test lookup_or_next/2
    {10,void} = consistent_hash2:lookup_or_next(t, 4294960011),
    {4294960010,void} = consistent_hash2:lookup_or_next(t, 4294960009),
    {10010,void} = consistent_hash2:lookup_or_next(t, 20),
    consistent_hash2:free_ets(t),
    success.

new_ets(TableName) ->
    ets:new(TableName, [ordered_set, named_table]).

insert_ets(TableName, Key, Value) when is_integer(Key) ->
    ets:insert(TableName, {Key, Value}).

delete_ets(TableName, Key) when is_integer(Key) ->
    ets:delete(TableName, Key).

lookup_nearest(Table, Key) when is_integer(Key) ->
    TableSize = ets:info(Table, size),
    if
        TableSize =:= 0 -> {nil, nil};
        true ->
            case ets:lookup(Table, Key) of
                [Item] -> Item;
                [] -> lookup_nearest1(Table, Key, ets:prev(Table, Key), ets:next(Table, Key))
            end
    end.
    
lookup_nearest1(Table, Key, '$end_of_table', NextKey) ->
    [{LastKey, LastValue}] = ets:lookup(Table, ets:last(Table)),
    if
        ?MAX_INT-LastKey+Key =< NextKey-Key -> {LastKey, LastValue};
        true -> 
            [NextItem] = ets:lookup(Table, NextKey),
            NextItem
    end;
lookup_nearest1(Table, Key, PrevKey, '$end_of_table') ->
    [{FirstKey, FirstValue}] = ets:lookup(Table, ets:first(Table)),
    if
        Key-PrevKey =< ?MAX_INT-Key+FirstKey -> 
            [PrevItem] = ets:lookup(Table, PrevKey),
            PrevItem;
        true -> {FirstKey, FirstValue}
    end;
lookup_nearest1(Table, Key, PrevKey, NextKey) ->
    if 
        Key-PrevKey =< NextKey-Key -> 
            [PrevItem] = ets:lookup(Table, PrevKey),
            PrevItem;
        true -> 
            [NextItem] = ets:lookup(Table, NextKey),
            NextItem
    end.
    
lookup_or_next(Table, Key) when is_integer(Key) ->
    TableSize = ets:info(Table, size),
    if
        TableSize =:= 0 -> {nil, nil};
        true -> 
            case ets:lookup(Table, Key) of
                [Item] -> Item;
                [] ->
                    case ets:next(Table, Key) of 
                        '$end_of_table' -> 
                            [FirstItem] = ets:lookup(Table, ets:first(Table)),
                            FirstItem;
                        NextKey ->
                            [NextItem] = ets:lookup(Table, NextKey),
                            NextItem
                    end
            end
    end.

free_ets(Table) -> ets:delete(Table).

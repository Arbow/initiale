%% =====================================================================
%% A simple consistent hash implement
%% Author: avindev@gmail.com
%% =====================================================================
-module(consistent_hash).

-export([new/0,add/2, lookup/2]).

-export([test/0]).

new() -> {treemap:empty(), gb_sets:new()}.  % {Buckets, EntryList}

add({Key, Value}, {TreeMap, Sets}) ->
    NewTreeMap = add_hash_keys({Key, Value}, TreeMap),
    {ok, {NewTreeMap, Sets}}.
    
add_hash_keys({Key, Value}, TreeMap) ->
    %io:format("TreeMap=~p~n",[TreeMap]),
    add_hash_keys({Key, Value}, TreeMap, 50).

add_hash_keys(_, TreeMap, 0) ->
    TreeMap;
add_hash_keys({Key, Value}, TreeMap, Factor) ->
    Digest = erlang:md5(Key ++ "-" ++ integer_to_list(Factor)),
    <<I1:32,I2:32,I3:32,I4:32>> = Digest,
    NewTreeMap1 = treemap:enter(I1, Value, TreeMap),
    NewTreeMap2 = treemap:enter(I2, Value, NewTreeMap1),
    NewTreeMap3 = treemap:enter(I3, Value, NewTreeMap2),
    NewTreeMap4 = treemap:enter(I4, Value, NewTreeMap3),
    add_hash_keys({Key, Value}, NewTreeMap4, Factor-1).
    
lookup(Key, {TreeMap, Sets}) ->
    Digest = erlang:md5(Key),
    <<I1:32,I2:32,I3:32,I4:32>> = Digest,
    HashKey = I1 bxor I2 bxor I3 bxor I4,
    NearestEntry = treemap:lookup_nearest(HashKey,TreeMap),
    case NearestEntry of 
        {nil, nil} -> nil;
        {_, Value} -> Value
    end.
    
    
test() ->
    erlang:statistics(wall_clock),
    ServerList = lists:map(fun(I) -> "192.168.0." ++ integer_to_list(I) end, lists:seq(1, 100)),
    {_, Duration1} = erlang:statistics(wall_clock),
    io:format("ServerList: ~p, create list using time ~p~n", [ServerList, Duration1]),
    Bucket = consistent_hash:new(),
    {_, UpdatedBucket} = lists:mapfoldl(fun(I,B)-> consistent_hash:add({I,list_to_atom(I)},B) end, 
        Bucket, ServerList),
    {_, Duration2} = erlang:statistics(wall_clock),
    {TreeMap,_} = UpdatedBucket,
    io:format("Bucket size is ~p, initial bucket using time:~p~n", [treemap:size(TreeMap), Duration2]),
    Mapping = lists:map(fun(Key)->{Key, consistent_hash:lookup(Key, UpdatedBucket)} end, 
        lists:map(fun(Num)->integer_to_list(Num) end, lists:seq(0, 1000000))),
    {_, Duration3} = erlang:statistics(wall_clock),
    io:format("1000000 times lookup finished, total time:~p~n", [Duration3]),
    io:format("Server hit stat:~n~p~n", [lists:keysort(2, reduce(collect(map(Mapping))))]),
    {_, Duration4} = erlang:statistics(wall_clock),
    io:format("MapReduce using time:~p~n", [Duration4]),
    ok.
    
map(List) -> lists:map(fun({Key, ServerAtom}) -> {ServerAtom, Key} end, List).

reduce(List) -> lists:map(fun({Key, VList}) -> {Key, erlang:length(VList)} end, List).

collect([]) -> [];
collect(List) -> collect1(lists:keysort(1,List)).

collect1([{Key,Value}|SortedListTail]) ->
    collect1(SortedListTail, [{Key, [Value]}]).
collect1([], CollectedList) -> CollectedList;
collect1([{Key,Value}|SortedListTail], [{Key, VList}|T]) ->
    collect1(SortedListTail, [{Key, [Value|VList]}|T]);
collect1([{Key,Value}|SortedListTail], CollectedList) ->
    collect1(SortedListTail, [{Key, [Value]}|CollectedList]).



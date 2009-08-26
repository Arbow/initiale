-module(shiftand).

-export([compile/1, match/2]).

-define(BARRAY,
<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).


compile(Array, PatternLength, Mask, [], _) ->
    {shiftand_method, Array, Mask, PatternLength};
compile(Array, PatternLength, Mask, [PatternHead|PatternTail], Index) ->
    <<First:PatternHead/bytes, Value, Rest/binary>> = Array,
    NewValue = Value bor (1 bsl Index),
    %io:format("Value=~p, NewValue=~p~n", [Value, NewValue]),
    NewArray = <<First/binary, NewValue:8, Rest/binary>>,
    %io:format("~p~n",[NewArray]),
    compile(NewArray, PatternLength, Mask, PatternTail, Index+1).
    
compile(Pattern) when is_binary(Pattern) ->
    compile(?BARRAY, size(Pattern), 1 bsl (size(Pattern)-1), binary_to_list(Pattern), 0);
compile(Pattern) when is_list(Pattern) ->
    compile(?BARRAY, length(Pattern), 1 bsl (length(Pattern)-1), Pattern, 0).

match({shiftand_method, Pattern, Mask, Length}, Text) when is_binary(Text) ->
    match_first(Pattern, Mask, Length, binary_to_list(Text), 0, 0);
match({shiftand_method, Pattern, Mask, Length}, Text) when is_list(Text) ->
    match_first(Pattern, Mask, Length, Text, 0, 0).

match_first(_, _, _, [], _, _) ->
    -1;
match_first(Pattern, Mask, Length, [TextHead|TextTail], D, Pos) ->
    D1 = D bsl 1,
    D2 = D1 bor 1,
    <<_:TextHead/bytes, Value, _/binary>> = Pattern,
    D3 = D2 band Value,
    %io:format("D1=~p,D2=~p,D3=~p,Value=~p~n", [D1,D2,D3,Value]),
    if
	(D3 band Mask) =/= 0 ->
	    Pos - Length +1;
	true ->
	    match_first(Pattern, Mask, Length, TextTail, D3, Pos+1)
    end.


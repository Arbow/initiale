-module(ip_locator).
-define(DB_FILE, "QQWry.Dat").

-export([main/0]).


%%
%% Load db file to binary
%%
load_db() ->
	file:read_file(?DB_FILE).

%%
%% Convert IP address to integer
%%
ip_to_integer([S1,S2,S3,S4]) ->
	I1 = list_to_integer(S1),
	I2 = list_to_integer(S2),
	I3 = list_to_integer(S3),
	I4 = list_to_integer(S4),
	<<I:4/big-integer-unit:8>> = <<I1,I2,I3,I4>>,
	I;
ip_to_integer(IpStr) ->
	ip_to_integer(string:tokens(IpStr, ".")).

%%
%% Convert integer to IP address (binary)
%%
integer_to_ip_bin(Number, little) ->
	<<Number:4/little-integer-unit:8>>;
integer_to_ip_bin(Number, big) ->
	<<Number:4/big-integer-unit:8>>.
%% native - default little endian
integer_to_ip_bin(Number) ->
	<<Number:4/little-integer-unit:8>>.


%%
%% Get Index Area Offset
%% 
parse_index_offset(<<BOffset:4/little-integer-unit:8, EOffset:4/little-integer-unit:8, _/binary>>=Bin) ->
	IndexCount = (EOffset - BOffset) div 7 + 1,
	%io:format("Index begin offset: ~p, end offset ~p, Total ~p~n", [BOffset, EOffset, IndexCount]),
	%IndexOffset = BOffset-8,
	%<<_:8/bytes, Data:IndexOffset/bytes, Index/binary>> = Bin,
	%io:format("File size is ~p, Data size is ~p, Index size is ~p~n", [size(Bin), size(Data), size(Index)]),
	%ip_index_iterator(0, Index),
	{IndexCount, BOffset, EOffset}.


%%
%% Find IP record offset in index area
%%
find_ip_record_offset(Ip, Left, Right, BOffset, Data) when (Right-Left =< 1) and ((Ip band 16#ffffff00) == 16#ffffff00) ->
	Offset = BOffset + Right * 7,
	<<_:Offset/bytes, _SeekIp:4/little-integer-unit:8, DataIndex:3/little-integer-unit:8, _/binary>> = Data,
	{found, DataIndex};
find_ip_record_offset(Ip, Left, Right, BOffset, Data) when Right-Left =< 1 ->
	Offset = BOffset + Left * 7,
	<<_:Offset/bytes, _SeekIp:4/little-integer-unit:8, DataIndex:3/little-integer-unit:8, _/binary>> = Data,
	{found, DataIndex};
find_ip_record_offset(Ip, Left, Right, BOffset, Data) ->
	Middle = (Left + Right) div 2,
	Offset = BOffset + Middle * 7,
	%io:format("Seeking index, left=~p, middle=~p, right=~p~n", [Left, Middle, Right]),
	<<_:Offset/bytes, SeekIp:4/little-integer-unit:8, DataIndex:3/little-integer-unit:8, _/binary>> = Data,
	%io:format("Search IP is ~p, Seeking IP is ~p~n", [Ip, SeekIp]),
	if
		SeekIp == Ip -> {found, DataIndex};
		SeekIp < Ip  -> find_ip_record_offset(Ip, Middle, Right, BOffset, Data);
		SeekIp > Ip -> find_ip_record_offset(Ip, Left, Middle, BOffset, Data)
	end.


%%
%% Get ip record in record area
%%
get_ip_record(Offset, Data) ->
	case Data of
		%% Redirect Mode 1
		<<_:Offset/bytes, _Ip:4/bytes, 1, CountryOffset:3/little-integer-unit:8, _/binary>> ->
			get_ip_record_mode1(Data, CountryOffset);
		%% Redirect Mode 2
		<<_:Offset/bytes, _Ip:4/bytes, 2, CountryOffset:3/little-integer-unit:8, _/binary>> ->
			get_ip_record_mode2(Data, CountryOffset, Offset+8);
		%% Simple Mode
		<<_:Offset/bytes, _Ip:4/bytes, Rest/binary>> ->
			%io:format("Record mode normal~n"),
			Country = extract_str_end_with_zero(Offset+4, Data),
			Area = extract_str_end_with_zero(Offset+4+size(Country)+1, Data),
			{result, Country, Area}
	end.

%%
%% Get ip record in record area - redirect mode 1
%%
get_ip_record_mode1(Data, CountryOffset) ->
	%io:format("Record redirect mode 1~n"),
	case Data of
		%% Yet another redirect - mode 2
		<<_:CountryOffset/bytes, 2, NewCountryOffset:3/little-integer-unit:8, 1, NewAreaOffset:3/little-integer-unit:8, _/binary>> ->
			io:format("CountryTag is 2, AreaTag is 1, redirect mode 2~n"),
			get_ip_record_mode2(Data, NewCountryOffset, NewAreaOffset);
		<<_:CountryOffset/bytes, 2, NewCountryOffset:3/little-integer-unit:8, 2, NewAreaOffset:3/little-integer-unit:8, _/binary>> ->
			io:format("CountryTag is 2, AreaTag is 2, redirect mode 2~n"),
			get_ip_record_mode2(Data, NewCountryOffset, NewAreaOffset);
		<<_:CountryOffset/bytes, 2, NewCountryOffset:3/little-integer-unit:8, _/binary>> ->
			io:format("CountryTag is 2, redirect mode 2~n"),
			get_ip_record_mode2(Data, NewCountryOffset, CountryOffset+4);
		_ ->
			%io:format("Simple redirect mode 1~n"),
			Country = extract_str_end_with_zero(CountryOffset, Data),
			AreaOffset = CountryOffset+size(Country)+1,
			case Data of
				<<_:AreaOffset/bytes, 2, NewAreaOffset:3/little-integer-unit:8, _/binary>> ->
					Area = extract_str_end_with_zero(NewAreaOffset, Data),
					{result, Country, Area};
				_ ->
					Area = extract_str_end_with_zero(CountryOffset+size(Country)+1, Data),
					{result, Country, Area}
			end
	end.

%%
%% Get ip record in record area - redirect mode 2
%%
get_ip_record_mode2(Data, CountryOffset, AreaOffset) ->
	%io:format("Record redirect mode 2~n"),
	Country = extract_str_end_with_zero(CountryOffset, Data),
	Area = extract_str_end_with_zero(AreaOffset, Data),
	{result, Country, Area}.



%%
%% Extract string end with 0x0 in binary
%%
extract_str_end_with_zero(Offset, Data) ->
	extract_str_end_with_zero(Offset, Data, <<>>).
extract_str_end_with_zero(Offset, Data, Buf) ->
	case Data of
		<<_:Offset/bytes, 0, _/binary>> -> Buf;
		<<_:Offset/bytes, Bin:1/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:2/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:3/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:4/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:5/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:6/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:7/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:8/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:9/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:10/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:11/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:12/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:13/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:14/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:15/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:16/bytes, 0, _/binary>> -> <<Buf/binary, Bin/binary>>;
		<<_:Offset/bytes, Bin:16/bytes, _/binary>> -> extract_str_end_with_zero(Offset+16, Data, <<Buf/binary, Bin/binary>>)
	end.


%%
%% Main entry
%%
main() ->
	{ok, Data} = load_db(),
	{IndexCount, BOffset, EOffset} = parse_index_offset(Data),
	%IPStr = "192.168.0.204",
	%IPStr = "10.0.0.172",
	IPStr = "58.83.160.25",
	Ip = ip_to_integer(IPStr),
	case find_ip_record_offset(Ip, 0, IndexCount, BOffset, Data) of
		{found, RecordOffset} ->
			%io:format("Get record offset ~p~n", [RecordOffset]),
			{result, Country, Area} = get_ip_record(RecordOffset, Data),
			io:format("IP:~p, Country:~p, Area:~p~n", [IPStr, Country, Area]);
		_ ->
			io:format("Record offset not found !!!~n")
	end,
	init:stop().


%% Use to debug
ip_index_iterator(Offset, Bin) when Offset == size(Bin) ->
	ok;
ip_index_iterator(Offset, Bin) ->
	<<_:Offset/bytes,A1,A2,A3,A4,_/binary>> = Bin,
	io:format("IP is ~p.~p.~p.~p ~n", [A1,A2,A3,A4]),
	<<_:Offset/bytes,IP:4/little-integer-unit:8,_/binary>> = Bin,
	io:format("IP number is ~p~n", [IP]),
	ip_index_iterator(Offset+7, Bin).

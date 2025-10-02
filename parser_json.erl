-module(parser_json).

-export([decode/1]).

decode(Json) ->
  List = decode(Json, <<>>, [], 0),
  {Kay, Value} = filter_kay_or_value(List, <<>>, [], []),
  NewValue = check_value(Value, <<>>, []),
  create_map(Kay, NewValue, #{}).

decode(<<>>, BinAcc, Acc, 0) ->
  lists:reverse([BinAcc | Acc]);
decode(<<"{", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, BinAcc, Acc, Depth);
decode(<<"}", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, BinAcc, Acc, Depth);
decode(<<"\n", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, BinAcc, Acc, Depth);
decode(<<"'", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, BinAcc, Acc, Depth);
decode(<<"[", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, <<BinAcc/binary, "[">>, Acc, Depth + 1);
decode(<<"]", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, <<BinAcc/binary, "]">>, Acc, Depth - 1);
decode(<<",", Rest/binary>>, BinAcc, Acc, 0) ->
  decode(Rest, <<>>, [BinAcc | Acc], 0);
decode(<<",", Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, <<BinAcc/binary, ",">>, Acc, Depth);
decode(<<X, Rest/binary>>, BinAcc, Acc, Depth) ->
  decode(Rest, <<BinAcc/binary, X>>, Acc, Depth).

filter_kay_or_value([], <<>>, Kay, Value) ->
  {lists:reverse(Kay), lists:reverse(Value)};
filter_kay_or_value([<<":", Rest/binary>> | T], BinAcc, Kay, Value) ->
  filter_kay_or_value(T, <<>>, [BinAcc | Kay], [Rest | Value]);
filter_kay_or_value([<<"[", Rest/binary>> | T], BinAcc, Kay, Value) ->
  filter_kay_or_value(T, <<BinAcc/binary, Rest>>, Kay, Value);
filter_kay_or_value([<<"]", Rest/binary>> | T], BinAcc, Kay, Value) ->
  filter_kay_or_value([Rest | T], <<>>, Kay, [BinAcc | Value]);
filter_kay_or_value([<<X, Rest/binary>> | T], BinAcc, [], []) ->
  filter_kay_or_value([Rest | T], <<BinAcc/binary, X>>, [], []);
filter_kay_or_value([<<X, Rest/binary>> | T], BinAcc, Kay, Value) ->
  filter_kay_or_value([Rest | T], <<BinAcc/binary, X>>, Kay, Value).

create_map([], [], Map) ->
  Map;
create_map([], [<<" ">>], Map) ->
  Map;
create_map([H | T], [H1 | T1], Map) ->
  Kay = trim_binary(H),
  Value = trim_binary(H1),
  create_map(T, T1, Map#{Kay => Value}).

check_value([], <<>>, Acc) ->
  lists:reverse(Acc);
check_value([], BinAcc, Acc) ->
  lists:reverse([BinAcc | Acc]);
check_value([<<"[", Rest/binary>> | T], BinAcc, Acc) ->
  {Kay, Value} = get_kay_value(Rest, <<>>, [], []),
  {List1, List2} = get_list_from_list(Value, [], []),
  NewList1 = delete_trash(List1, <<>>, []),
  NewList2 = cutter_list(List2, <<>>, [], []),
  {List3, List4} = get_list_from_list(NewList2, [], []),
  NewList4 = one_list(List4),
  {K, V} = kay_and_value(List3, [], []),
  Map0 = create_map(K, V, #{}),
  TakeValue = lists:reverse([NewList4 |NewList1]),
  TakeValue0 = lists:reverse([Map0 |lists:reverse(TakeValue)]),
  {L1, L2} = get_map_from_value(TakeValue0, [], []),
  Map0 = get_map(L2),
  Map = create_map(Kay, L1, #{}),
  check_value(T, BinAcc, [[Map, Map0] | Acc]);
check_value([<<>> | T], BinAcc, Acc) ->
  check_value(T, <<>>, [BinAcc | Acc]);
check_value([<<X, Rest/binary>> | T], BinAcc, Acc) ->
  check_value([Rest | T], <<BinAcc/binary, X>>, Acc).

get_kay_value(<<>>, <<>>, Kay, Value) ->
  {lists:reverse(Kay), lists:reverse(Value)};
get_kay_value(<<" ">>, BinAcc, Kay, Value) ->
  get_kay_value(<<>>, BinAcc, Kay, Value);
get_kay_value(<<>>, BinAcc, Kay, Value) ->
  get_kay_value(<<>>, <<>>, Kay, [BinAcc | Value]);
get_kay_value(<<"[", Rest/binary>>, BinAcc, Kay, Value) ->
  List = create_list(Rest, <<>>, []),
  get_kay_value(<<>>, BinAcc, Kay, [List | Value]);
get_kay_value(<<"]", Rest/binary>>, BinAcc, Kay, Value) ->
  get_kay_value(Rest, BinAcc, Kay, Value);
get_kay_value(<<",", Rest/binary>>, BinAcc, Kay, Value) ->
  get_kay_value(Rest, <<>>, Kay, [BinAcc | Value]);
get_kay_value(<<":", Rest/binary>>, BinAcc, Kay, Value) ->
  get_kay_value(Rest, <<>>, [BinAcc | Kay], Value);
get_kay_value(<<X, Rest/binary>>, BinAcc, Kay, Value) ->
  get_kay_value(Rest, <<BinAcc/binary, X>>, Kay, Value).

create_list(<<>>, <<>>, Acc) ->
  lists:reverse(Acc);
create_list(<<>>, BinAcc, Acc) ->
  create_list(<<>>, <<>>, [BinAcc | Acc]);
create_list(<<"]", Rest/binary>>, BinAcc, Acc) ->
  create_list(Rest, BinAcc, Acc);
create_list(<<",", Rest/binary>>, BinAcc, Acc) ->
  create_list(Rest, <<>>, [BinAcc | Acc]);
create_list(<<X, Rest/binary>>, BinAcc, Acc) ->
  create_list(Rest, <<BinAcc/binary, X>>, Acc).

get_list_from_list([], Acc, Acc1) ->
  {lists:reverse(Acc), lists:reverse(Acc1)};
get_list_from_list([H| T], Acc, Acc1) when is_binary(H)  ->
  get_list_from_list(T, [H | Acc], Acc1);
get_list_from_list([H | T] , Acc, Acc1) when is_list(H) ->
  get_list_from_list(T, Acc, [H | Acc1]).

delete_trash([], <<>>, Acc) ->
  lists:reverse(Acc);
delete_trash([<<>> | T], BinAcc, Acc)  ->
  delete_trash(T, <<>>, [BinAcc |Acc]);
delete_trash([<<" ">> |T], BinAcc, Acc) ->
  delete_trash(T, BinAcc, Acc);
delete_trash([<<X, Rest/binary>> | T], BinAcc, Acc) ->
  delete_trash([Rest | T], <<BinAcc/binary, X>>, Acc).

cutter_list([[]], <<>>, Acc, Acc1) ->
  [Acc| lists:reverse(Acc1)];
cutter_list([[<<":", Rest/binary>> | T]], BinAcc, Acc, Acc1) ->
  cutter_list([T], <<>>, Acc, [BinAcc, Rest | Acc1]);
cutter_list([[<<>> | T]], BinAcc, Acc, Acc1) ->
  cutter_list([T], <<>>, [BinAcc | Acc], Acc1);
cutter_list([[<<X, Rest/binary>> | T]], BinAcc, Acc, Acc1) ->
  cutter_list([[Rest | T]], <<BinAcc/binary, X>>, Acc, Acc1).

one_list([H]) ->
  H.

kay_and_value([], Acc0, Acc2) ->
  Acc1 = trim_binary(Acc0),
  {lists:reverse(Acc2), lists:reverse(Acc1)};
kay_and_value([H, H1| T], Acc1, Acc2) ->
  kay_and_value(T, [H | Acc1], [H1 | Acc2]).

get_map_from_value([], Acc, Acc1) ->
  {lists:reverse(Acc), lists:reverse(Acc1)};
get_map_from_value([H|T], Acc, Acc1) when is_map(H) ->
  get_map_from_value(T, Acc, [H | Acc1]);
get_map_from_value([H|T], Acc, Acc1) when is_list(H) ->
  get_map_from_value(T, [H | Acc], Acc1);
get_map_from_value([H|T], Acc, Acc1) when is_binary(H) ->
  get_map_from_value(T, [H | Acc], Acc1).

get_map([Map]) ->
  Map.

trim_binary(List) when is_list(List) ->
  trim_for_list(List, []);
trim_binary(Bin) when is_binary(Bin) ->
  string:trim(Bin).

trim_for_list([], Acc) ->
  lists:reverse(Acc);
trim_for_list([H | T], Acc) when is_map(H) ->
  trim_for_list(T, [H | Acc]);
trim_for_list([H | T], Acc) when is_binary(H) ->
  H1 = check_bin(H, <<>>),
  trim_for_list(T, [ string:trim(H1) | Acc]).

check_bin(<<>>, BinAcc) ->
  BinAcc;
check_bin(<<"[", Rest/binary>>, BinAcc) ->
  check_bin(Rest, BinAcc);
check_bin(<<X, Rest/binary>>, BinAcc) ->
  check_bin(Rest, <<BinAcc/binary, X>>).
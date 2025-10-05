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
filter_kay_or_value([<<>> | T], BinAcc, Kay, Value) ->
  filter_kay_or_value(T, BinAcc, Kay, Value);
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
create_map([], [[]], Map) ->
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
  {L0, L01} = list_two_list(Value, [], []),
  {L, L1} = list(L01, <<>>, [], []),
  List = list_add_list(L, L0),
  {Nl, Nl2} = create_new_list(List, [], []),
  New_list = create_one_list(Nl, Nl2, []),
  NL1 = one_list(L1),
  NL01 = trim_binary(NL1),
  New_list2 = split_members(NL01),
  {Map1, Map01} = create_two_map(New_list2),
  Map = create_map(Kay, New_list, #{}),
  check_value(T, <<>>, [BinAcc, [Map, Map1, Map01]| Acc]);
check_value([<<>> | T], BinAcc, Acc) ->
  check_value(T, <<>>, [BinAcc | Acc]);
check_value([<<X, Rest/binary>> | T], BinAcc, Acc) ->
  check_value([Rest | T], <<BinAcc/binary, X>>, Acc).

get_kay_value(<<>>, <<>>, Kay, Value) ->
  {lists:reverse(Kay), lists:reverse(Value)};
get_kay_value(<<" ">>, BinAcc, Kay, Value) ->
  get_kay_value(<<>>, BinAcc, Kay, Value);
get_kay_value(<<>>, BinAcc, Kay, Value) ->
  case string:trim(BinAcc) of
    <<>> -> get_kay_value(<<>>, <<>>, Kay, Value);
    Trimmed -> get_kay_value(<<>>, <<>>, Kay, [Trimmed | Value])
  end;
get_kay_value(<<"[", Rest/binary>>, BinAcc, Kay, Value) ->
  List = create_list(Rest, <<>>, []),
  get_kay_value(<<>>, BinAcc, Kay, [List| Value]);
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

trim_binary(Map) when is_map(Map) ->
   Map;
trim_binary(List) when is_list(List) ->
  trim_for_list(List, []);
trim_binary(Bin) when is_binary(Bin) ->
  string:trim(Bin).

trim_for_list([], Acc) ->
  lists:reverse(Acc);
trim_for_list([[H | T]], Acc) ->
  trim_for_list([H | T], Acc);
trim_for_list([H | T], Acc) when is_map(H) ->
  trim_for_list(T, [H | Acc]);
trim_for_list([H | T], Acc) when is_binary(H) ->
  H1 = check_bin(H, <<>>),
  trim_for_list(T, [ string:trim(H1) | Acc]).

one_list([H]) ->
  H.

check_bin(<<>>, BinAcc) ->
  BinAcc;
check_bin(<<"[", Rest/binary>>, BinAcc) ->
  check_bin(Rest, BinAcc);
check_bin(<<X, Rest/binary>>, BinAcc) ->
  check_bin(Rest, <<BinAcc/binary, X>>).

list([[]], <<>>, Acc, Acc2) ->
  {lists:reverse(Acc), Acc2};
list([[<<"name", Rest/binary>> | T]], Bin, Acc, Acc2) ->
  list([[]], <<>>, [Bin | Acc], [[<<"name", Rest/binary>> | T] | Acc2]);
list([[<<>> | T]], Bin, Acc, Acc2) ->
  list([T], <<>>, [Bin | Acc], Acc2);
list([[<<X, Rest/binary>> | T]], Bin, Acc, Acc1) ->
  list([[Rest | T]], <<Bin/binary, X>>, Acc, Acc1).

list_two_list([], Acc, Acc1) ->
  {lists:reverse(Acc), lists:reverse(Acc1)};
list_two_list([H | T], Acc, Acc1) when is_binary(H) ->
  list_two_list(T, [H | Acc], Acc1);
list_two_list([H | T], Acc, Acc1) when is_list(H) ->
  list_two_list(T, Acc, [H | Acc1]).

list_add_list(List, Acc) ->
  lists:reverse([List | lists:reverse(Acc)]).

bin([], Acc) ->
  lists:reverse(Acc);
bin([<<>> | T], Acc) ->
  bin(T, Acc);
bin([<<Bin/binary>> | T], Acc) ->
  bin(T, [<<Bin/binary>> | Acc]).

create_new_list([], Acc, Acc01) ->
  Acc0 = trim_binary(Acc01),
  Acc1 = bin(Acc0, []),
  {lists:reverse(Acc), lists:reverse(Acc1)};
create_new_list([H | T], Acc, Acc0) when is_list(H) ->
  create_new_list(T, Acc, [H | Acc0]);
create_new_list([H | T], Acc, Acc0) when is_binary(H) ->
  create_new_list(T, [H | Acc], Acc0).

create_one_list([], List, Acc) ->
  lists:reverse([List | Acc]);
create_one_list([H | T], List, Acc) when is_binary(H) ->
  create_one_list(T, List, [H | Acc]).

split_members(List) ->
  split_members(List, [], []).

split_members([], [], Acc) ->
  lists:reverse(Acc);
split_members([], Current, Acc) ->
  lists:reverse([lists:reverse(Current) | Acc]);
split_members([H | T], [], Acc) ->
  case is_name(H) of
    true -> split_members(T, [H], Acc);
    false -> split_members(T, [], Acc)
  end;
split_members([H | T], Current, Acc) ->
  case is_name(H) of
    true ->
      split_members(T, [H], [lists:reverse(Current) | Acc]);
    false ->
      split_members(T, [H | Current], Acc)
  end.

is_name(Bin) when is_binary(Bin) ->
  binary:match(Bin, <<"name:">>) =/= nomatch.

create_two_map(List) ->
  create_two_map(List, [], []).

create_two_map([], Acc, Acc1) ->
  {K, V} = kay_value(Acc, <<>>, [], [], []),
  {K1, V1} = kay_value(Acc1, <<>>, [], [], []),
  Map = create_map(K, V, #{}),
  Map1 = create_map(K1, V1, #{}),
  {Map, Map1};
create_two_map([H, H1| T], Acc, Acc1) ->
  create_two_map(T, [H | Acc], [H1 | Acc1]).

kay_value([[]], <<>>, V, Kay, Value) ->
  V1 = lists:reverse(V),
  Value1 = lists:reverse([V1 | Value]),
  {lists:reverse(Kay), Value1};
kay_value([[<<":", Rest/binary>> | T]], Bin, V, Kay, Value) ->
  case Rest of
    <<"[", ListRest/binary>> ->
      List = create_list(ListRest, <<>>, []),
      kay_value([T], <<>>, V, [Bin | Kay], [List | Value]);
    _ when Bin == <<"powers">> ->
      {ListRest, Remaining} = collect_list_items(T, [Rest]),
      kay_value([Remaining], <<>>, V, [Bin | Kay], [ListRest | Value]);
    _ ->
      kay_value([T], <<>>, V, [Bin | Kay], [Rest | Value])
  end;

kay_value([[<<>> | T]], Bin, V, Kay, Value) ->
  kay_value([T], <<>>, [Bin | V], Kay,Value);
kay_value([[<<X, Rest/binary>> | T]], Bin, V, Kay, Value) ->
  kay_value([[Rest | T]], <<Bin/binary, X>>, V, Kay, Value).

collect_list_items([], Acc) ->
  {lists:reverse(Acc), []};
collect_list_items([H | T], Acc) ->
  case is_key(H) of
    true -> {lists:reverse(Acc), [H | T]};
    false -> collect_list_items(T, [H | Acc])
  end.

is_key(Bin) when is_binary(Bin) ->
  BinTrim = string:trim(Bin),
  lists:any(fun(Prefix) -> binary:match(BinTrim, Prefix) =/= nomatch end,
    [<<"name:">>, <<"age:">>, <<"secretIdentity:">>, <<"powers:">>]).

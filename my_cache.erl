-module(my_cache).

-export([create/1, insert/4, lookup/2, delete_obsolete/1]).

-record(cache_entry, {value, expire_time}).
create(my_cache) ->
  ets:new(my_cache, [set, named_table]),
  ok.

insert(TableName, Kay, Value, Ttl) ->
  CurrentTime = erlang:system_time(seconds),
  ExpireAt = add_second(CurrentTime, Ttl),
  ets:insert(TableName, {Kay, #cache_entry{value = Value, expire_time = ExpireAt}}).

lookup(TableName, Key) ->
  CurrentTime = erlang:system_time(seconds),
  case ets:lookup(TableName, Key) of
    [{Key, #cache_entry{value=Value, expire_time=ExpireTime}}] when ExpireTime == undefined ->
      {ok, Value};
    [{Key, #cache_entry{value=Value, expire_time=ExpireTime}}] when is_integer(ExpireTime), ExpireTime > CurrentTime ->
      {ok, Value};
    _ ->
      undefined
  end.

delete_obsolete(TableName) ->
  CurrentTime = erlang:system_time(seconds),
  Entries = ets:tab2list(TableName),
  F = fun({_Key, #cache_entry{expire_time=undefined}}) -> false;
    ({_Key, #cache_entry{expire_time=ExpireTime}}) when ExpireTime =< CurrentTime -> true;
    (_) -> false
      end,
  KeysToDelete = [Key || {Key, Entry} <- Entries, F({Key, Entry})],
  [ets:delete(TableName, Key) || Key <- KeysToDelete],
  ok.

add_second(CurrentTime, Ttl) ->
  CurrentTime + Ttl.
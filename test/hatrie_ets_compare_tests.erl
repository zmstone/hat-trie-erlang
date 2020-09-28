%% This test compares segment prefix lookups with ETS.
%% e.g. if "a/b/c" is to be stored, the prefix lookups in this test
%% consist of "a/", "a/b", and "a/b/c"
%% In trie, it is only "a/b/c" stored, however in ETS, it has to enumerate
%% all prefixes and store them all (i.e. "a", "a/b", and "a/b/c")

-module(hatrie_ets_compare_tests).

-include_lib("eunit/include/eunit.hrl").

concurrent_test_() ->
  {timeout, 60, fun concurrent_test_run/0}.

concurrent_test_run() ->
  Name = trie1,
  try
    ok = hattrie:create(Name),
    Name = ets:new(Name, [named_table, public, set, {read_concurrency, true}]),
    {T1, ok} = timer:tc(fun() -> concurrent_run(ets, Name) end),
    {T2, ok} = timer:tc(fun() -> concurrent_run(trie, Name) end),
    log("\n time_ets=~p\ntime_trie=~p\n", [T1, T2]),
    EtsMem = ets:info(Name, memory) * erlang:system_info(wordsize),
    log(" size_ets=~p\nsize_trie=~p\n", [EtsMem, hattrie:bytes(trie1)]),
    ok
  after
    hattrie:destroy(Name),
    ets:delete(Name)
  end.

concurrent_run(Type, Name) ->
  Nums = lists:seq(1, 10),
  Pids = lists:map(fun(I) -> spawn_and_run(Type, Name, I) end, Nums),
  wait_for_pids(Pids).

wait_for_pids([]) -> ok;
wait_for_pids(Pids) ->
  receive
    {'DOWN', _, process, Pid, Info} ->
      true = lists:member(Pid, Pids), %% assert
      normal = Info,
      wait_for_pids(lists:delete(Pid, Pids))
  end.

spawn_and_run(Type, Name, Id) ->
  {Pid, _} = erlang:spawn_monitor(fun() -> worker_run(Type, Name, Id) end),
  Pid.

worker_run(Type, Name, _Id) ->
  Keys = hattrie_test_lib:generate_levels([4, 8, 64, 20]),
  lists:foreach(fun(Key) -> upsert(Type, Name, Key) end, Keys),
  lists:foreach(fun(Key) -> lookup(Type, Name, Key) end, Keys).

upsert(ets, Name, Key) ->
  hattrie_test_lib:foreach_prefix(Key, fun(Prefix) -> ets:insert(Name, {Prefix, <<>>}) end);
upsert(trie, Name, Key) ->
  hattrie:upsert(Name, Key, <<>>).

lookup(Type, Name, Key) ->
  F = case Type of
        ets -> fun(Prefix) -> ets:lookup(Name, Prefix) end;
        trie -> fun(Prefix) -> hattrie:lookup(Name, Prefix) end
      end,
  hattrie_test_lib:foreach_prefix(Key, F).

log(Fmt, Args) -> io:format(standard_error, Fmt, Args).

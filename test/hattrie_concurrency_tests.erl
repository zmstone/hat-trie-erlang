%% This test module spawns processes to insert/delete/read keys concurrently

-module(hattrie_concurrency_tests).

-include_lib("eunit/include/eunit.hrl").

concurrent_test_() ->
  {timeout, 60, fun concurrent_test_run/0}.

concurrent_test_run() ->
  Name = trie1,
  try
    ok = hattrie:create(Name),
    concurrent_run(Name)
  after
    hattrie:destroy(Name)
  end.

concurrent_run(Name) ->
  Pids = lists:map(fun(I) -> spawn_and_run(Name, I) end, lists:seq(1, 10)),
  wait_for_pids(Pids).

wait_for_pids([]) -> ok;
wait_for_pids(Pids) ->
  receive
    {'DOWN', _, process, Pid, Info} ->
      true = lists:member(Pid, Pids), %% assert
      normal = Info,
      wait_for_pids(lists:delete(Pid, Pids))
  end.

spawn_and_run(Name, Id) ->
  {Pid, _} = erlang:spawn_monitor(fun() -> worker_run(Name, Id) end),
  Pid.

worker_run(Name, _Id) ->
  Keys = hattrie_test_lib:generate_levels([2, 8, 64, 10]),
  lists:foreach(fun(Key) -> hattrie:upsert(Name, Key, <<>>) end, Keys),
  lists:foreach(fun(Key) -> hattrie:lookup(Name, Key) end, Keys).


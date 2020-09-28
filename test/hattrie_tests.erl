-module(hattrie_tests).

-include_lib("eunit/include/eunit.hrl").

create_2_names_test() ->
  try
    ?assertEqual(ok, hattrie:create(trie1)),
    ?assertEqual(ok, hattrie:create(trie2)),
    ?assertException(error, badarg, hattrie:create(trie1)),

    ?assertEqual([], hattrie:upsert(trie1, <<"key1">>, <<"value-a">>)),
    ?assertEqual([], hattrie:upsert(trie2, <<"key1">>, <<"value-b">>)),

    ?assertEqual([<<"value-a">>], hattrie:lookup(trie1, <<"key1">>)),
    ?assertEqual([<<"value-b">>], hattrie:lookup(trie2, <<"key1">>))
  after
    hattrie:destroy(trie1),
    hattrie:destroy(trie2)
  end,
  ?assertException(error, badarg, hattrie:destroy(trie1)).

crud_1_test() ->
  ?assertEqual(ok, hattrie:create(trie1)),
  try
    ?assertEqual([], hattrie:upsert(trie1, <<"key1">>, <<"value1">>)),
    ?assertEqual([], hattrie:lookup(trie1, <<"some-unknown-key">>)),
    ?assertEqual([<<"value1">>], hattrie:lookup(trie1, <<"key1">>))
  after
    hattrie:destroy(trie1)
  end.

crud_2_test() ->
  ?assertEqual(ok, hattrie:create(trie2)),
  try
    ?assertEqual([], hattrie:upsert(trie2, <<"key2">>, <<"value2">>)),
    ?assertEqual([<<"value2">>], hattrie:lookup(trie2, <<"key2">>)),
    ?assertEqual(ok, hattrie:delete(trie2, <<"key2">>)),
    ?assertEqual([], hattrie:lookup(trie2, <<"key2">>))
  after
    hattrie:destroy(trie2)
  end.

bench_1_test_() ->
  {timeout, 600,
   fun() ->
     ok = hattrie:create(trie1),
     ?assertEqual(0, hattrie:count(trie1)),
     Topics = hattrie_test_lib:generate_levels([2, 8, 64, 10]),
     {T, ok} =
       timer:tc(fun() -> lists:foreach(fun(T) -> hattrie:upsert(trie1, T, <<0>>)
                                       end, Topics) end ),
     io:format(standard_error, "took: ~p seconds\n", [T/1000000]),
     ?assert(length(Topics) >= hattrie:count(trie1)),
     io:format(standard_error, "hat-trie size: ~p\n", [hattrie:bytes(trie1)]),
     io:format(standard_error, "    term size: ~p\n", [iolist_size(Topics)]),
     hattrie:destroy(trie1),
     Tab = ets:new(name, [set]),
     {T2, ok} =
       timer:tc(fun() ->
         lists:foreach(fun(Topic) ->
           ets:insert(Tab, {iolist_to_binary(Topic), <<0>>}) end, Topics) end),
     EtsMem = ets:info(Tab, memory) * erlang:system_info(wordsize),
     io:format(standard_error, "     ets size: ~p\n", [EtsMem]),
     io:format(standard_error, "~p seconds to insert ets\n", [T2/1000000])
   end}.


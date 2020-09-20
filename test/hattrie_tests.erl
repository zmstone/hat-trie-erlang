-module(hattrie_tests).
-include_lib("eunit/include/eunit.hrl").

crud_1_test() ->
  ?assertEqual(ok, hattrie:create(trie1)),
  ?assertEqual([], hattrie:upsert(trie1, <<"key1">>, <<"value1">>)),
  ?assertEqual([], hattrie:lookup(trie1, <<"some-unknown-key">>)),
  ?assertEqual([<<"value1">>], hattrie:lookup(trie1, <<"key1">>)).

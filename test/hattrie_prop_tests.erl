-module(hattrie_prop_tests).

-define(EUNIT_NOAUTO, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_check_test_() ->
  {timeout, 60,
   fun prop_check_run/0
  }.

prop_check_run() ->
  Opts = [{numtests, 1000}, {to_file, user}],
  ?assert(
     proper:quickcheck(
       ?FORALL(Ops, prop_ops(), prop_check(Ops)),
       Opts)
  ).

prop_op() -> proper_types:oneof([lookup, delete]).
prop_key() -> proper_types:binary(100). %% trie key
prop_value() -> proper_types:binary(64). %% trie value
prop_ops() -> proper_types:list({prop_op(), prop_key(), prop_value()}).

prop_check(Ops) ->
  Name = trie_test,
  Ets = ets:new(Name, [set]),
  ok = hattrie:create(Name),
  try
    ok =:= do_prop_check(Ets, Name, Ops)
  after
    ets:delete(Ets),
    hattrie:destroy(Name)
  end.

do_prop_check(Ets, Trie, []) ->
  All = ets:tab2list(Ets),
  lists:foreach(
    fun({K, V}) ->
        ?assertEqual([V], hattrie:lookup(Trie, K))
    end, All);
do_prop_check(Ets, Trie, [{Op, K, V} | Ops]) ->
  ets:insert(Ets, {K, V}),
  hattrie:upsert(Trie, K, V),
  case Op of
    lookup ->
      ?assertEqual([V], hattrie:lookup(Trie, K));
    delete ->
      ets:delete(Ets, K),
      ?assertEqual(ok, hattrie:delete(Trie, K))
  end,
  do_prop_check(Ets, Trie, Ops).



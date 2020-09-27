-module(hattrie_test_lib).

-export([generate_levels/1]).

generate_levels(Levels) ->
  generate_levels("", Levels, []).

%% generate levels of random segments,
%% segments are delimited with '/'
generate_levels(Prefix, [], Acc) ->
  [Prefix | Acc];
generate_levels(Prefix, [N | Levels], Acc) ->
  lists:foldl(
    fun(_, AccIn) ->
        generate_levels([Prefix, "/", random()], Levels, AccIn)
    end, Acc, lists:seq(1, N)).

%% generate 1-32 random (printable) bytes
random() ->
  N = rand:uniform(22),
  Bin0 = base64:encode(crypto:strong_rand_bytes(N)),
  Bin = binary:replace(Bin0, <<"/">>, <<"-">>, [global]),
  hd(binary:split(Bin, <<"=">>, [trim])).


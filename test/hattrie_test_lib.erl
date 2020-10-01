-module(hattrie_test_lib).

-export([generate_levels/1,
         foreach_prefix/2,
         split_levels/1]).

generate_levels(Levels) ->
  generate_levels("", Levels, []).

%% generate levels of random segments,
%% segments are delimited with '/'
generate_levels(Prefix, [], Acc) ->
  [iolist_to_binary(Prefix) | Acc];
generate_levels(Prefix, [N | Levels], Acc) ->
  lists:foldl(
    fun(_, AccIn) ->
        case Prefix of
          "" -> generate_levels([random()], Levels, AccIn);
          _ -> generate_levels([Prefix, "/", random()], Levels, AccIn)
        end
    end, Acc, lists:seq(1, N)).

%% generate 1-32 random (printable) bytes
random() ->
  N = rand:uniform(22),
  Bin0 = base64:encode(crypto:strong_rand_bytes(N)),
  Bin = binary:replace(Bin0, <<"/">>, <<"-">>, [global]),
  hd(binary:split(Bin, <<"=">>, [trim])).

split_levels(Key) ->
  binary:split(Key, <<"/">>, [global]).

foreach_prefix(Key, Fun) ->
  Segs = split_levels(Key),
  foreach_prefix(tl(Segs), Fun, hd(Segs)).

foreach_prefix(Segs, Fun, Prefix) ->
  Fun(Prefix),
  case Segs of
    [] -> ok;
    [Seg | Rest] ->
      NewPrefix = <<Prefix/binary, "/", Seg/binary>>,
      foreach_prefix(Rest, Fun, NewPrefix)
  end.

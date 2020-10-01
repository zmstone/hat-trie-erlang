-module(hattrie).

%% Trie NIF APIs
-export([create/1,
         upsert/3,
         lookup/2,
         delete/2,
         count/1,
         bytes/1,
         destroy/1
        ]).

%% Dict NIF APIs
-export([add_seg/1,
         del_seg/1,
         find_seg/1,
         clear_segs/0
        ]).

%% Segment Trie APIs
-export([lookup_segs/2,
         upsert_segs/3,
         delete_segs/2
        ]).

-on_load(on_load/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).
-define(NOT_LOADED, exit({?MODULE, not_loaded, ?LINE})).

on_load() -> erlang:load_nif(so_name(), 0).

so_name() ->
 case code:priv_dir(?APPNAME) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", priv])) of
        true -> filename:join(["..", priv, ?LIBNAME]);
        _    -> filename:join([priv, ?LIBNAME])
      end;
    Dir ->
     filename:join(Dir, ?LIBNAME)
  end.

% -spec create(Name::atom()) -> ok.
create(_) -> ?NOT_LOADED.

% -spec upsert(Name::atom(), Key::binary(), Val::binary()) -> [OldVal::binary()].
upsert(_, _, _) -> ?NOT_LOADED.

% -spec lookup(Name::atom(), Key::binary()) -> [Val::binary()].
lookup(_, _) -> ?NOT_LOADED.

% -spec delete(Name::atom(), Key::binary()) -> ok.
delete(_, _) -> ?NOT_LOADED.

% -spec count(Name::atom()) -> integer().
count(_) -> ?NOT_LOADED.

% -spec bytes(Name::atom()) -> integer().
bytes(_) -> ?NOT_LOADED.

% -spec destroy(Name::atom()) -> ok.
destroy(_) -> ?NOT_LOADED.

% Segment should not contain <<0>>
% -spec add_seg(Segment::binary()) -> binary().
add_seg(_) -> ?NOT_LOADED.

% Segment should not contain <<0>>.
% -spec del_seg(Segment::binary()) -> ok.
del_seg(_) -> ?NOT_LOADED.

% Segment should not contain <<0>>.
% -spec find_seg(Segment::binary()) -> binary() | undefined.
find_seg(_) -> ?NOT_LOADED.

% -spec clear_segs() -> ok.
clear_segs() -> ?NOT_LOADED.

% Segment should not contain <<0>>.
% -spec lookup_segs(Name::atom(), [Segment::binary()]) -> [Val:binary()].
lookup_segs(Name, [_ | _] = Segments) ->
  case try_find_segs(Segments, []) of
    [] ->
      %% not all segments are found in the dict
      [];
    EncodedSegs ->
      %% Encoded segments now can be concatenated for lookup in the trie.
      ?MODULE:lookup(Name, iolist_to_binary(EncodedSegs))
  end.

% Segment should not contain <<0>>.
% -spec upsert_segs(Name::atom(), [Segment::binary()], Val::binary()) -> [OldVal::binary()].
upsert_segs(Name, [_ | _] = Segments, Val) ->
  %% ensure segments are added to dict
  EncodedSegs = lists:map(fun ?MODULE:add_seg/1, Segments),
  %% insert concatenated segments in the trie
  ?MODULE:upsert(Name, iolist_to_binary(EncodedSegs), Val).

% Segment should not contain <<0>>.
% -spec delete_segs(Name::atom(), [Segment::binary()]) -> ok.
delete_segs(Name, [_ | _] = Segments) ->
  EncodedSegs = find_segs(Segments, []),
  %% delete from trie
  ok = ?MODULE:delete(Name, iolist_to_binary(EncodedSegs)),
  %% delete from segment dict
  lists:foreach(fun(Seg) -> ?MODULE:del_seg(Seg) end, Segments).

%% ============================== internal functions =========================== 

%% Find (encoded) segments for the given segment list.
%% Return empty list as soon as it fails to find one.
try_find_segs([], EncodedSegs) -> lists:reverse(EncodedSegs);
try_find_segs([Seg | Segs], EncodedSegs) ->
  case ?MODULE:find_seg(Seg) of
    undefined  -> [];
    EncodedSeg -> try_find_segs(Segs, [EncodedSeg | EncodedSegs])
  end.

%% Find (encoded) segments for the given segment list.
%% Raise an error exception as soon as it fails to find one.
find_segs([], EncodedSegs) -> lists:reverse(EncodedSegs);
find_segs([Seg | Segs], EncodedSegs) ->
  case ?MODULE:find_seg(Seg) of
    undefined  -> erlang:error({unknown_segment, Seg});
    EncodedSeg -> find_segs(Segs, [EncodedSeg | EncodedSegs])
  end.



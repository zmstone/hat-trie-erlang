-module(hattrie).

-export([create/1,
         upsert/3,
         lookup/2,
         delete/2,
         count/1,
         bytes/1,
         destroy/1
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


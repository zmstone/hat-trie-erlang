#include <string.h>
#include "erl_nif.h"
#include "hat-trie.h"

static hattrie_t* find_by_name(ErlNifEnv*, int, const ERL_NIF_TERM*);
static ERL_NIF_TERM ok(ErlNifEnv*);
static ERL_NIF_TERM ok2(ErlNifEnv*, ERL_NIF_TERM);
static ERL_NIF_TERM error(ErlNifEnv*, ERL_NIF_TERM);

typedef char* TrieName;

static hattrie_t* global_trie = NULL;

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
  // TODO initialize a global mapping from name to tries
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv) {
  // TODO: release all tries
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
  // do nothing
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info) {
  // do nothing
  return 0;
}

static ERL_NIF_TERM
create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL != trie) {
    // already created
    return enif_make_badarg(env);
  }
  // TODO start using name
  global_trie = hattrie_create();
  // TODO end
  return ok(env);
}

static ERL_NIF_TERM
destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  // TODO: free all enif allocations
  hattrie_free(trie);
  return ok(env);
}

static ERL_NIF_TERM
count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  size_t c = hattrie_size(trie);
  ERL_NIF_TERM result = enif_make_int(env, (int)c);
  return ok2(env, result);
}

static ERL_NIF_TERM
bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  size_t c = hattrie_sizeof(trie);
  ERL_NIF_TERM result = enif_make_int(env, (int)c);
  return ok2(env, result);
}


// Create or update a key.
// Return [] if not value before, otherwise [Value].
static ERL_NIF_TERM
upsert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 3) {
    return enif_make_badarg(env);
  }
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  // get key input
  ErlNifBinary key;
  if(0 == enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    return enif_make_badarg(env);
  }
  // get value input
  ErlNifBinary val;
  if(0 == enif_inspect_iolist_as_binary(env, argv[2], &val)) {
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM result;
  value_t* value_p = hattrie_get(trie, (char*)key.data, key.size);
  if(0 == (*value_p)) {
    result = enif_make_list(env, 0);
  }else{
    ErlNifBinary* old_val= (ErlNifBinary*)(*value_p);
    result = enif_make_list(env, 1, enif_make_binary(env, old_val));
    enif_free(old_val);
  }

  ErlNifBinary *copy = enif_alloc(sizeof(ErlNifBinary));
  if(0 == enif_alloc_binary(val.size, copy)) {
    // failed to allocate
    return enif_raise_exception(env, enif_make_atom(env, "enif_alloc_binary_failed"));
  }

  memcpy(copy->data, val.data, val.size);
  (*value_p) = (value_t)(copy);

  return result;
}

static ERL_NIF_TERM
lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 2) {
    return enif_make_badarg(env);
  }
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  // get key input
  ErlNifBinary key;
  if(0 == enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    return enif_make_badarg(env);
  }
  value_t* value_p = hattrie_tryget(trie, key.data, key.size);
  if(0 == value_p) {
    // no value
    return enif_make_list(env, 0);
  }
  if(0 == (*value_p)) {
    // value deleted, TODO: check if this should happen if we never call hattrie_clear
    return enif_make_list(env, 0);
  }
  ErlNifBinary* bin = (ErlNifBinary*)(*value_p);
  ERL_NIF_TERM res = enif_make_binary(env, bin);
  return enif_make_list(env, 1, res);
}

static ERL_NIF_TERM
delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 2) {
    return enif_make_badarg(env);
  }
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  // get key input
  ErlNifBinary key;
  if(0 == enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    return enif_make_badarg(env);
  }
  return ok(env);
}

static ERL_NIF_TERM
ok(ErlNifEnv* env) {
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
ok2(ErlNifEnv* env, ERL_NIF_TERM result) {
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM
error(ErlNifEnv* env, ERL_NIF_TERM result) {
  return enif_make_tuple2(env, enif_make_atom(env, "error"), result);
}

// Parse trie name from arg[0],
// return NULL if failed to parse.
static TrieName
parse_arg_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  // ensure one arg (the name) passed in
  if(argc == 0) {
    return NULL;
  }
  // get lenght of the name
  unsigned int len = 0;
  ERL_NIF_TERM name_term = argv[0];
  if(0 == enif_get_atom_length(env, name_term, &len, ERL_NIF_LATIN1)) {
    return NULL;
  }
  // get the name
  TrieName name = (TrieName)enif_alloc(len + 1);
  if(0 == enif_get_atom(env, name_term, name, len + 1, ERL_NIF_LATIN1)) {
    enif_free(name);
    return NULL;
  }
  return name;
}

// Parse trie name from arg[0], and find the trie for it,
// return NULL if faile to parse or not found.
static hattrie_t*
find_by_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  TrieName name = parse_arg_name(env, argc, argv);
  if(NULL == name) {
    return NULL;
  }
  // TODO find per-name trie
  enif_free(name);
  return global_trie;
}

static ErlNifFunc nif_funcs[] = {
   {"create",  1, create},

   {"upsert",  3, upsert},
   {"lookup",  2, lookup},
   {"delete",  2, delete},

   {"count",   1, count},
   {"bytes",   1, bytes},

   {"destroy", 1, destroy}
};

ERL_NIF_INIT(hattrie, nif_funcs, &load, &reload, &upgrade, &unload);

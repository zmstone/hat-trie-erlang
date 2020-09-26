#include <string.h>
#include "erl_nif.h"
#include "hat-trie.h"

static hattrie_t* find_by_name(ErlNifEnv*, int, const ERL_NIF_TERM*);
static char* parse_arg_name(ErlNifEnv*, int, const ERL_NIF_TERM*, unsigned int*);
static ERL_NIF_TERM ok(ErlNifEnv*);
static ERL_NIF_TERM ok2(ErlNifEnv*, ERL_NIF_TERM);
static ERL_NIF_TERM error(ErlNifEnv*, ERL_NIF_TERM);

typedef char* TrieName;

// This is a map from names (from atom) to tries.
static hattrie_t* name_to_trie = NULL;

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
  name_to_trie = hattrie_create();
  // TODO initialize a global mapping from name to tries
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv) {
  // do nothing
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
  unsigned int len = 0;
  TrieName name = parse_arg_name(env, argc, argv, &len);
  if(NULL == name) {
    // failed to parse name arg
    return enif_make_badarg(env);
  }
  // TODO add lock
  // get inserts it if not found
  value_t* value_p = hattrie_get(name_to_trie, (char*)name, len);
  enif_free(name);
  if(0 != (*value_p)) {
    // alrady created
    return enif_make_badarg(env);
  }
  // create a new trie
  hattrie_t* new_trie = hattrie_create();
  (*value_p) = (value_t)(new_trie);
  return ok(env);
}

static ERL_NIF_TERM
destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  unsigned int len = 0;
  TrieName name = parse_arg_name(env, argc, argv, &len);
  if(NULL == name) {
    // failed to parse name arg
    return enif_make_badarg(env);
  }
  // TODO add lock
  value_t* value_p = hattrie_tryget(name_to_trie, (char*)name, len);
  if(NULL == value_p) {
    // not found
    enif_free(name);
    return enif_make_badarg(env);
  }
  hattrie_t* old_trie = (hattrie_t*)(*value_p);
  hattrie_free(old_trie);
  hattrie_del(name_to_trie, (char*)name, len);
  enif_free(name);
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
  return enif_make_int(env, (int)c);
}

static ERL_NIF_TERM
bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  hattrie_t* trie = find_by_name(env, argc, argv);
  if(NULL == trie) {
    // not found
    return enif_make_badarg(env);
  }
  size_t c = hattrie_sizeof(trie);
  return enif_make_int64(env, (long long)c);
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
  } else {
    ErlNifBinary* old_val= (ErlNifBinary*)(*value_p);
    result = enif_make_list(env, 1, enif_make_binary(env, old_val));
    // free only 'old_val', but not old_val->data
    // because old_val->data is copied to caller env
    // hence will be garbage collected.
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
  value_t* value_p = hattrie_tryget(trie, (char*)key.data, key.size);
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
  value_t* value_p = hattrie_tryget(trie, (char*)key.data, key.size);
  if(0 == value_p) {
    // no value
    return ok(env);
  }
  if(0 == (*value_p)) {
    // value deleted, TODO: check if this should happen if we never call hattrie_clear
    return ok(env);
  }
  ErlNifBinary* bin = (ErlNifBinary*)(*value_p);

  // hattrie_del may free seome tree nodes
  // but it does not take care of values.
  hattrie_del(trie, (char*)key.data, key.size);
  // release binary data
  enif_release_binary(bin);
  // and the binary struct itself
  enif_free(bin);

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
// it allocates memory for name, caller should call enif_free after usage.
static TrieName
parse_arg_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], unsigned int *len) {
  // ensure one arg (the name) passed in
  if(argc == 0) {
    return NULL;
  }
  // get lenght of the name
  *len = 0;
  ERL_NIF_TERM name_term = argv[0];
  if(0 == enif_get_atom_length(env, name_term, len, ERL_NIF_LATIN1)) {
    return NULL;
  }
  // get the name
  TrieName name = (TrieName)enif_alloc(*len + 1);
  if(0 == enif_get_atom(env, name_term, name, *len + 1, ERL_NIF_LATIN1)) {
    enif_free(name);
    return NULL;
  }
  return name;
}

// Parse trie name from arg[0], and find the trie for it,
// return NULL if faile to parse or not found.
static hattrie_t*
find_by_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  unsigned int len;
  TrieName name = parse_arg_name(env, argc, argv, &len);
  if(NULL == name) {
    return NULL;
  }
  value_t* value_p = hattrie_tryget(name_to_trie, (char*)name, len);
  enif_free(name);
  if(NULL == value_p) {
    // not found
    return NULL;
  }
  return (hattrie_t*)(*value_p);
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

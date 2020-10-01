#include <string.h>
#include "erl_nif.h"
#include "mytrie.h"
#include "mydict.h"
#include "varint.h"

static mytrie_t* find_by_name(ErlNifEnv*, int, const ERL_NIF_TERM*, ERL_NIF_TERM*);
static ERL_NIF_TERM ok(ErlNifEnv*);
static ERL_NIF_TERM ok2(ErlNifEnv*, ERL_NIF_TERM);
static ERL_NIF_TERM error(ErlNifEnv*, ERL_NIF_TERM);
static ERL_NIF_TERM exception(ErlNifEnv*, const char*);

typedef char* TrieName;
static TrieName parse_arg_name(ErlNifEnv*, int, const ERL_NIF_TERM*, unsigned int*);
static char* parse_arg_seg(ErlNifEnv*, int, const ERL_NIF_TERM*, unsigned int*, ERL_NIF_TERM*);

// This is a map from names (from atom) to tries.
static mytrie_t* name_to_trie = NULL;

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
  name_to_trie = mytrie_create("hattrie-names");
  mydict_init();
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv) {
  // TODO iterate over trie names, destroy all tries
  // TODO destroy mydict
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
    return exception(env, "bad_trie_name");
  }
  // create a new trie
  mytrie_t* new_trie = mytrie_create(name);
  bool is_put = mytrie_tryput(name_to_trie, name, len, (value_t)new_trie);
  enif_free(name);
  if(is_put) {
    return ok(env);
  }
  // failed to put
  mytrie_free(new_trie);
  return exception(env, "already_created");
}

static ERL_NIF_TERM
destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  unsigned int len = 0;
  TrieName name = parse_arg_name(env, argc, argv, &len);
  if(NULL == name) {
    // failed to parse name arg
    return exception(env, "bad_trie_name");
  }
  value_t value_p = mytrie_lookup(name_to_trie, (char*)name, len);
  if(0 == value_p) {
    // not found
    enif_free(name);
    return exception(env, "no_such_trie");
  }
  mytrie_t* old_trie = (mytrie_t*)value_p;
  mytrie_free(old_trie);
  mytrie_delete(name_to_trie, (char*)name, len);
  enif_free(name);
  return ok(env);
}

static ERL_NIF_TERM
count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM excep;
  mytrie_t* trie = find_by_name(env, argc, argv, &excep);
  if(NULL == trie) {
    // not found
    return excep;
  }
  size_t c = mytrie_size(trie);
  return enif_make_int(env, (int)c);
}

static ERL_NIF_TERM
bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM excep;
  mytrie_t* trie = find_by_name(env, argc, argv, &excep);
  if(NULL == trie) {
    // not found
    return excep;
  }
  size_t c = mytrie_sizeof(trie);
  return enif_make_int64(env, (long long)c);
}

// Allocate a new binary, copy data data from the input,
// to detach this binary from caller env.
// Return NULL if failed to allocate.
// The allocated binary should be released with enif_release_binary.
static ErlNifBinary*
copy_binary(ErlNifBinary* input) {
  ErlNifBinary* copy = enif_alloc(sizeof(ErlNifBinary));
  if(NULL == copy) {
    return NULL;
  }
  if(0 == enif_alloc_binary(input->size, copy)) {
    enif_free(copy);
    return NULL;
  }
  memcpy(copy->data, input->data, input->size);
  return copy;
}


// Create or update a key.
// Return [] if key is not found, otherwise [OldValue].
static ERL_NIF_TERM
upsert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 3) {
    return enif_make_badarg(env);
  }
  ERL_NIF_TERM excep;
  mytrie_t* trie = find_by_name(env, argc, argv, &excep);
  if(NULL == trie) {
    // not found
    return excep;
  }
  // get key input
  ErlNifBinary key;
  if(0 == enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    return exception(env, "bad_key");
  }
  // get value input
  ErlNifBinary new_val;
  if(0 == enif_inspect_iolist_as_binary(env, argv[2], &new_val)) {
    return exception(env, "bad_val");
  }

  // make a copy of the input binary
  ErlNifBinary* new_val_copy_p = copy_binary(&new_val);
  if(NULL == new_val_copy_p) {
    // failed to allocate
    return enif_raise_exception(env, enif_make_atom(env, "enif_alloc_binary_failed"));
  }

  value_t old_value_p = mytrie_upsert(trie, (char*)key.data, key.size, (value_t)new_val_copy_p);
  ERL_NIF_TERM result;
  if(0 == old_value_p) {
    // no old value, reutrn empty list
    result = enif_make_list(env, 0);
  } else {
    ErlNifBinary* old_val = (ErlNifBinary*)old_value_p;
    // move the old value to caller env (released from nif)
    result = enif_make_list(env, 1, enif_make_binary(env, old_val));
    // free only 'old_val', but not old_val->data
    enif_free(old_val);
  }
  return result;
}

static ERL_NIF_TERM
lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 2) {
    return enif_make_badarg(env);
  }
  ERL_NIF_TERM excep;
  mytrie_t* trie = find_by_name(env, argc, argv, &excep);
  if(NULL == trie) {
    // not found
    return excep;
  }
  // get key input
  ErlNifBinary key;
  if(0 == enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    return exception(env, "bad_key");
  }
  value_t value_p = mytrie_lookup(trie, (char*)key.data, key.size);
  if(0 == value_p) {
    // no value
    return enif_make_list(env, 0);
  }
  // make a copy of the value for caller env attachment
  ErlNifBinary* bin = copy_binary((ErlNifBinary*)value_p);
  ERL_NIF_TERM res = enif_make_binary(env, bin);
  return enif_make_list(env, 1, res);
}

static ERL_NIF_TERM
delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 2) {
    return enif_make_badarg(env);
  }
  ERL_NIF_TERM excep;
  mytrie_t* trie = find_by_name(env, argc, argv, &excep);
  if(NULL == trie) {
    // trie name not found
    return excep;
  }
  // get key input
  ErlNifBinary key;
  if(0 == enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    return exception(env, "bad_key");
  }
  value_t old_value_p = mytrie_delete(trie, (char*)key.data, key.size);
  if(0 != old_value_p) {
    ErlNifBinary* bin = (ErlNifBinary*)old_value_p;
    // release binary data
    enif_release_binary(bin);
    // and the binary struct itself
    enif_free(bin);
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

static ERL_NIF_TERM
exception(ErlNifEnv* env, const char* reason) {
  return enif_raise_exception(env, enif_make_atom(env, reason));
}

// Parse char* string from atom arg[0].
// Return NULL if failed to parse.
// It allocates memory for name, caller should call enif_free after usage.
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

// Parse char* string from binary arg[0].
// Return NULL if failed to parse.
// No need for manual deallocation of the returned pointer.
static char*
parse_arg_seg(ErlNifEnv* env,
              int argc,
              const ERL_NIF_TERM argv[],
              unsigned int *len,
              ERL_NIF_TERM* excep) {
  // ensure one arg (the name) passed in
  if(argc == 0) {
    *excep = enif_make_badarg(env);
    return NULL;
  }
  // get key input
  ErlNifBinary word;
  if(0 == enif_inspect_iolist_as_binary(env, argv[0], &word)) {
    *excep = exception(env, "bad_word");
    return NULL;
  }
  return (char*)(word.data);
}

// Parse trie name from arg[0], and find the trie for it,
// return NULL if faile to parse or not found.
static mytrie_t*
find_by_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], ERL_NIF_TERM* excep) {
  unsigned int len;
  TrieName name = parse_arg_name(env, argc, argv, &len);
  if(NULL == name) {
    *excep = exception(env, "bad_trie_name");
    return NULL;
  }
  value_t value_p = mytrie_lookup(name_to_trie, (char*)name, len);
  enif_free(name);
  if(0 == value_p) {
    // not found
    *excep = exception(env, "no_such_trie");
    return NULL;
  }
  *excep = NULL;
  return (mytrie_t*)value_p;
}

static ERL_NIF_TERM
encode_index(ErlNifEnv* env, index_t index) {
  int varint_len = varint_encoding_length(index);
  ErlNifBinary* res = enif_alloc(sizeof(ErlNifBinary));
  if(NULL == res) {
    return enif_raise_exception(env, enif_make_atom(env, "oom"));
  }
  if(0 == enif_alloc_binary(varint_len, res)) {
    enif_free(res);
    return enif_raise_exception(env, enif_make_atom(env, "oom"));
  }
  varint_encode(index, res->data, res->size);
  return enif_make_binary(env, res);
}

static ERL_NIF_TERM
find_seg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  unsigned int len;
  ERL_NIF_TERM excep;
  char* word = parse_arg_seg(env, argc, argv, &len, &excep);
  if(NULL == word) {
    return excep;
  }
  index_t index = mydict_find(word);
  if(0 == index){
    return enif_make_atom(env, "undefined");
  }
  return encode_index(env, index);
}

static ERL_NIF_TERM
clear_segs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  mydict_free();
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
add_seg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  unsigned int len;
  ERL_NIF_TERM excep;
  char* word = parse_arg_seg(env, argc, argv, &len, &excep);
  if(NULL == word) {
    return excep;
  }
  index_t index = mydict_add(word);
  if(0 == index) {
    return enif_raise_exception(env, enif_make_atom(env, "bug"));
  }
  return encode_index(env, index);
}

static ERL_NIF_TERM
del_seg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  unsigned int len;
  ERL_NIF_TERM excep;
  char* word = parse_arg_seg(env, argc, argv, &len, &excep);
  if(NULL == word) {
    return excep;
  }
  mydict_del(word);
  return ok(env);
}

static ErlNifFunc nif_funcs[] = {
   {"create",  1, create},

   {"upsert",  3, upsert},
   {"lookup",  2, lookup},
   {"delete",  2, delete},

   {"count",   1, count},
   {"bytes",   1, bytes},

   {"destroy", 1, destroy},

   {"add_seg", 1, add_seg},
   {"del_seg", 1, del_seg},
   {"find_seg", 1, find_seg},
   {"clear_segs", 0, clear_segs}
};

ERL_NIF_INIT(hattrie, nif_funcs, &load, &reload, &upgrade, &unload);

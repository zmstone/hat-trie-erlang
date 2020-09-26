
#include <string.h>
#include "mytrie.h"
#include "misc.h"

#define LOCK_NAME_PREFIX "mytrie-rwlock-"
#define LOCK_NAME_PREFIX_LEN strlen(LOCK_NAME_PREFIX)

// mytrie ties together hattrie and enif RWLock
struct mytrie_t_ {
  hattrie_t* trie;
  ErlNifRWLock* rwlock;
};

mytrie_t*
mytrie_create (const TrieName name){
  int name_len = strlen(name);
  int lock_name_len = name_len + LOCK_NAME_PREFIX_LEN;
  char* lock_name = malloc_or_die(lock_name_len + 1);
  memset(lock_name, 0, lock_name_len + 1);
  strncpy(lock_name, LOCK_NAME_PREFIX, LOCK_NAME_PREFIX_LEN);
  strncpy(lock_name + LOCK_NAME_PREFIX_LEN, name, name_len);
  mytrie_t* trie_p = malloc_or_die(sizeof(mytrie_t));
  trie_p->trie = hattrie_create();
  trie_p->rwlock = enif_rwlock_create(name);
  // erts copies the name, so we should free here
  free(lock_name);
  return trie_p;
}

void
mytrie_free(mytrie_t* trie_p){
  hattrie_free(trie_p->trie);
  enif_rwlock_destroy(trie_p->rwlock);
  free(trie_p);
}

void
mytrie_clear(mytrie_t* trie_p) {
  enif_rwlock_rwlock(trie_p->rwlock);
  hattrie_clear(trie_p->trie);
  enif_rwlock_rwunlock(trie_p->rwlock);
}

size_t
mytrie_size(mytrie_t* trie_p) {
  enif_rwlock_rlock(trie_p->rwlock);
  hattrie_size(trie_p->trie);
  enif_rwlock_runlock(trie_p->rwlock);
}

size_t
mytrie_sizeof(mytrie_t* trie_p) {
  enif_rwlock_rlock(trie_p->rwlock);
  hattrie_sizeof(trie_p->trie);
  enif_rwlock_runlock(trie_p->rwlock);
}

bool
mytrie_tryput(mytrie_t* trie_p, const char* key, size_t len, value_t new_value) {
  enif_rwlock_rwlock(trie_p->rwlock);
  value_t* value_p = hattrie_get(trie_p->trie, key, len);
  value_t old_value = (*value_p);
  bool should_put = (0 == old_value);
  if (should_put) {
    *value_p = new_value;
  }
  enif_rwlock_rwunlock(trie_p->rwlock);
  return should_put;
}

value_t
mytrie_upsert(mytrie_t* trie_p, const char* key, size_t len, value_t new_value) {
  enif_rwlock_rwlock(trie_p->rwlock);
  value_t* value_p = hattrie_get(trie_p->trie, key, len);
  value_t old_value = (*value_p);
  *value_p = new_value;
  enif_rwlock_rwunlock(trie_p->rwlock);
  return old_value;
}

value_t
mytrie_lookup(mytrie_t* trie_p, const char* key, size_t len) {
  enif_rwlock_rlock(trie_p->rwlock);
  value_t* value_p = hattrie_tryget(trie_p->trie, key, len);
  value_t result = 0;
  if(NULL != value_p) {
    result = (*value_p);
  }
  enif_rwlock_runlock(trie_p->rwlock);
  return result;
}

value_t
mytrie_delete(mytrie_t* trie_p, const char* key, size_t len) {
  enif_rwlock_rwlock(trie_p->rwlock);
  value_t* value_p = hattrie_tryget(trie_p->trie, key, len);
  value_t result = 0;
  if(NULL != value_p) {
    result = (*value_p);
  }
  hattrie_del(trie_p->trie, key, len);
  enif_rwlock_rwunlock(trie_p->rwlock);
  return result;
}


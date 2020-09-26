#ifndef MYTRIE_H
#define MYTRIE_H

#include "erl_nif.h"
#include "hat-trie.h"

typedef char* TrieName;
typedef struct mytrie_t_ mytrie_t;

mytrie_t* mytrie_create (const TrieName); // Create an empty trie.
void      mytrie_free   (mytrie_t*); // Free all memory used by a trie.
void      mytrie_clear  (mytrie_t*); // Remove all entries.
size_t    mytrie_size   (mytrie_t*); // Number of stored keys.
size_t    mytrie_sizeof (mytrie_t*); // Memory used in structure in bytes.
bool      mytrie_tryput(mytrie_t*, const char*, size_t, value_t); // insert and return true if not found, return false otherwise
value_t   mytrie_upsert(mytrie_t*, const char*, size_t, value_t); // insert or update, reutrn old value (0 if not found)
value_t   mytrie_lookup(mytrie_t*, const char*, size_t); // lookup return 0 if not found
value_t   mytrie_delete(mytrie_t*, const char*, size_t); // return old value, 0 if not found

#endif

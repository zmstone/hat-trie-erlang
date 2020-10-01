/*******************************************************************************
 * AVL tree as dictionary. Words are reference-counted.
 ******************************************************************************/
#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"
#include "avlmini.h"
#include "misc.h"
#include "mydict.h"

#define MAX_WORD_LEN 65535

typedef struct avl_node avlnode_t;

typedef struct word_t_ {
  avlnode_t node;
  char* word;
  index_t index;
  int refs;
} word_t;

typedef struct avl_tree avltree_t;

typedef struct mydict_t_ {
  avltree_t avltree;
  index_t next_index;
  ErlNifRWLock* rwlock;
} mydict_t;

static mydict_t allwords;

int compare_words(const void*, const void*);
void destroy_node(void*);
void free_word(word_t*);
word_t* make_tmpnode(const char*);

int compare_words(const void* a, const void* b) {
   return strcmp(((word_t*)a)->word, ((word_t*)b)->word);
}

void free_word(word_t* node) {
  free(node->word);
  free(node);
}

void destroy_node(void* data) {
  free_word((word_t*)data);
}

word_t* make_tmpnode(const char* word) {
  int word_len = strlen(word);
  if(word_len <= 0 || word_len > MAX_WORD_LEN) {
    exit(EXIT_FAILURE);
  }
  char* copyword = malloc_or_die(word_len + 1);
  strncpy(copyword, word, word_len + 1);
  word_t* node = malloc_or_die(sizeof(word_t));
  node->word = copyword;
  node->index = allwords.next_index;
  node->refs = 1;
  return node;
}
void mydict_init() {
  avl_tree_init(&allwords.avltree,
      compare_words,
      sizeof(word_t),
      AVL_OFFSET(word_t, node));
  allwords.next_index = 1;
  allwords.rwlock = enif_rwlock_create("mydict-rwlock");
}

void mydict_free() {
  avl_tree_clear(&allwords.avltree, destroy_node);
  allwords.next_index = 1;
}

// add word to dictionary if not already added
// return the index of the word
index_t mydict_add(const char* word) {
  index_t result = 0;
  word_t* tmpnode = make_tmpnode(word);
  enif_rwlock_rwlock(allwords.rwlock);
  void* existing = avl_tree_add(&allwords.avltree, (void*)tmpnode);
  if(NULL == existing) {
    // added as a new node
    result = tmpnode->index;
    allwords.next_index++;
  } else {
    word_t* existing_word = (word_t*)existing;
    existing_word->refs++;
    result = existing_word->index;
    free_word(tmpnode);
  }
  enif_rwlock_rwunlock(allwords.rwlock);
  return result;
}

// de-reference a word from the dictionary
// nothing to return
void mydict_del(const char* word) {
  word_t* tmpnode = make_tmpnode(word);
  enif_rwlock_rwlock(allwords.rwlock);
  void* found = avl_tree_find(&allwords.avltree, (void*)tmpnode);
  free_word(tmpnode);
  if(NULL != found) {
    word_t* found_word = (word_t*)found;
    if(found_word->refs <= 1){
      avl_tree_remove(&allwords.avltree, found);
    } else {
      found_word->refs--;
    }
  }
  enif_rwlock_rwunlock(allwords.rwlock);
}

// find a word in the dictionary, return its 1-based index.
// return 0 if not found.
index_t mydict_find(const char* word) {
  word_t* tmpnode = make_tmpnode(word);
  enif_rwlock_rlock(allwords.rwlock);
  void* found = avl_tree_find(&allwords.avltree, (void*)tmpnode);
  index_t result;
  if(NULL == found) {
    result = 0;
  } else {
    word_t* found_word = (word_t*)found;
    result = found_word->index;
  }
  enif_rwlock_runlock(allwords.rwlock);
  return result;
}

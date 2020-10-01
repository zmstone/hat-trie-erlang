# hattrie

Erlang binding (nif) for [hat-trie](https://github.com/dcjones/hat-trie)

## Build & test

```
$ make
```

## Implementation

### Plain trie

Trie tress are named and can be shared between processes.
A rw-lock is used to ensure thread-safe.

Just because trie is right here, the mapping from name to a trie is implemented
using a global trie. Meaning each API call will perform a name lookup to find
a trie for read or write.

### Segment trie

Segment trie is an optimization for prefixing of words instead of characters.
e.g. `word1` is a prefix of `word1/word2/word3`, but `word` is not.

Internally, the keys are split into words (segments), then stored in an
[AVL](https://github.com/skywind3000/avlmini) tree
as a dictionary, each word gets a one-based index assigned by the AVL tree
manager. Then the index numbers are
[varint](https://github.com/sorribas/varint.c) encoded, concatenated,
and finally stored in a plain trie.

## Performance

Since I am only interested in segment matches, e.g. `word1/word2/...`,
in comparism to ets, all the segmented prefixes are inserted as ETS key.

Observations so far:
* Plain trie can be quite bloated in memory
* Segment trie is much more compact, and takes less RAM than ets
    -- excluding the AVL dict though. (TODO: count bytes for AVL tree)
* Both plain and segment trie are outperforming ets in concurrent prefix
  enumeration insert + lookup tests.

See or run `hattrie_ets_compare_tests` for more information.

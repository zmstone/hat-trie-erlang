# hattrie

Erlang binding (nif) for [hat-trie](https://github.com/dcjones/hat-trie)

## Build & test

```
$ make
```

## Implementation

Trie tress are named and can be shared between processes.
A rw-lock is used to ensure thread-safe.

Just because trie is right here, the mapping from name to a trie is implemented
using a global trie. Meaning each API call will perform a name lookup to find
a trie for read or write.

## Performance

Since I am only interested in segment matches, e.g. `segment1/segment2/...`,
in comparism to ets, all the segmented prefixes are inserted as ETS key.

Observation so far:

* `hat-trie` is quite bloated in memory
* Prefix enumeration lookup performance is much higher than ets

See or run `hatrie_ets_compare_tests` for more information.

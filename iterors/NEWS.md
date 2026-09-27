# iterors 1.0.1

* Fix links in documentation
* For consistency, all iterator-constructing functions are spelled beginning with `i_`. The previous spellings beginning with `i` are retained as aliases.

# iterors  1.0

Initial release.

  * Split off from "async" package.
  * Incorporated/ported "iterators" package.
  * Incorporated/ported "itertools" package.
  * Incorporated/ported "itertools2" package.

New functions:

  * `i_enumerate.array` can iterate over arbitrary margins, providing you
    with a vector index.
  * `i_window` constructs a sliding window of arbitrary length over a given
    iterator, generalizing `itertools2::ipairwise` and `itripletwise`.
  * New functions `i_rle()` and `i_rle_inverse()` for run-length encoding.
  * Introduced accumulation methods `i_accum()`, `reduce()`, `sum()`
    and `prod()`.
  * `i_chain(...)` has a companion function `i_concat(it)` which accepts
    an iterable (rather than `...`).
  * New iteror methods for `as.numeric`, `as.vector`, `as.character`,
    and `as.logical`.
  * `concat()` pastes chunks from an iterator into a vector.
  
New features:

  * Memory-backed and counting iterors `icount`, `icountn`, `idiv`,
    `igrid`, `iseq`, `i_enumerate.default`, `iteror.default`,
    `iteror.data.frame`, `iteror.default` and `i_enumerate.array`
    all have shared logic; all accept options `chunks`, `chunksize`
    and `recycle` with equivalent behavior.
  * `icount` and `icountn` preserve dimnames.
  * Multidimensional iterators `icountn`, `igrid`, `i_enumerate.array` and
    `iteror.array` have option `rowMajor` to control the order of iteration.
  * `i_unique` uses a hash table rather than linear scan, for much improved
    performance; it also now works with any type of R object.
  * Random number iterators like `irunif`, `isample` and friends
    accept options `independent`,  and `seed`  if given, the iterator will maintain a
    private seed value, so that interleaving with other iterators does
    not affect reproducibility. You can use a specific random number
    generator algorithm by also giving `kind`, `normal.kind`, and `sample.kind`.
  * `i_tee` works for any iterator, using a queue, where previously
    `itertools::i_tee` only worked for memory-backed iterators.
  * Python compatibility: `py_iteror` wraps an iterator so that it can
    be used by Python code via package `reticulate`. Meanwhile
    `iteror` has a method for Python objects, allowing Python
    iterators to be used transparently with iteror code.

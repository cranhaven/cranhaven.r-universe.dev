# keyToEnglish 0.2.1

# Minor Changes

* Added `reconciliation.R` and related functions to convert keys/strings generated from 0.2.0 to this version (and other versions for possible future fixes)
* Changed `leoprine` to `leporine` in word list files and data. First string was typo, second string means "of or relating to rabbits".

# keyToEnglish 0.2.0

## Major changes

* `keyToEnglish()` argument `word_list` can now be a list of word lists, each representing a position in the phrases built. This can be used to construct grammatically-correct phrases
* Included several curated word lists for more sensical and useful patterns


## Minor changes

* Included warnings when hash function cannot cover all combinations of input
* Added function `hash_to_sentence()` that produces (more or less) grammatically correct phrases with high entropy (54 bits)
* Added function `generate_random_sentences()` to generate random sentences, either with `openssl`'s random number generator or R's
* Added two functions to assist with hash collision probability calculations
* Added a function to randomly generate sentences as keys
* Added least-common-multiple and greatest-common-denominator functions

## Bugfixes

* Fixed indexing bug in `keyToEnglish()` where an index of 0 would produce an `NA` value. This also changes the values in the tests and the function will not produce the same values as before.

# keyToEnglish 0.1.0

## Major changes

 * Initial release
 * Added `keyToEnglish()` function, the main feature of the package
 * Added `corpus_to_word_list()` function to help users build word lists
 * Added 5 example/default word lists
 

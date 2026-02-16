# SafeMapper 1.0.0

Initial CRAN release.

## Features

* Drop-in replacements for `purrr` mapping functions: `s_map()`, `s_map_chr()`, 
  `s_map_dbl()`, `s_map_int()`, `s_map_lgl()`, `s_map_dfr()`, `s_map_dfc()`,
  `s_map2()` and variants, `s_pmap()`, `s_walk()`, `s_walk2()`, `s_imap()`.

* Drop-in replacements for `furrr` parallel functions: `s_future_map()` and 
  variants, `s_future_map2()` and variants, `s_future_pmap()`, `s_future_walk()`,
  `s_future_walk2()`, `s_future_imap()`.

* Error handling wrappers: `s_safely()`, `s_possibly()`, `s_quietly()`.

* Automatic checkpointing based on input data fingerprinting.

* Seamless recovery from interruptions without manual session management.

* Configurable batch size and retry attempts via `s_configure()`.

* Session cleanup via `s_clean_sessions()`.

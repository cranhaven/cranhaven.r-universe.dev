# rladybugdb 0.4.4

## CRAN documentation

* Added return-value documentation for interval formatting and all documented
  DBI methods, including output classes, contents, and side effects. Clarified
  return values for result streaming, resource management, and conversions.
* Unwrapped short in-memory examples, guarded the graph example with
  `requireNamespace()`, and added explicit result and connection cleanup.
* Removed the deprecated installation-helper example and installation commands
  from optional-dependency error messages. Functions, examples, and vignettes
  do not install packages or external software.

## CRAN build diagnostics

* The configure script no longer forwards R's diagnostic flags (`-Wall`,
  `-pedantic`, and related) to the bundled LadybugDB build. The vendored
  third-party sources use documented compiler extensions that produced
  significant-warning output on Debian Clang. All other configured flags,
  including optimisation, debugging, architecture, hardening, and sanitizer
  settings, are still passed through, and this package's own sources continue
  to be compiled with R's full flags.

# rladybugdb 0.4.3

## CRAN Linux atomic runtime

* Added a configure-time C++20 link test for LadybugDB's 16-byte atomic
  operations. The package now links `libatomic` only on toolchains that require
  it, fixing package loading with Debian Clang 23 while remaining portable to
  platforms that provide the operations natively.

# rladybugdb 0.4.2

## CRAN compiler portability

* Added explicit fixed-width integer declarations for bundled Thrift so the
  source builds with the stricter Clang 23 headers used by Debian R-devel.
* Fixed bundled CRoaring inline linkage for Rtools/MinGW and restricted the
  MSVC structured-exception translator to MSVC builds.
* Rtools builds now use LadybugDB's malloc-backed buffer manager, avoiding
  reliance on MSVC structured exception handling under MinGW.

# rladybugdb 0.4.1

## CRAN portability

* Replaced downloaded precompiled libraries with a pinned, checksum-verified
  LadybugDB source archive that is compiled by R's C++20 toolchain.
* Added a small documented upstream portability patch so linked code does not
  call process-terminating functions, system RNGs, or write directly to
  stdout/stderr. Remote extension installation is disabled in this build,
  removing the native OpenSSL runtime dependency.

# rladybugdb 0.4.0

## Engine and native safety

* Updated the embedded engine from LadybugDB 0.15.2 to 0.20.4. Platform
  artifacts and matching headers are selected for macOS, Linux, and Windows
  and verified against pinned SHA-256 checksums.
* Linked the upstream static library on macOS and Linux, eliminating the local
  `@rpath/liblbug.dylib` development failure. Windows installs its matching
  runtime DLL through `install.libs.R`.
* Result, connection, and database ownership is now explicit. Closed-handle,
  live-child, finalizer, and double-close paths are checked natively.
* Fixed R-devel/Windows compilation by passing explicit `Rboolean` values to
  Arrow external-pointer finalizers.
* Fixed Windows loading by installing the checksum-verified OpenSSL runtime
  DLLs required by the upstream LadybugDB DLL.
* Added a persisted 0.15.2 compatibility fixture and migration guidance.

## Results, parameters, and types

* `as.data.frame()` is repeatable and `print()`/`format()` use a bounded,
  non-consuming preview.
* Added `lb_fetch()`, `lb_has_next()`, `lb_reset()`, `lb_result_info()`,
  `lb_query_summary()`, and `lb_next_result()` for streaming and multi-result
  queries.
* Named parameters now require unique, non-empty names. All typed R missing
  scalars bind as database `NULL`, and binding failures identify the parameter
  in a typed error.
* Added parameter support for dates, timestamps, `integer64`, raw bytes, lists,
  arrays, and structs.
* Added exact DECIMAL and INT128 output, configurable INT64/UINT64 handling,
  raw BLOB output, UTC timestamp handling, and structured INTERVAL, UNION, and
  recursive-path values.

## R integrations

* Added native Arrow C Data Interface export, chunked Arrow fetching, Arrow
  table registration, and `lb_copy_from_arrow()`. Data-frame loading prefers
  Arrow and retains CSV as an explicit fallback.
* Added a DBI driver with connection, query, binding, fetch, transaction,
  quoting, and table metadata methods.
* Added query timeout, interruption, transaction, scoped-connection, database
  configuration, and extension management helpers.
* Graph conversion now handles recursive paths, empty results, null-leading
  columns, multiple labels and relationship types, isolated nodes, and exact
  internal identifiers.

## Packaging

* Removed tracked compiler output and the old bundled dylib, regenerated Rcpp
  and R documentation, and excluded development-only assets from source builds.
* Added cross-platform R CMD check and native sanitizer CI jobs, installed
  examples, and extension and storage-migration vignettes.

# rladybugdb 0.2.0

* Replaced the original Python/reticulate backend with native Rcpp bindings to
  LadybugDB 0.15.2.

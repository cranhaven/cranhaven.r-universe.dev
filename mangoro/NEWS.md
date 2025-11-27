# mangoro 0.2.5

## CRAN Policy Compliance

- Replaced non-suppressible console output (`print()`/`cat()`) with `message()` where appropriate (notably `mangoro_go_build()`), so information.
- Updated `tools/generate_certs.R` to avoid writing into the package or user home by default: when no explicit `--dir` is provided it writes into a temporary directory and reports the chosen path via `message()`; the script still accepts an explicit `--dir` for persistent output.
- Added `inst/AUTHORS` and `inst/COPYRIGHTS`  so vendored components and their license files are easy to find; full license/NOTICE/AUTHORS files remain in the vendor directories.
- Added `LICENSE.note` summarizing the license types present in vendored code.
- Added `Copyright: See inst/AUTHORS` to `DESCRIPTION` to make copyright ownership explicit as requested by CRAN.

# mangoro 0.2.4

## DESCRIPTION File Updates

- Added single quotes around all software names in Title and Description fields ('R', 'Go', 'IPC', 'Nanomsg', 'nanonext', 'nanoarrow') to comply with CRAN requirements

# mangoro 0.2.3

## CRAN Policy Compliance

- `mangoro_go_build()` now sets `GOCACHE` to a temporary directory by default to prevent populating `~/.cache/go-build` during package checks, complying with CRAN policy
- Added `gocache` parameter to `mangoro_go_build()` for users who want to specify a custom cache location or use the default Go cache (`gocache = NA`)
- Maintains backward compatibility while ensuring CRAN compliance



# mangoro 0.2.2

## CRAN Packaging Improvements

- Fixed long path warnings by relocating flatbuf files during build process
- Flatbuf files now stored in `tools/flatbuf/` and restored during package installation via configure script
- Configure scripts updated to be POSIX-compliant (replaced bashisms with standard sh syntax)
- Moved `processx` from Imports to Suggests (only used in tests)
- Package now passes `R CMD check --as-cran` with no warnings

## Internal Changes

- Updated `tools/vendorMangos.sh` to move flatbuf files to tools directory after vendoring
- Enhanced configure/configure.win scripts to restore flatbuf files during installation
- Excluded `inst/go/vendor/.../flatbuf` directory from package tarball via .Rbuildignore

# mangoro 0.2.1

## RPC Interface

- New `rgoipc` Go package for type-safe function registration with Arrow schema validation
- RPC protocol wrapping Arrow IPC data with function call envelope
- RPC helper functions: `mangoro_rpc_get_manifest()`, `mangoro_rpc_call()`, `mangoro_rpc_send()`, `mangoro_rpc_recv()`, `mangoro_rpc_parse_response()`
- RPC example server demonstrating function registration (add, echoString functions)
- HTTP file server with RPC control interface (start/stop/status commands)
- Helper functions for HTTP server control: `mangoro_http_start()`, `mangoro_http_stop()`, `mangoro_http_status()`

## Examples

- Complete RPC function registration and calling example in README
- HTTP server RPC control demonstration with server output capture
- Arrow IPC-based RPC communication examples

# mangoro 0.2.0

## Initial Release

- R/Go IPC using Nanomsg Next Gen (mangos v3, nanonext)
- Vendored Go dependencies for reproducible builds
- Helper functions to build and run Go binaries from R
- Example echo server and on-the-fly Go compilation from R
- Platform-correct IPC path helpers
- Designed for extensibility and cross-platform use
- We do not cgo's c-shared mode to avoid loading multiple Go runtimes in the same R session

## Arrow Go IPC Support

- Add Arrow Go IPC roundtrip example and support: send and receive Arrow IPC streams between R and Go using nanoarrow and arrow-go.
- New function: `get_arrow_go_version()` to report the vendored Arrow Go version.
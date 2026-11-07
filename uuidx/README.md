# uuidx

`uuidx` is an R package for generating and working with modern UUIDs using a
Rust backend. It focuses on a small, practical API and uses the Rust
[`uuid`](https://docs.rs/uuid/latest/uuid/) crate through
[`rextendr`](https://extendr.rs/rextendr/) and
[`extendr`](https://extendr.rs/).

The package is developed by Thomas Bryce Kelly and published as part of
[Icy Seas Co-Laboratory](https://icyseascolab.io), an applied research and
engineering institute focused on practical computational tools, data systems, and
scientific software and based in Fairbanks, AK.

## About Icy Seas Co-Laboratory

[Icy Seas Co-Laboratory](https://icyseascolab.io) is the home organization
behind `uuidx`. If you are using this package in research, engineering, or
production workflows and want more context on the broader work around it, the
project site is the best place to start.

## Why Rust?

- The Rust `uuid` crate gives us current RFC 9562 behavior without relying on a
  system package such as `libuuid`.
- Parsing, validation, version detection, and byte conversion stay in one
  well-tested backend.
- The R layer can stay compact while the low-level work happens
  in a fast, memory-safe implementation.

## Why version 7 by default?

Version 7 UUIDs combine time-ordering with strong uniqueness properties and are
generally a practical default for new application identifiers. They sort more
naturally than version 4 identifiers. Users should familiarize themselves with the structure and trade-offs associated with each version (e.g., [Wikipedia](https://en.wikipedia.org/wiki/Universally_unique_identifier#Format)). 

## Development setup

End users installing CRAN binaries on macOS or Windows typically do not need a
local Rust toolchain. Rust is mainly needed for source installs and package
development.

Install Rust first if it is not already available:

- https://www.rust-lang.org/tools/install

Then install the R-side development dependencies:

```r
install.packages(c("rextendr", "testthat"), repos = "https://cloud.r-project.org")
```

From the package root:

```sh
Rscript -e 'rextendr::document()'
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --as-cran uuidx_*.tar.gz
```

For quick local iteration:

```r
rextendr::document()
testthat::test_local()
```

You can also use the included `Makefile`:

```sh
make deps
make document
make test
make build
make check
```

## Examples

### Generate version 7 UUIDs

```r
uuid_v7()
uuid_v7(3)
uuid_generate()
```

### Generate version 4 UUIDs

```r
uuid_v4()
uuid_generate(5, version = "v4")
```

### Generate deterministic version 5 UUIDs

```r
uuid_v5("dns", "example.com")
uuid_v5("dns", c("alpha", "beta"))
uuid_generate(version = "v5", namespace = "dns", name = "example.com")
```

### Validate UUIDs

```r
x = c(uuid_v7(), "not-a-uuid")
uuid_validate(x)
uuid_version(x)
```

### Parse to raw bytes

```r
x = uuid_v7(2)
uuid_parse(x, output = "raw")
uuid_parse(x, output = "fields")
uuid_nil(output = "raw")
```

## Notes

- Version 7 is the default generator.
- Versions 4, 5, 6, and legacy version 1 are also supported.
- Functions return character vectors by default.
- Raw output is available where byte-level access is useful.

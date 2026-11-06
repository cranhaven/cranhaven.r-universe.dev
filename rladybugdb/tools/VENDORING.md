# Vendoring LadybugDB

The CRAN source package includes a compressed LadybugDB source tree rather
than downloading platform binaries during installation. The source is the
`lbug` crate published by the LadybugDB project at crates.io, pinned by version
and SHA-256 in `tools/lbug_source_checksums`.

`tools/vendor.R` performs the maintainer-only refresh:

1. Download and verify the pinned upstream crate.
2. Extract its complete `lbug-src` directory.
3. Apply `tools/patches/ladybugdb-cran.patch`.
4. Create `tools/vendor/ladybugdb-<version>.tar.xz` and update its checksum.

The patch adds a dedicated `LBUG_R_PACKAGE` CMake mode. It disables native
remote-extension downloads and their OpenSSL dependency, uses already
generated WAL sources so Python is not needed, supports the Rtools MinGW/LLVM
archivers, and removes code paths that terminate the host R process or write
directly to stdout/stderr. Third-party debug printers are compiled as no-ops.

LadybugDB and all bundled third-party license files remain inside the source
archive. Run `Rscript tools/vendor.R` only when intentionally updating the
pinned engine or the portability patch, then review and test the resulting
archive before committing it.

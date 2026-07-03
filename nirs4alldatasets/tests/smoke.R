# SPDX-License-Identifier: MIT
# Smoke test for the R binding (offline: resolve / verify / abi). Run after installing
# the package (see bindings/r/build_and_test.sh).
library(nirs4alldatasets)

index <- paste0(
  '{"schema":"1.0","n_datasets":1,"datasets":{"demo":{',
  '"tier":"public",',
  '"dataverse":{"instance":"https://dv.example","doi":"10.70112/ABC","dataset_version":"1.0"},',
  '"files":[{"name":"X.parquet","relpath":"canonical/sources/X.parquet","directory_label":"canonical/sources","sha256":"aa","size":9,"file_id":42}],',
  '"origins":[{"kind":"zenodo","mode":"canonical","locator":"10.5281/zenodo.5","access":"open"}],',
  '"descriptor":{"id":"demo"}}}}'
)

# resolve -> a contract that mentions the tier + file id; trailing newline-free JSON.
contract <- n4ds_resolve(index, "demo")
stopifnot(is.character(contract), grepl('"tier"', contract, fixed = TRUE), grepl('"file_id":42', contract, fixed = TRUE))

# an unknown id is an error.
err <- tryCatch({ n4ds_resolve(index, "nope"); FALSE }, error = function(e) TRUE)
stopifnot(err)

# verify_cached on an empty dir reports the file missing.
report <- n4ds_verify_cached(contract, tempdir())
stopifnot(grepl('"missing"', report, fixed = TRUE))

# ABI version looks like semver.
stopifnot(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+", n4ds_abi_version()))

cat("R binding smoke OK\n")

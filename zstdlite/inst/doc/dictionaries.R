## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(zstdlite)
library(bench)

## -----------------------------------------------------------------------------
set.seed(2024)
countries <- rownames(LifeCycleSavings)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create 'test' and 'train' datasets
# In this example consider the case of having a named vector of rankings of 
# countries.  Each ranking will be compressed separately and stored (say in a database)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_samples <- lapply(
  1:1000, 
  \(x) setNames(sample(length(countries)), countries)
)

test_samples <- lapply(
  1:1000, 
  \(x) setNames(sample(length(countries)), countries)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a dictionary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dict <- zstd_train_dict_serialize(train_samples, size = 5000, optim = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup Compression/Decompression contexts to use this dictionary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cctx_nodict <- zstd_cctx(level = 3) # No dictionary. For comparison
cctx_dict   <- zstd_cctx(level = 3, dict = dict)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When using the dictionary, what is the size of the compressed data compared
# to not using a dicionary here?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s1 <- lapply(test_samples, \(x) zstd_serialize(x, cctx = cctx_nodict)) |> lengths() |> sum()
s2 <- lapply(test_samples, \(x) zstd_serialize(x, cctx = cctx_dict  )) |> lengths() |> sum()
cat(round(s2/s1 * 100, 1), "%")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simple benchmark to test speed when using dicionary.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bench::mark(
  "No Dict" = lapply(test_samples, \(x) zstd_serialize(x, cctx = cctx_nodict)),
  "Dict"    = lapply(test_samples, \(x) zstd_serialize(x, cctx = cctx_dict  )),
  check = FALSE
)[, 1:5]


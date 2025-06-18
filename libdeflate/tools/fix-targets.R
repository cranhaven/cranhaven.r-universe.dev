#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
fn <- args[[1]]
txt <- readLines(fn)
txt <- sub(
  ".*INTERFACE_INCLUDE_DIRECTORIES.*",
  '  INTERFACE_INCLUDE_DIRECTORIES "${_IMPORT_PREFIX}/include"',
  txt
)
writeLines(txt, fn)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

for (fn in args) {
  txt = readLines(fn, warn = FALSE) # breaks on any of \r, \n
  con = file(fn, open = "wb", encoding = "UTF-8") # binary write
  on.exit(close(con), add = TRUE)
  writeLines(txt, con, sep = "\n", useBytes = TRUE) # write plain LF
}

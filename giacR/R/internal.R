subtraction <- function(equality) {
  members <- trimws(strsplit(equality, "=")[[1L]])
  paste0(members[1L], " - (", members[2L], ")")
}

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

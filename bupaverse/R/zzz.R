# From https://github.com/tidyverse/tidyverse/blob/main/R/zzz.R

.onAttach <- function(...) {

  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  msg(bupaverse_logo(), startup = TRUE)
  bupaverse_attach()

  if (!"package:conflicted" %in% search()) {
    x <- bupaverse_conflicts()
    msg(bupaverse_conflict_message(x), startup = TRUE)
  }

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
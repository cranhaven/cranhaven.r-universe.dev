# From https://github.com/tidyverse/tidyverse/blob/main/R/conflicts.R
#' @title Conflicts between the bupaverse and other packages
#'
#' @description
#' This function lists all the conflicts between packages in the [`bupaverse`] and other packages that you have loaded.
#'
#' @returns
#' Returns a [`list`] all the conflicts between packages in the [`bupaverse`] and other packages that you have loaded.
#'
#' @examples
#' bupaverse_conflicts()
#'
#' @export
bupaverse_conflicts <- function() {

  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", bupaverse_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "bupaverse_conflicts")
}

bupaverse_conflict_message <- function(x) {

  # No conflicts
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "bupaverse_conflicts()"
  )

  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(cli::col_blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(cli::col_blue(winner), "::", cli::col_green(paste0(names(x), "()"))))
  bullets <- paste0(
    cli::col_red(cli::symbol$cross), " ", funs,
    " masks ", other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

#' @export
print.bupaverse_conflicts <- function(x, ..., startup = FALSE) {

  cli::cat_line(bupaverse_conflict_message(x))
  invisible(x)
}

confirm_conflict <- function(packages, name) {

  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}

ls_env <- function(env) {

  x <- ls(pos = env)

  # intersect, setdiff, setequal, union come from generics
  # if (env %in% c("package:dplyr", "package:lubridate")) {
  #   x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  # }
  #
  # if (env == "package:lubridate") {
  #   x <- setdiff(x, c(
  #     "as.difftime", # lubridate makes into an S4 generic
  #     "date"         # matches base behaviour
  #   ))
  # }

  x
}
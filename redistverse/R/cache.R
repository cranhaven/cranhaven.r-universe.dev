#" Summarize caches for `redistverse` packages
#"
#" @param packages character vector of package names. Default is `c("alarmdata", "tinytiger")`.
#"
#" @return a vector of paths to the cache directories for the specified packages
#" @export
#"
#" @examples
#" redistverse_cache_status()
redistverse_cache_status <- function(packages = c("alarmdata", "tinytiger")) {
  if ("alarmdata" %in% packages) {
    packages[packages == "alarmdata"] <- "alarm"
  }

  check_fns <- c(
    tinytiger = tinytiger::tt_cache_path,
    alarm = alarmdata::alarm_cache_path
  )

  use_cache <- logical(length(packages))
  paths <- character(length(packages))
  for (i in seq_along(packages)) {
    use_cache[i] <- getOption(x = paste0(packages[i], ".use_cache"), default = FALSE)
    if (use_cache[i] && !is.null(check_fns[[packages[i]]])) {
      paths[i] <- check_fns[[packages[i]]]()
    } else {
      paths[i] <- getOption(paste0(packages[i], ".cache_dir"), default = NA_character_)
    }
  }

  ul <- cli::cli_ul()
  for (i in seq_along(packages)) {
    if (is.na(paths[i])) {
      if (isFALSE(use_cache[i])) {
        cli::cli_li("{.pkg {packages[i]}}: cache is not enabled.")
      } else {
        cli::cli_li("{.pkg {packages[i]}}: cache enabled but not found.")
      }
    } else {
      cli::cli_li("{.pkg {packages[i]}}: cache is {.path {paths[i]}}.")
    }
  }
  cli::cli_end(ul)

  names(paths) <- packages

  invisible(paths)
}

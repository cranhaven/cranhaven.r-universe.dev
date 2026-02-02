# From https://github.com/tidyverse/tidyverse/blob/main/R/attach.R

# The core packages of bupaverse
core <- c("bupaR", "edeaR", "eventdataR", "processcheckR", "processmapR")

# Get core packages that are not loaded yet.
core_unloaded <- function() {

  search <- paste0("package:", core)
  core[!search %in% search()]
}

# Attach the package from the same package library it was loaded from before.
same_library <- function(pkg) {

  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))

  do.call(
    what = "library",
    list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
}

bupaverse_attach <- function() {

  to_load <- core_unloaded()

  if (length(to_load) == 0)
    return(invisible())

  msg(
    cli::rule(
      left = cli::style_bold("Attaching packages"),
      right = glue::glue("bupaverse {package_version('bupaverse')}")
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1L))
  packages <- paste0(
    cli::col_green(cli::symbol$tick), " ", cli::col_blue(format(to_load)), " ",
    cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }

  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible()
}

package_version <- function(x) {

  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- cli::col_red(as.character(version[4:length(version)]))
  }

  paste0(version, collapse = ".")
}
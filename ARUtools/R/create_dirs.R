#' Create directory structure for recording folders
#'
#' Create a set of nested folders for storing ARU recordings by plots and sites.
#'
#' @param plots Character vector. Hexagon or cluster names for folder names.
#' @param site_ids  Character vector. Site IDs. Should include the plot/cluster
#'   id in the name.
#' @param base_dir Character. Base directory to build directory structure in.
#' @param dir_list Logical. Whether to return a vector of directories (to be)
#'   created (defaults to `FALSE`).
#' @param dry_run Logical. Whether to do a dry-run of the process (i.e. do not
#'   actually create directories; defaults to `TRUE`)
#' @param expect_dirs Logical. Expect that directories may already exist? Default
#'   (`FALSE`) is to stop if directories to be created already exist.
#'
#' @return If `dir_list = TRUE`, returns a list of directories (to be) created.
#'   If not a dry run, also creates the folder structure.
#' @export
#'
#' @examples
#' # Default is to do a dry-run (don't actually create the directories)
#' create_dirs(
#'   plots = c("river1", "river2", "river3"),
#'   site_ids = c(
#'     "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
#'     "river3_sm05", "river3_sm06"
#'   ),
#'   base_dir = "Recordings"
#' )
#'
#' # Get a list of directories which would be created
#' create_dirs(
#'   plots = c("river1", "river2", "river3"),
#'   site_ids = c(
#'     "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
#'     "river3_sm05", "river3_sm06"
#'   ),
#'   base_dir = "Recordings", dir_list = TRUE
#' )
#'
#' @examplesIf dir.exists("Recordings")
#' # Create directories AND return a list of those created
#' d <- create_dirs(
#'   plots = c("river1", "river2", "river3"),
#'   site_ids = c(
#'     "river1_sm01", "river1_sm02", "river2_sm03", "river2_sm04",
#'     "river3_sm05", "river3_sm06"
#'   ),
#'   base_dir = "Recordings", dir_list = TRUE, expect_dirs =TRUE,
#'   dry_run = FALSE
#' )
#' d
create_dirs <- function(plots, site_ids, base_dir = NULL, dir_list = FALSE,
                        dry_run = TRUE, expect_dirs = FALSE) {
  # Get absolute path so user can be *really* sure they want to do this
  if (is.null(base_dir)) base_dir <- fs::path_wd() else base_dir <- fs::path_abs(base_dir)

  # Calculate directories
  d <- vector()
  for (p in plots) d <- c(d, fs::path(base_dir, p, stringr::str_subset(site_ids, p)))

  # Create directories
  if (!dry_run) {
    if (!expect_dirs & any(fs::dir_exists(d))) {
      abort(
        c("Trying to create directories that already exist",
          "i" = "If you're certain this is correct, use `expect_dirs = TRUE`"
        ),
        call = caller_env()
      )
    }

    fs::dir_create(d)

    readr::write_lines(utils::sessionInfo(), glue::glue("{base_dir}/session_info.md"))
  } else {
    msg <- "This is a dry run, no directories are created"
    if (!dir_list) msg <- c(msg, "i" = "Use `dir_list = TRUE` to return a list of directories to be created")
    if (!expect_dirs & any(fs::dir_exists(d))) {
      msg <- c(msg,
        "i" = "This will create directories that already exist",
        "*" = "If you're certain this is correct, use `expect_dirs = TRUE`"
      )
    }

    inform(msg)
  }
  if (dir_list) d
}

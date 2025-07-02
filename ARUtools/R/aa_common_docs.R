# common_docs ------------------
#' Common arguments and documentation for various functions
#'
#' @param project_dir Character. Directory where project files are stored. File
#'   paths will be used to extract information and must actually exist.
#' @param project_files Character. Vector of project file paths. These paths can
#'   be absolute or relative to the working directory, and don't actually need
#'   to point to existing files unless you plan to use `clean_gps()` or other
#'   sampling steps down the line. Must be provided if `project_dir` is `NULL`.
#' @param subset Character. Text pattern to mark a subset of files/directories
#'   to either `"keep"` or `"omit"` (see `subset_type`)
#' @param subset_type Character. Either `keep` (default) or `omit`
#'   files/directories which match the pattern in `subset`.
#' @param meta Data frame. Recording metadata. Output of `clean_metadata()`.
#' @param meta_sites (Spatial) Data frame. Recording metadata with added
#'   coordinates. Output of `clean_metadata()` and then `add_sites()` (with
#'   either `clean_gps()` or `clean_site_index()`).
#' @param col_site_id Column. Unquoted column containing site strata IDs
#'   (defaults to `site_id`).
#' @param date Logical. Whether to summarize output by date (as well as
#'   `site_id` and `aru_id`. Default `FALSE`.
#' @param path Character. Path to wave file.
#' @param dir_out Character. Output directory.
#' @param quiet Logical. Whether to suppress progress messages and other
#'   non-essential updates.
#'
#' @details
#' Use `@inheritParams common_docs` to include the above in any function
#' documentation with a matching argument (will only include matching args)
#'
#' @keywords internal
#' @name common_docs
NULL

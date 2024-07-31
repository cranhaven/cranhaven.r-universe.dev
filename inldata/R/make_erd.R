#' Create Entity Relationship Diagram
#'
#' @description Create an Entity-Relationship Diagram (ERD) from a data model and save it to disk in SVG format.
#'   Requires that the \pkg{dm}, \pkg{DiagrammeR}, \pkg{V8}, and \pkg{DiagrammeRsvg} packages are available.
#'
#' @param dm '[dm][dm::dm]' object.
#'   Data model.
#' @param path 'character' string.
#'   File path to write the ERD.
#' @param overwrite 'logical' flag.
#'   Whether to overwrite an existing file.
#'
#' @return Invisibly returns the file path.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @seealso [`make_dm`] function for creating a data model for the \pkg{inldata} package.

make_erd <- function(dm, path = tempfile(fileext = ".svg"), overwrite = FALSE) {

  # check packages
  for (pkg in c("dm", "DiagrammeR", "V8", "DiagrammeRsvg")) {
    check_package(pkg, msg = "Creating a entity relationship diagram")
  }

  # check arguments
  checkmate::assert_class(dm, classes = "dm")
  checkmate::assert_string(path, min.chars = 1)
  path <- path.expand(path) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  dirname(path) |>
    dir.create(showWarnings = FALSE, recursive = TRUE)
  checkmate::assert_flag(overwrite)
  checkmate::assert_path_for_output(path, overwrite = overwrite, extension = "svg")

  # draw diagram and write to disk
  dm::dm_draw(dm,
    view_type = "all",
    graph_name = "ERD",
    column_types = FALSE,
    font_size = c("header" = 12L, "column" = 12L)
  ) |>
    DiagrammeRsvg::export_svg() |>
    cat(file = path)

  invisible(path)
}

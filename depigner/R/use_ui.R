#' Use usethis'ui(s) in your package
#'
#' Does setup necessary to use the usethis' user interfaces in your package.
#' This function requires the use roxygen.
#' * Check that the active package uses roxygen2
#' * Adds usethis package to "Imports" in `DESCRIPTION`
#' * Imports in your namespace:
#'   - block styles: \code{\link{ui_line}}, \code{\link{ui_todo}}
#'       \code{\link{ui_done}}, \code{\link{ui_todo}}, \code{\link{ui_oops}}
#'       \code{\link{ui_info}}, \code{\link{ui_code_block}}
#'   - conditions: \code{\link{ui_stop}}, \code{\link{ui_warn}}
#'   - questions: \code{\link{ui_yeah}}, \code{\link{ui_nope}}
#'   - inline styles: \code{\link{ui_field}}, \code{\link{ui_value}}
#'       \code{\link{ui_path}}, \code{\link{ui_code}}) user interfaces
#'
#' @export
#'
#' @details Attribution: most of the source content of this function is
#'   taken and/or adapted from the corresponding unexported function in
#'   the `usethis` package.
#'
#' @examples
#' \dontrun{
#'   # while setup of a package
#'   use_ui()
#' }
use_ui <- function() {
  # check if package (adapted from usethis:::is_package())
  tryCatch({
    this_proj <- usethis::proj_get()
    rprojroot::find_package_root_file(path = this_proj)
  },
    error = function(e) {
      ui_stop(
        "{ui_code('use_ui')} is designed to work with packages.",
      )
    }
  )

  # check if package (adapted from usethis:::uses_roxygen())
  if (!desc::desc_has_fields("RoxygenNote", this_proj)) {
    ui_stop("
      {ui_code('use_ui')} can not find roxygen2 active.
      You might just need to run {ui_code('devtools::document()')} once,
      then try again.

    ")
  }

  # add usethis to Imports (adapted from usethis:::use_dependency())
  deps <- desc::desc_get_deps(this_proj)
  existing_dep  <- deps[["package"]] == "usethis"
  existing_type <- deps[["type"]][existing_dep]
  if (!any(existing_dep) || any(existing_type == "LinkingTo")) {
    ui_done("
      Adding {ui_value('usethis')} to {ui_field('Imports')} field
      in DESCRIPTION.
    ")
    desc::desc_set_dep("usethis", "Imports", file = this_proj)
  }

  existing_type <- setdiff(existing_type, "LinkingTo")
  types <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")

  if (length(existing_type) &&
    (match(existing_type, types) > match("Imports", types))
  ) {
    ui_done("
      Moving {ui_value('usethis)} from {ui_field(existing_type)}
      to {ui_field('Imports')} field in DESCRIPTION.
    ")
    desc::desc_del_dep("usethis", existing_type, file = this_proj)
    desc::desc_set_dep("usethis", "Imports", file = this_proj)
  }

  path <- fs::path("R", "utils-depigner", ext = "R")

  if (!fs::file_exists(path)) {
    usethis::use_template("utils-depigner.R", path, package = "depigner")
  }

  # Paste is needed because roxygen2 reads those lines as
  # roxygen-comments! This way they start with '"' and the problem is
  # avoided.
  tag <- paste(
    "#' @importFrom usethis ui_line ui_todo ui_done ui_todo ui_oops ui_info",
    "#' @importFrom usethis ui_code_block",
    "#' @importFrom usethis ui_stop ui_warn",
    "#' @importFrom usethis ui_yeah ui_nope",
    "#' @importFrom usethis ui_field ui_value ui_path ui_code",
    sep = "\n"
  )

  success <- block_append(
    tag,
    path = fs::path(usethis::proj_path(), path)
  )
  if (success) ui_todo("
    Run {ui_code('devtools::document()')} to update {ui_path('NAMESPACE')}
  ")

  success
}

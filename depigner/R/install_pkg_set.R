#' Check basic installed packages
#'
#' This function is used to politely ask the user to install requiring
#' packages. It is intended to be used in interactive sessions only.
#'
#' You can pass arbitrarily sets of packages; on the other hands, you
#' can use some sets already prepared and included into `{depigner}`
#' (see `?pkg_sets`).
#'
#' @note By default this function install all the packages listed in
#'   `pkg_all`, which is the union of all the sets of packages listed
#'   in `?pkg_sets`.
#'
#' @param set (chr) packages' names
#' @param dependencies do you want to install the dependencies?
#'
#' @importFrom utils data install.packages installed.packages menu
#' @importFrom utils update.packages
#'
#' @return invisible character vector of the subset of `interested`
#'   which was not already present, and installed.
#'
#' @seealso please_install, pkg_sets
#'
#' @export
#' @examples
#' \dontrun{
#'   install_pkg_set() # to install all the `?pkg_all`
#' }
install_pkg_set <- function(set = pkg_all, dependencies = TRUE) {

   are_missing <- purrr::map_lgl(set, ~{
     identical(find.package(.x, quiet = TRUE), character())
   })

  needed <- set[are_missing]

  if (length(needed) > 0L) {
    please_install(needed, dependencies = dependencies)
  }

  invisible(needed)
}

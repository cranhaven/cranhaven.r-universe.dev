#' Please install
#'
#' A polite helper for installing and update packages (quite exactly)
#' inspired from a function used by Hadley Wickham at `RStudio::conf
#' 2018 - San Diego`.
#'
#' @param pkgs character vector of package(s) to install
#' @param install_fun function to use for installing package(s)
#' @param ... further options for install_fun
#' @export
#'
#' @return invisible
please_install <- function(pkgs, install_fun = install.packages, ...) {
  if (!length(pkgs)) {
    return(invisible())
  }

  if (!interactive()) {
    ui_stop("Please run in interactive session")
  }

  q_pkg_title <- "Do you agree to install the following packges?"
  q_pkg_tale <- "(among the corresponding dependencies)"

  ko_pkg <- ui_nope(c(q_pkg_title, paste("* ", pkgs), q_pkg_tale))


  if (ko_pkg) {
    return(invisible())
  }
  install_fun(pkgs, ...)


  q_upd_title <- "Ok to check and update all your packages?"
  ko_upd <- ui_nope(q_upd_title)

  if (ko_upd) {
    return(invisible())
  }
  update.packages(ask = FALSE, checkBuilt = TRUE)

  invisible(pkgs)
}

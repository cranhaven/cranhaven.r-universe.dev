#' Check for presence of a package
#'
#' This function efficiently checks for the presence of a package
#' without loading it (unlike [library()] or [require()].
#' This is useful to force yourself to use the package::function
#' syntax for addressing functions; you can make sure required packages
#' are installed, but their namespace won't attach to the search path.
#'
#' @param ... A series of packages. If the packages are named, the
#' names are the package names, and the values are the minimum required
#' package versions (see the second example).
#' @param install Whether to install missing packages from `repos`.
#' @param load Whether to load packages (which is exactly *not* the point
#' of this package, but hey, YMMV).
#' @param repos  The repository to use if installing packages; default
#' is the RStudio repository.
#'
#' @return Invisibly, a vector of the available packages.
#' @export
#'
#' @examples \donttest{
#' ufs::checkPkgs('base');
#'
#' ### Require a specific version
#' ufs::checkPkgs(ufs = "0.3.1");
#'
#' ### This will show the error message
#' tryCatch(
#'   ufs::checkPkgs(
#'     base = "99",
#'     stats = "42.5",
#'     ufs = 20
#'   ),
#'   error = print
#' );
#' }
checkPkgs <- function(...,
                      install = FALSE,
                      load = FALSE,
                      repos = "https://cran.rstudio.com") {
  vrsn <- unlist(list(...));
  if (is.null(names(vrsn))) {
    x <- vrsn;
    vrsn <- rep("0.0.0.1", length(x));
  } else {
    x <- names(vrsn);
  }
  installedPkgs <- utils::installed.packages();
  pkgNames <- installedPkgs[, 'Package'];
  res <- stats::setNames(rep(FALSE, length(x)),
                         x);
  for (i in seq_along(x)) {
    if (x[i] %in% pkgNames) {
      if (utils::compareVersion(as.character(utils::packageVersion(x[i])), vrsn[i]) < 0) {
        res[x[i]] <- TRUE;
      }
    } else {
      res[x[i]] <- TRUE;
    }
  }
  if (any(res)) {
    if (install) {
      installedPkgs(x[res], repos=repos);
    } else {
      stop("Of package(s) ", vecTxtQ(x[res]),
           ", you need at least versions ", vecTxt(vrsn[res]),
           ", respectively. Install those with:\n\n",
           "install.packages(c(",
           vecTxtQ(x[res], lastDelimiter = ", "),
           "));\n");
    }
  }
  if (load) {
    suppressMessages(invisible(lapply(x[!res],
                                      require)));
  }
  return(invisible(res));
}

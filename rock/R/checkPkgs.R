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
#' rock::checkPkgs('base');
#'
#' ### Require a specific version
#' rock::checkPkgs(rock = "0.9.1");
#'
#' ### This will show the error message
#' tryCatch(
#'   rock::checkPkgs(
#'     base = "99",
#'     stats = "42.5",
#'     rock = 2000
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

  presences <-
    unlist(
      lapply(
        x,
        requireNamespace,
        quietly = TRUE
      )
    );
  names(presences) <- x;

  res <- rep(FALSE, length(x));
  names(res) <- x;

  for (i in x) {
    if (presences[i]) {
      if (utils::compareVersion(as.character(utils::packageVersion(i)), vrsn[i]) < 0) {
        res[i] <- TRUE;
      }
    } else {
      res[i] <- TRUE;
    }
  }
  if (any(res)) {
    if (install) {
      utils::install.packages(x[res],
                              repos=repos);
    } else {
      stop("Of package(s) ", vecTxtQ(x[res]),
           ", you need at least versions ", vecTxt(vrsn[res]),
           ", respectively. Install those with:\n\n  ",
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

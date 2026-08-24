#' Load a package, install if not available
#'
#' @param packageName The package
#' @param mirrorIndex The index of the mirror (1 is used if not specified)
#'
#' @export
safeRequire <- function(packageName, mirrorIndex=NULL) {
  if (!is.element(packageName, utils::installed.packages()[,1])) {
    if (is.null(mirrorIndex)) {
      utils::chooseCRANmirror(ind=1);
    } else {
      utils::chooseCRANmirror(ind=mirrorIndex);
    }
    utils::install.packages(packageName, dependencies=TRUE);
  }
  suppressPackageStartupMessages(require(package = packageName,
                                         character.only=TRUE,
                                         quietly=TRUE));
}

#' @title HMDA popup message
#' @description notifies the user about the current status of the package
#' @importFrom curl curl
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
#'
.onAttach <- function(lib, pkg) {
  packageStartupMessage(
    'Holistic Multimodel Domain Analysis (HMDA)\n
   \nNote1: This is a free software and comes with no guarantee.
   \nNote2: Currently the random forest and gradient boosting algorithms\n       are supported
   \nNote3: To improve HMDA, please share your thoughts and comments\n       https://github.com/haghish/HMDA/discussions \n       haghish@uio.no
   \nNote4: If you find a bug, post it on GitHub\n       https://github.com/haghish/HMDA\n'
  )

}

#    \nNote2: If you use HMDA, please consider writing and sharing a tutorial\n       for it. You can reduce imputation error by\n       adding other algorithms e.g. "RF", "Ensemble", ...

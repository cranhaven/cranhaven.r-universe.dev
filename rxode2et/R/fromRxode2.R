.hasRxode2 <- FALSE
#' This tells 'rxode2et' that 'rxode2' is available
#'
#' This really should not be called by a user.  This call allows
#' 'rxode2' solves to be a bit more flexible (and actually work!)
#' 
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @export 
#' @keywords internal
.setRxode2 <- function() {
  assignInMyNamespace(".hasRxode2", TRUE)
  invisible()
}

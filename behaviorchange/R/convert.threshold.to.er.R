#' @rdname nncvis
#' @export
convert.threshold.to.er <- function(threshold, mean, sd,
                                    eventIfHigher = TRUE,
                                    pdist = stats::pnorm) {
  return(pdist(threshold, mean=mean, sd=sd, lower.tail=!eventIfHigher));
}

#' @rdname nncvis
#' @export
convert.er.to.threshold <- function(er, mean, sd,
                                    eventIfHigher = TRUE,
                                    qdist = stats::qnorm) {
  q <- qdist(er);
  if (eventIfHigher) {
    return(mean - q * sd);
  } else {
    return(mean + q * sd);
  }
}

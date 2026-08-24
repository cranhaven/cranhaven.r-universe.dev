#' @rdname nncConversion
#' @export
convert.cer.to.d <- function(cer, eer, eventDesirable=TRUE, eventIfHigher=TRUE,
                             dist = "norm", distArgs=NULL, distNS="stats") {
  qdistFuncName <- paste0("q", dist);
  qdist <-
    function(p) {
      return(
        do.call(
          utils::getFromNamespace(qdistFuncName, ns = distNS),
          c(list(p = p), distArgs)
        )
      );
    }
  if (eventIfHigher) {
    return(qdist(eer) - qdist(cer));
  } else {
    return(qdist(cer) - qdist(eer));
  }
}

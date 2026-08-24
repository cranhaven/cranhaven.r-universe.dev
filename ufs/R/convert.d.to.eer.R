#' @rdname nncConversion
#' @export
convert.d.to.eer <- function(d, cer, eventDesirable=TRUE, eventIfHigher=TRUE,
                             dist = "norm", distArgs=list(), distNS="stats") {
  pdistFuncName <- paste0("p", dist);
  qdistFuncName <- paste0("q", dist);
  pdist <-
    function(q) {
      return(
        do.call(
          utils::getFromNamespace(pdistFuncName, ns = distNS),
          c(list(q = q), distArgs)
        )
      );
    }
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
    return(pdist(qdist(cer) + d));
  } else {
    return(1 - pdist(qdist(1-cer) + d));
  }
}

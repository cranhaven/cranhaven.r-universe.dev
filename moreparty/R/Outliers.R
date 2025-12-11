#' @importFrom stats median mad

#' @export

Outliers <- function(prox, cls=NULL, data=NULL, threshold=10) {
  if (nrow(prox) != ncol(prox)) stop ("prox must be a square matrix")
  n <- nrow(prox)
  if (is.null(cls)) cls <- rep(1, n)
  cls <- factor(cls)
  lvl <- levels(cls)
  cls.n <- table(cls)[lvl]
  id <- if (is.null(rownames(prox))) 1:n else rownames(prox)
  scores <- structure(rep(NA, n), names=id)
  for (i in lvl) {
    iclass <- cls == i
    out <- rowSums(prox[iclass, iclass]^2)
    out <- n / ifelse(out == 0, 1, out)
    out <- (out - median(out)) / mad(out)
    scores[iclass] <- out
  }
  
  if(is.null(data)) outliers <- which(scores>threshold)
  if(!is.null(data)) {
    outliers <- data.frame(rowname=rownames(data),data,scores)[scores>threshold,]
    rownames(outliers) <- NULL
    }
  
  res = list(scores=scores, outliers=outliers)
  return(res)
}
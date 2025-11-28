DNN <- function(dst, cl, k=NULL, d=NULL, details=FALSE, self=FALSE) {
 if (is.null(k) && is.null(d) || !is.null(k) && !is.null(d))
  stop("either 'k' or 'd' (but not both) must be specified")
 if (sum(is.na(cl)) == 0 & !self) {
  warning("no NAs in class labels, return labels back")
  return(cl)
  }
 .dm <- as.matrix(dst)
 if(self) .ap <- .dm else .ap <- .dm[is.na(cl), , drop=FALSE]
 .clt <- table(cl)
 .vote <- function(.t) {
  .res <- names(.t)[max.col(t(.t), ties.method="random")]
  if (sum(.t -.clt) == 0 || sum(.t) == 0) .res <- NA
  return(.res)
 }
 if (is.null(d)) {
  res <- apply(.ap, 1, function(.x) {
   .t <- table(na.omit(cl[order(.x)])[1:k])
   if (!details) .vote(.t) else .t
   }
   )
 }
 if (is.null(k)) {
  res <- apply(.ap, 1, function(.x) {
   .s <- (.x <= d * max(.dm))
   .t <- table(cl[.s][order(.x[.s])])
   if (!details) .vote(.t) else .t
   }
   )
 }
return(res)
}

## ===

Dnn <- function(trn, tst, classes, FUN=function(.x) dist(.x), ...) {
 dst <- FUN(rbind(trn, tst))
 cl <- factor(c(as.character(classes), rep(NA, nrow(tst))))
 DNN(dst, cl, ...)
}

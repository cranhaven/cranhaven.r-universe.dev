BootKNN <- function(data, classes, sub="none", nsam=4, nboot=1000, misclass=TRUE, method="knn", ...) {
 method <- match.arg(method, choices=c("knn", "dnn"))
 PRED <- matrix(character(0), nrow=nrow(data), ncol=nboot)
 TBL <- table(results=classes, observed=classes)
 TBL[TBL > 0] <- 0
 for(b in 1:nboot) {
  cat(".")
  if (length(sub) == 1 && sub == "none") sub <- !logical(nrow(data))
  data.sub <- data[sub, ]
  classes.sub <- classes[sub]
  sel <- ave(seq_len(nrow(data.sub)), classes.sub, FUN=function(.x) sample.int(length(.x))) <= nsam
  train <- data.sub[sel, ]
  classes.train <- classes.sub[sel]
  if (method == "knn") res <- class::knn(train, data, classes.train, ...)
  if (method == "dnn") res <- Dnn(trn=train, tst=data, classes=classes.train, ...)
  if (misclass) TBL <- TBL + table(res, classes)
  PRED[, b] <- as.character(res)
 }
 cat("\n")
 if (misclass){
  cat("\n")
  TBLb <- round(TBL/nboot)
  sum <- colSums(TBLb)
  dia <- diag(TBLb)
  msc <- (sum - dia)/sum * 100
  m.m <- mean(msc)
  cat("Classification table:", "\n")
  print(TBLb)
  cat("Misclassification errors:", "\n")
  print(round(msc, 1))
  cat("Mean misclassification error: ", round(m.m, 1), "%", "\n", sep="")
 }
 invisible(PRED)
}

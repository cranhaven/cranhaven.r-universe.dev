#' @importFrom pRoc auc

#' @export

PerfsBinClassif <- function(pred,actual) {
  t <- table(predicted=factor(as.numeric(pred>0.5),levels=c(0,1)),actual)
  acc <- sum(diag(t))/sum(t)
  tpr <- t[2,2]/sum(t[,2])
  tnr <- t[1,1]/sum(t[,1])
  bac <- (tpr+tnr)/2
  auc <- suppressMessages(pROC::auc(actual,pred, quite=TRUE))
  res <- c(tpr,tnr,acc,bac,auc)
  res <- round(res,3)
  names(res) <- c("tpr","tnr","acc","bac","auc")
  return(res)
}

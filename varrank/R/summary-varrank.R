###############################################################################
## summary.varrank.R ---
## Author          : Gilles Kratzer
## Last modified   : 05/02/2018
##                 :
###############################################################################

summary.varrank <- function(object, digits=3, ...){

  if(object$algorithm=="forward") out1 <- as.matrix(round(t(diag(object$distance.m)),digits = digits))
  if(object$algorithm=="backward") out1 <- as.matrix(round(t(diag(object$distance.m[dim(object$distance.m)[2]:1,])),digits = digits))

  out2<-as.matrix(round(object$distance.m,digits = digits))
  out2[!is.finite(out2)] <- " "
  out2 <- as.data.frame(out2)
  rownames(out1)<-c("Scores")

  cat("Number of variables ranked: ",dim(out1)[2],"\n",sep = "")
  cat(object[[3]], " search using ", object[[4]]," method \n",sep = "")
  cat("(",object[[5]]," scheme) \n \n",sep = "")

  if(object[[3]]=="forward") cat("Ordered variables (decreasing importance):\n")
  if(object[[3]]=="backward") cat("Ordered variables (increasing importance):\n")

  print(out1, ...)
  cat("\n ---")
  cat("\n \n Matrix of scores: \n")

  print(out2, ...)

}#EOF

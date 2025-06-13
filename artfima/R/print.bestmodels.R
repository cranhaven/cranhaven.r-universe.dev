#print.bestmodels
print.bestmodels <- function(x, ...) {
  aicN <- names(x$aic)
  nbest <- length(aicN)
  if (nbest <= 7) {
    cnames <- c("best", "2nd best", "3rd best", "4th best", "5th best", 
                "6th best", "7th best")[1:nbest]
  } else {
    cnames <- as.character(1:nbest)
  }
  aicX <- formatC(x$aic, digits=2, format="f")
  aicP <- formatC(exp(0.5*(x$aic[1] - x$aic)), digits=3, format="f")
  bicN <- names(x$bic)
  bicX <- formatC(x$bic, digits=2, format="f")
  bicP <- formatC(exp(0.5*(x$bic[1] - x$bic)), digits=3, format="f")
  m <- matrix(c(aicN,aicX,aicP,bicN,bicX,bicP), byrow=TRUE, nrow=6)
  dimnames(m) <- list(c("AIC models","AIC","p(AIC)","BIC models",
                        "BIC","p(BIC)"), cnames)
  print(as.data.frame.matrix(m))
}



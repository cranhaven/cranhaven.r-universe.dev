summary.orcutt <-
function(object, ...)
{
  TAB <- cbind(Estimate = coef(object),
               StdErr = object$std.error,
               t.value = round(object$t.value,3),
               p.value = object$p.value)
  colnames(TAB) <- c("Estimate", "Std. Error" ,"t value" ,"Pr(>|t|)")
  
  stat <- c(round(object$rse,4),
            object$df.residual,
            round(object$r.squared,4),
            round(object$adj.r.squared,4),
            round(object$Fs[1],1),
            object$rank,
            object$Fs[2])
  
  r.squared <- round(object$r.squared,4)
  adj.r.squared <- round(object$adj.r.squared,4)
  
  fstatistic <- c(round(object$Fs[1],1), object$stat[7])
  
  df <- c(object$rank, object$df.residual)
  
  DW.t <- object$DW
  
  res <- list(call=object$call,
              coefficients=TAB, 
              stat = stat,
              DW.t = DW.t)
  
  class(res) <- "summary.orcutt"
  res
}

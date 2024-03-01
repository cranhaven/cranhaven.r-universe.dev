print.summary.penmodel <- function (x, digits = max(3, getOption("digits") - 3), 
                                    signif.stars=TRUE, ...) 
{
  savedig <- options(digits = digits)
  on.exit(options(savedig))
  
  cat("Estimates: \n")

  if(signif.stars){
    k <- ifelse(attr(x,"robust"), 5, 4) 
    Signif <- symnum(x$estimates[,k], corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("***", "**", "*", ".", " "))
    
    coefstars <- cbind(data.frame(x$estimates), format(Signif) )
    colnames(coefstars) <- c(colnames(x$estimates), "")
    print(coefstars)
    cat("Signif. codes:   0 '***'  0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 \n")
    x$estimates <- coefstars 
  }
  else print(x$estimates)
    

  invisible(x)
  
}

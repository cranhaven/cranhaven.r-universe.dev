print.penmodel <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{
  savedig <- options(digits = digits)
  on.exit(options(savedig))
  frailty.dist <- attr(x, "frailty.dist")
  
  cat("Call: \n")
  if(frailty.dist=="none" | is.null(frailty.dist)){
  cat("Penetrance model for",attr(x,"design"),"design using",
      attr(x,"base.dist"), "baseline distribution \n")
  }
  else{
    f <- match(frailty.dist, c("gamma", "lognormal", "cgamma", "clognormal"))
    fname <- c("Gamma", "Log-normal", "Correlated gamma", "Correlated log-normal")[f]
    cat("Penetrance model for",attr(x,"design"),"design using \n", fname, "frailty distribution with",
        attr(x,"base.dist"), "baseline distribution \n")
  }
  cat("Minimum age at onset used: ", attr(x, "agemin"))
  cat("\n")
  cat("\nEstimates: \n")
  print(x$estimates)
  if(attr(x, "robust")) cat("Robust standard errors was obtained. \n")
  invisible(x)

}

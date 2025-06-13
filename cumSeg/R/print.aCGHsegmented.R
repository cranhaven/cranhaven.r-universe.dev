print.aCGHsegmented <-
function(x, digits = max(3, getOption("digits") - 3), ...){
      #cat("Call:\n")
      #print(x$call)
      r<- x$y-x$fitted.values
      RSS<-sum(r^2)
      n<-length(x$fitted.values)
      psi<- x$psi
      medie<-x$est.means
      cat("\nNo. obs:", n, "   No. parameters:", 2*length(psi), "   No. changepoints:", length(psi),"\n")
      cat("Res. Sum of Squares:", round(RSS, digits), "  est.st.dev.=", round(sqrt(RSS/(n-2*length(psi))),digits), "\n")
      cat("=====================\n")
      cat("Estimated means:", round(medie,3), "\n")
      cat("Estimated change point(s):", psi, "\n")
}



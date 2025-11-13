print.summary.catpredi <-
function(x, ...) {
  print.catpredi(x, digits = x$digits)

  cat("\n\n---------------------------------------------------\n")
  cat("Fitted model for the categorized predictor variable\n")
  cat("---------------------------------------------------\n\n")
  tpm <- summary(x$fit.gam)
  digits = max(3, getOption("digits") - 3)
  signif.stars = getOption("show.signif.stars")
   print(tpm$family)
      cat("Formula:\n")
      if (is.list(tpm$formula))
          for (i in 1:length(tpm$formula)) print(tpm$formula[[i]])
      else print(tpm$formula)
      if (length(tpm$p.coeff) > 0) {
          cat("\nParametric coefficients:\n")
          printCoefmat(tpm$p.table, digits = digits, signif.stars = signif.stars,
              na.print = "NA", ...)
      }
      cat("\n")
      if (tpm$m > 0) {
          cat("Approximate significance of smooth terms:\n")
          printCoefmat(tpm$s.table, digits = digits, signif.stars = signif.stars,
              has.Pvalue = TRUE, na.print = "NA", cs.ind = 1, ...)
      }
      cat("\n")
      if (!is.null(tpm$rank) && tpm$rank < tpm$np)
          cat("Rank: ", tpm$rank, "/", tpm$np, "\n", sep = "")
      
      invisible(tpm)

}

#' Personalised Synthetic Controls - print
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return printing psc results
#' @export
print.psc <- function(x,...){

  frm <- x$DC_clean$model_extract$formula;frm
  fam <- x$DC_clean$model_extract$family;fam
  mt <- x$model.type

  x$DC_clean$model_extract

  cat("Counterfactual Model (CFM): \n")

  if("glm"%in%mt){
    cat(
      paste("A model of class 'GLM'"," \n",sep=""),
      paste("Family: ",fam$family," \n",sep=""),
      paste("Link: ",fam$link," \n",sep=""))
  }

  if("flexsurvreg"%in%mt){
    cat(
      paste("A model of class 'flexsurvreg'"," \n",sep=""),
      paste("Fit with ",x$DC_clean$model_extract$k," internal knots","\n",sep=""))
  }


  cat("\n")
  cat("Formula: \n")
  print(frm)

  cat("\n")

  cat("Call:\n", "CFM model + beta")
  cat("\n")
  cat("\n")
  cat("Coefficients:\n")

  print.default(format(coef(x), digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)

}

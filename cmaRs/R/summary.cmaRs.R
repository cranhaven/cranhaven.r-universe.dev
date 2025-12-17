#' A summary function designed for CMARS
#'
#' This function allows you to print the output of CMARS model.
#' @param object A cmaRs object which is constructed by cmaRs.
#' @param ... Additional arguments affecting the summary result.
#' @importFrom utils write.table
#' @return An S3 model of class "summary.cmaRs"
#' @export
#' @examples
#' \dontrun{
#' # Without \code{MOSEK}, the example code is not executable.
#' # For installation of Mosek, plese see the documentation of 'Rmosek'.
#' data("trees", package = "datasets")
#' model.prediction <- cmaRs(Volume ~ ., degree = 5, nk = 20, data = trees)
#' summary.cmaRs(model.prediction)
#' }
summary.cmaRs <- function(object, ...) {
  coefficients <- object$coefficients
  numBF <- object$number.of.BF
  final.sqrtz <- object$final.sqrtz
  bf.cmars <- object$bf.cmars
  L <- object$L
  DMS <- object$DMS
  VARMS <- object$VARMS
  response.name <- object$response.name
  R2 <- object$R2
  r <- object$r
  RSS <- object$RSS
  AUC <- object$AUC
  MCR <- object$MCR
  PCC <- object$PCC
  AUC <- object$AUC
  precision <- object$precision
  recall <- object$recall
  specificity <- object$specificity
  classification <- object$classification
  fitted.binary <- object$fitted.binary
  # the final model

  cat("Call:\n")
  # use paste0 to convert vector of strings to single string if necessary
  cat(strwrap(paste0(deparse(object$call, control = NULL, nlines = 5),
    sep = " ", collapse = " "
  ), exdent = 6), sep = "\n")
  cat("\n")

  Theta.original <- object$coefficients
  Theta.cmars <- matrix(c(round(object$coefficients, digits = 4)), ncol <- 1)
  out1 <- c()
  for (i in 1:(object$number.of.BF + 1))
  {
    if (sign(Theta.cmars[i]) == +1) {
      out1 <- c(out1, paste("+", Theta.cmars[i], sep = ""))
    } else {
      out1 <- c(out1, Theta.cmars[i])
    }
  }

  output.cmars.first <- paste(object$response.name, " = ", out1[1], sep = "")
  Theta.cmars <- out1[2:(length(out1))]
  Theta.cmars <- paste(Theta.cmars, " *", sep = "")
  Theta.cmars <- paste(Theta.cmars, sep = "")
  output.cmars <- paste(Theta.cmars, object$bf.cmars)
  output.cmars.final <- c()
  output.cmars.final <- output.cmars.first
  for (i in 1:object$number.of.BF)
  {
    output.cmars.final[i + 1] <- output.cmars[i]
  }
  output.cmars.final <- str_replace_all(output.cmars.final, "first", "")
  utils::write.table(format(output.cmars.final, justify = "left"),
    row.names = FALSE, col.names = FALSE, quote = FALSE
  )
  cat("\n")

  if (object$classification == TRUE) {
    cat(
      "AUC", round(object$AUC, digits = 4),
      "MCR", round(object$MCR, digits = 4),
      "PCC", round(object$PCC, digits = 4), "precision",
      round(object$precision, digits = 4),
      "recall", round(object$recall, digits = 4),
      "specificity", round(object$specificity, digits = 4), "\n"
    )
  } else {
    cat(
      "R2", round(object$R2, digits = 4), "r",
      round(object$r, digits = 4),
      "RSS", round(object$RSS, digits = 4), "\n"
    )
  }


  retval <- list(
    call = object$call,
    coefficients = coefficients,
    output.cmars.final = output.cmars.final,
    final.sqrtz = final.sqrtz,
    L = L,
    DMS = DMS,
    VARMS = VARMS,
    R2 = R2,
    r = r,
    RSS = RSS,
    MCR = MCR,
    AUC = AUC,
    PCC = PCC,
    AUC = AUC,
    recall = recall,
    precision = precision,
    specificity = specificity,
    classification = classification,
    fitted.binary = fitted.binary
  )


  class(retval) <- "summary.cmaRs"
  invisible(retval)
}

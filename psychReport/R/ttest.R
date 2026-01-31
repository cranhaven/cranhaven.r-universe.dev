#' @title statStrT
#'
#' @description Returns required Latex formatted string T-test required for R/Knitr integration.
#' For example, \emph{t}(11) = 3.45, \emph{p} < 0.05.
#' Returns values to 2 sig decimal places and < 0.01, < 0.001 for p values.
#'
#' @param tObj The returned object from a call to t.test
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) levels
#' dat <- createDF(nVP = 50,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 100, 100),
#'                                 "Comp incomp" = c(600, 100, 100)))
#'
#' tObj <- t.test(dat$RT[dat$Comp == "incomp"],
#'                dat$RT[dat$Comp == "comp"],
#'                paired = TRUE)
#'
#' statStrT <- statStrT(tObj)
#'
#' @export
statStrT <- function(tObj) {
  return(paste0(tValueString(tObj), ", ", pValueString(tObj$p.value)))
}



#' @title tValueString
#'
#' @description Returns required Latex formatted string for \emph{t}(df) = XXX for
#' R/knitr integration. Returns values to 2 sig decimal places.
#'
#' @param tObj The returned object from a call to t.test
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) levels
#' dat <- createDF(nVP = 50,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 100, 100),
#'                                 "Comp incomp" = c(600, 100, 100)))
#'
#' tObj <- t.test(dat$RT[dat$Comp == "incomp"],
#'                dat$RT[dat$Comp == "comp"],
#'                paired = TRUE)
#'
#' tString <- tValueString(tObj)
#'
#' @export
tValueString <- function(tObj) {

  tVal   <- format(round(tObj$statistic, 2), nsmall = 2)
  DF     <- tObj$parameter
  if (DF %% 1 != 0) {
    DF <- format(round(DF, 2), nsmall = 2)
  }

  return(paste0("\\emph{t}", "(", DF, ") = ", tVal, sep = ""))

}



#' @title meanStrT
#'
#' @description Returns a string with the mean value from a t.test in Latex format.
#'
#' @param tObj The returned object from a call to t.test
#' @param numDigits The number of digits to round to
#' @param unit "" vs. "ms" vs. "mv" vs. "\%"
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data
#' dat <- createDF(nVP = 50,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 100, 100),
#'                                 "Comp incomp" = c(600, 100, 100)))
#'
#' tObj <- t.test(dat$RT[dat$Comp == "incomp"],
#'                dat$RT[dat$Comp == "comp"],
#'                paired = TRUE)
#'
#' tString <- meanStrT(tObj, numDigits = 0, unit = "ms")
#'
#' @export
meanStrT <- function(tObj, numDigits = 0, unit = "") {
  if (length(tObj$estimate) == 1) {
    return(paste0(numValueString(tObj$estimate[1], numDigits, unit)))
  } else {
    return(paste0(numValueString(tObj$estimate[1], numDigits, ""),
                " vs. ",    numValueString(tObj$estimate[2], numDigits, unit)))
  }
}



#' @title ciStrT
#'
#' @description Returns a string with the 95\% CI from a t.test in Latex format.
#'
#' @param tObj The returned object from a call to t.test
#' @param numDigits The number of digits to round to
#' @param unit "" vs. "ms" vs. "mv" vs. "\%"
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) levels
#' dat <- createDF(nVP = 50,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 100, 100),
#'                                 "Comp incomp" = c(600, 100, 100)))
#'
#' tObj <- t.test(dat$RT[dat$Comp == "incomp"],
#'                dat$RT[dat$Comp == "comp"],
#'                paired = TRUE)
#'
#' ciString <- ciStrT(tObj, unit = "ms")
#'
#' @export
ciStrT <- function(tObj, numDigits = 0, unit = "") {
  return(paste0("95\\% CI: ", numValueString(tObj$conf.int[1], numDigits, ""),
                " to ",    numValueString(tObj$conf.int[2], numDigits, unit)))
}

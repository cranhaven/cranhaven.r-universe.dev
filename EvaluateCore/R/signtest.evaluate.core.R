### This file is part of 'EvaluateCore' package for R.

### Copyright (C) 2018-2022, ICAR-NBPGR.
#
# EvaluateCore is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# EvaluateCore is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/


#' Sign Test
#'
#' Test difference between means and variances of entire collection (EC) and
#' core set (CS) for quantitative traits by Sign test (\mjseqn{+} versus
#' \mjseqn{-})
#' \insertCite{basigalup_development_1995,tai_core_2001}{EvaluateCore}.
#' \loadmathjax
#'
#' The test statistic for Sign test (\mjseqn{\chi^{2}}) is computed as follows.
#'
#' \mjsdeqn{\chi^{2} = \frac{(N_{1}-N_{2})^{2}}{N_{1}+N_{2}}}
#'
#' Where, where \mjseqn{N_{1}} is the number of variables for which the mean or
#' variance of the CS is greater than the mean or variance of the EC (number of
#' \mjseqn{+} signs); \mjseqn{N_{2}} is the number of variables for which the
#' mean or variance of the CS is less than the mean or variance of the EC
#' (number of \mjseqn{-} signs). The value of \mjseqn{\chi^{2}} is compared with
#' a Chi-square distribution with 1 degree of freedom.
#'
#' @param data The data as a data frame object. The data frame should possess
#'   one row per individual and columns with the individual names and multiple
#'   trait/character data.
#' @param names Name of column with the individual names as a character string
#' @param quantitative Name of columns with the quantitative traits as a
#'   character vector.
#' @param selected Character vector with the names of individuals selected in
#'   core collection and present in the \code{names} column.
#'
#' @return A data frame with the following components. \item{Comparison}{The
#'   comparison measure.} \item{ChiSq}{The test statistic (\mjseqn{\chi^{2}}).}
#'   \item{p.value}{The p value for the test statistic.} \item{significance}{The
#'   significance of the test statistic (*: p \mjseqn{\leq} 0.01; **: p
#'   \mjseqn{\leq} 0.05; ns: p \mjseqn{ > } 0.05).}
#'
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
#' data("cassava_CC")
#' data("cassava_EC")
#'
#' ec <- cbind(genotypes = rownames(cassava_EC), cassava_EC)
#' ec$genotypes <- as.character(ec$genotypes)
#' rownames(ec) <- NULL
#'
#' core <- rownames(cassava_CC)
#'
#' quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
#'            "ARSR", "SRDM")
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#'
#' ec[, qual] <- lapply(ec[, qual],
#'                      function(x) factor(as.factor(x)))
#'
#' signtest.evaluate.core(data = ec, names = "genotypes",
#'                        quantitative = quant, selected = core)
#'
signtest.evaluate.core <- function(data, names, quantitative, selected) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  mdiff <- snk.evaluate.core(data, names, quantitative,
                             selected)
  vdiff <- levene.evaluate.core(data, names, quantitative,
                                selected)

  mn1 <- sum(mdiff$CS_Mean > mdiff$EC_Mean)
  mn2 <- sum(mdiff$CS_Mean < mdiff$EC_Mean)
  mteststat <- ((mn1 - mn2)^2) / (mn1 + mn2)

  vn1 <- sum(vdiff$CS_CV > vdiff$EC_CV)
  vn2 <- sum(vdiff$CS_CV < vdiff$EC_CV)
  vteststat <- ((vn1 - vn2)^2) / (vn1 + vn2)

  mpvalue <- pchisq(mteststat, df = 1, lower.tail = FALSE)

  vpvalue <- pchisq(vteststat, df = 1, lower.tail = FALSE)

  outdf <- data.frame(Comparison = c("Mean", "Variance"),
                      ChiSq = c(mteststat, vteststat),
                      p.value = c(mpvalue, vpvalue),
                      stringsAsFactors = FALSE, row.names = NULL)

  outdf$significance <- ifelse(outdf$p.value <= 0.01, "**",
         ifelse(outdf$p.value <= 0.05, "*", "ns"))

  return(outdf)

}

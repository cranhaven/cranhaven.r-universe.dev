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


#' Variable Rate of Coefficient of Variation
#'
#' Compute the Variable Rate of Coefficient of Variation (\mjseqn{VR})
#' \insertCite{hu_methods_2000}{EvaluateCore} to compare quantitative traits of
#' the entire collection (EC) and core set (CS). \loadmathjax
#'
#' The Variable Rate of Coefficient of Variation (\mjseqn{VR}) is computed as
#' follows.
#'
#' \mjsdeqn{VR = \left ( \frac{1}{n} \sum_{i=1}^{n}
#' \frac{CV_{CS_{i}}}{CV_{EC_{i}}} \right ) \times 100}
#'
#' Where, \mjseqn{CV_{CS_{i}}} is the coefficients of variation for the
#' \mjseqn{i}th trait in the CS, \mjseqn{CV_{EC_{i}}} is the coefficients of
#' variation for the \mjseqn{i}th trait in the EC and \mjseqn{n} is the total
#' number of traits
#'
#' @inheritParams snk.evaluate.core
#'
#' @return The \mjseqn{VR} value.
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
#' vr.evaluate.core(data = ec, names = "genotypes",
#'                  quantitative = quant, selected = core)
#'
vr.evaluate.core <- function(data, names, quantitative, selected) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  vdiff <- levene.evaluate.core(data, names, quantitative,
                                selected)

  VR <- (sum(vdiff$CS_CV / vdiff$EC_CV) / length(quantitative)) * 100

  return(VR)

}

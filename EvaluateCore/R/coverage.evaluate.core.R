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


#' Class Coverage
#'
#' Compute the Class Coverage \insertCite{kim_powercore_2007}{EvaluateCore} to
#' compare the distribution frequencies of qualitative traits between entire
#' collection (EC) and core set (CS). \loadmathjax
#'
#' Class Coverage \insertCite{kim_powercore_2007}{EvaluateCore} is computed as
#' follows.
#'
#' \mjsdeqn{Class\, Coverage = \left ( \frac{1}{n} \sum_{i=1}^{n}
#' \frac{A_{CS_{i}}}{A_{EC_{i}}} \right ) \times 100}
#'
#' Where, \mjseqn{A_{CS_{i}}} is the sets of categories in the CS for the
#' \mjseqn{i}th trait, \mjseqn{A_{EC_{i}}} is the sets of categories in the EC
#' for the \mjseqn{i}th trait and \mjseqn{n} is the total number of traits.
#'
#' @inheritParams chisquare.evaluate.core
#'
#' @return The Class Coverage value.
#'
#' @import mathjaxr
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
#' coverage.evaluate.core(data = ec, names = "genotypes",
#'                        qualitative = qual, selected = core)
#'
coverage.evaluate.core <- function(data, names, qualitative, selected) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       qualitative = qualitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  cdiff <- chisquare.evaluate.core(data, names, qualitative,
                                   selected)

  coverage <- (sum(cdiff$CS_No.Classes /
                     cdiff$EC_No.Classes) / length(qualitative)) * 100


  return(coverage)

}

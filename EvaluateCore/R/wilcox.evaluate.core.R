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


#' Wilcoxon Rank Sum Test
#'
#' Compare the medians of quantitative traits between entire collection (EC) and
#' core set (CS) by Wilcoxon rank sum test or Mann-Whitney-Wilcoxon test or
#' Mann-Whitney U test
#' \insertCite{wilcoxon_individual_1945,mann_test_1947}{EvaluateCore}.
#'
#' @inheritParams snk.evaluate.core
#'
#' @return \item{Trait}{The quantitative trait.} \item{EC_Med}{The median value
#'   of the trait in EC.} \item{CS_Med}{The median value of the trait in CS.}
#'   \item{Wilcox_pvalue}{The p value of the Wilcoxon test for equality of
#'   medians of EC and CS.} \item{Wilcox_significance}{The significance of the
#'   Wilcoxon test for equality of medians of EC and CS.}
#'
#' @seealso \code{\link[stats]{wilcox.test}}
#'
#' @importFrom stats wilcox.test
#' @importFrom dplyr bind_rows
#' @importFrom stats median
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
#' wilcox.evaluate.core(data = ec, names = "genotypes",
#'                      quantitative = quant, selected = core)
#'
wilcox.evaluate.core <- function(data, names, quantitative, selected) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  dataf <- data[, c(names, quantitative)]

  datafcore <- dataf[dataf[, names] %in% selected, ]

  dataf$`[Type]` <- "EC"
  datafcore$`[Type]` <- "CS"

  dataf <- rbind(dataf, datafcore)
  rm(datafcore)

  outdf <- vector(mode = "list", length = length(quantitative))
  names(outdf) <- quantitative

  for (i in seq_along(quantitative)) {
    wilcoxout <- wilcox.test(dataf[dataf$`[Type]` == "EC", quantitative[i]],
                             dataf[dataf$`[Type]` == "CS", quantitative[i]],
                             alternative = "two.sided")

    outdf[[quantitative[i]]] <- data.frame(`Trait` = quantitative[i],
                                           `EC_Med` = stats::median(dataf[dataf$`[Type]` == "EC", quantitative[i]]),
                                           `CS_Med` = stats::median(dataf[dataf$`[Type]` == "CS", quantitative[i]]),
                                           `Wilcox_pvalue` = wilcoxout$p.value,
                                           stringsAsFactors = FALSE)

    rm(wilcoxout)
  }

  outdf <- dplyr::bind_rows(outdf)

  outdf$Wilcox_significance <- ifelse(outdf$Wilcox_pvalue <= 0.01, "**",
                                   ifelse(outdf$Wilcox_pvalue <= 0.05, "*",
                                          "ns"))

  return(outdf)

}

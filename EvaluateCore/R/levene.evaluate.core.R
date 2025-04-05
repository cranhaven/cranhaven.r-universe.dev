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


#' Levene's Test
#'
#' Test for  of variances of the entire collection (EC) and core set (CS) for
#' quantitative traits by Levene's test
#' \insertCite{levene_robust_1960}{EvaluateCore}. \loadmathjax
#'
#' @inheritParams snk.evaluate.core
#'
#' @return A data frame with the following columns \item{Trait}{The quantitative
#'   trait.} \item{EC_V}{The variance of the EC.} \item{CS_V}{The variance of
#'   the CS.} \item{EC_CV}{The coefficient of variance of the EC.}
#'   \item{CS_CV}{The coefficient of variance of the CS.}
#'   \item{Levene_Fvalue}{The test statistic.} \item{Levene_pvalue}{The p value
#'   for the test statistic.} \item{Levene_significance}{The significance of the
#'   test statistic (*: p \mjseqn{\leq} 0.01; **: p \mjseqn{\leq} 0.05; ns: p
#'   \mjseqn{ > } 0.05).}
#'
#' @seealso \code{\link[car]{leveneTest}}
#'
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom stats formula
#' @importFrom stats sd
#' @importFrom stats var
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
#' levene.evaluate.core(data = ec, names = "genotypes",
#'                      quantitative = quant, selected = core)
#'
levene.evaluate.core <- function(data, names, quantitative, selected) {
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

  dataf$`[Type]` <- as.factor(dataf$`[Type]`)

  outdf <- vector(mode = "list", length = length(quantitative))
  names(outdf) <- quantitative

  for (i in seq_along(quantitative)) {
    frmla <- stats::formula(paste("`", quantitative[i], "` ~ `[Type]`",
                                  sep = ""))

    leveneout <- car::leveneTest(frmla, data = dataf)

    outdf[[quantitative[i]]] <- data.frame(`Trait` = quantitative[i],
                                           `EC_V` = stats::var(dataf[dataf$`[Type]` == "EC", quantitative[i]]),
                                           `CS_V` = stats::var(dataf[dataf$`[Type]` == "CS", quantitative[i]]),
                                           `EC_CV` = stats::sd(dataf[dataf$`[Type]` == "EC", quantitative[i]]) / mean(dataf[dataf$`[Type]` == "EC", quantitative[i]]),
                                           `CS_CV` = stats::sd(dataf[dataf$`[Type]` == "CS", quantitative[i]]) / mean(dataf[dataf$`[Type]` == "CS", quantitative[i]]),
                                           `Levene_Fvalue` =
                                             leveneout["group", "F value"],
                                           `Levene_pvalue` =
                                             leveneout["group", "Pr(>F)"],
                                           stringsAsFactors = FALSE)

    rm(leveneout, frmla)
  }

  outdf <- dplyr::bind_rows(outdf)

  outdf$Levene_significance <- ifelse(outdf$Levene_pvalue <= 0.01, "**",
                                   ifelse(outdf$Levene_pvalue <= 0.05, "*",
                                          "ns"))

  # DescTools::LeveneTest(dataf[, quantitative[i]], dataf$`[Type]`)

  return(outdf)

}

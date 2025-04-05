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


#' Student's t Test
#'
#' Test difference between means of entire collection (EC) and core set (CS) for
#' quantitative traits by Student's t test
#' \insertCite{student_probable_1908}{EvaluateCore}.
#'
#' @inheritParams snk.evaluate.core
#'
#' @return \item{Trait}{The
#'   quantitative trait.} \item{EC_Min}{The minimum value of the trait in EC.}
#'   \item{EC_Max}{The maximum value of the trait in EC.} \item{EC_Mean}{The
#'   mean value of the trait in EC.} \item{EC_SE}{The standard error of the
#'   trait in EC.} \item{CS_Min}{The minimum value of the trait in CS.}
#'   \item{CS_Max}{The maximum value of the trait in CS.} \item{CS_Mean}{The
#'   mean value of the trait in CS.} \item{CS_SE}{The standard error of the
#'   trait in CS.} \item{ttest_pvalue}{The p value of the Student's t
#'   test for equality of means of EC and CS.} \item{ttest_significance}{The
#'   significance of the Student's t test for equality of means of EC
#'   and CS.}
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @importFrom stats t.test
#' @importFrom dplyr bind_rows
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
#' ttest.evaluate.core(data = ec, names = "genotypes",
#'                     quantitative = quant, selected = core)
#'
ttest.evaluate.core <- function(data, names, quantitative, selected) {
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

    tout <- t.test(dataf[dataf$`[Type]` == "EC", quantitative[i]],
                  dataf[dataf$`[Type]` == "CS", quantitative[i]])

    outdf[[quantitative[i]]] <- data.frame(`Trait` = quantitative[i],
                                           `EC_Min` = min(dataf[dataf$`[Type]` == "EC", quantitative[i]]),
                                           `EC_Max` = max(dataf[dataf$`[Type]` == "EC", quantitative[i]]),
                                           `EC_Mean` = mean(dataf[dataf$`[Type]` == "EC", quantitative[i]]),
                                           `EC_SE` = sd(dataf[dataf$`[Type]` == "EC", quantitative[i]]) / sqrt(length(dataf[dataf$`[Type]` == "EC", quantitative[i]])),
                                           `CS_Min` = min(dataf[dataf$`[Type]` == "CS", quantitative[i]]),
                                           `CS_Max` = max(dataf[dataf$`[Type]` == "CS", quantitative[i]]),
                                           `CS_Mean` = mean(dataf[dataf$`[Type]` == "CS", quantitative[i]]),
                                           `CS_SE` = sd(dataf[dataf$`[Type]` == "CS", quantitative[i]]) / sqrt(length(dataf[dataf$`[Type]` == "CS", quantitative[i]])),
                                           `ttest_pvalue` = tout$p.value,
                                           stringsAsFactors = FALSE)

    rm(tout)
  }

  outdf <- dplyr::bind_rows(outdf)

  outdf$ttest_significance <- ifelse(outdf$ttest_pvalue <= 0.01, "**",
                                   ifelse(outdf$ttest_pvalue <= 0.05, "*",
                                          "ns"))

  return(outdf)

}

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


#' Distance Between Probability Distributions
#'
#' Compute Kullback-Leibler
#' \insertCite{kullback_information_1951}{EvaluateCore}, Kolmogorov-Smirnov
#' \insertCite{kolmogorov_sulla_1933,smirnov_table_1948}{EvaluateCore} and
#' Anderson-Darling distances
#' \insertCite{anderson_asymptotic_1952}{EvaluateCore} between the probability
#' distributions of collection (EC) and core set (CS) for quantitative traits.
#' \loadmathjax
#'
#' @inheritParams snk.evaluate.core
#'
#' @return A data frame with the following columns. \item{Trait}{The
#'   quantitative trait.} \item{KL_Distance}{The Kullback-Leibler distance
#'   \insertCite{kullback_information_1951}{EvaluateCore} between EC and CS.}
#'   \item{KS_Distance}{The Kolmogorov-Smirnov distance
#'   \insertCite{kolmogorov_sulla_1933,smirnov_table_1948}{EvaluateCore} between
#'   EC and CS.} \item{KS_pvalue}{The p value of the Kolmogorov-Smirnov
#'   distance.} \item{AD_Distance}{Anderson-Darling distance
#'   \insertCite{anderson_asymptotic_1952}{EvaluateCore} between EC and CS.}
#'   \item{AD_pvalue}{The p value of the Anderson-Darling distance.}
#'   \item{KS_significance}{The significance of the Kolmogorov-Smirnov distance
#'   (*: p \mjseqn{\leq} 0.01; **: p \mjseqn{\leq} 0.05; ns: p \mjseqn{>}
#'   0.05).} \item{AD_pvalue}{The significance of the Anderson-Darling distance
#'   (*: p \mjseqn{\leq} 0.01; **: p \mjseqn{\leq} 0.05; ns: p \mjseqn{>}
#'   0.05).}
#'
#' @seealso \code{\link[entropy]{KL.plugin}}, \code{\link[stats]{ks.test}},
#'   \code{\link[kSamples]{ad.test}}
#'
#' @importFrom entropy KL.plugin
#' @importFrom entropy discretize
#' @importFrom stats ks.test
#' @importFrom kSamples ad.test
#' @importFrom grDevices nclass.FD
#' @importFrom dplyr bind_rows
#' @export
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
#' pdfdist.evaluate.core(data = ec, names = "genotypes",
#'                       quantitative = quant, selected = core)
#'
pdfdist.evaluate.core <- function(data, names, quantitative, selected) {
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

  # Kullback–Leibler distance
  nbinscs <- grDevices::nclass.FD(dataf[dataf$`[Type]` == "CS",
                                        quantitative[i]])
  rangeec <- range(dataf[dataf$`[Type]` == "EC", quantitative[i]])

  g1 <- entropy::discretize(dataf[dataf$`[Type]` == "EC", quantitative[i]],
                            nbinscs, rangeec)
  g1[g1 == 0] <- 0.000000001 #Smoothing
  g2 <- entropy::discretize(dataf[dataf$`[Type]` == "CS", quantitative[i]],
                            nbinscs, rangeec)
  g2[g2 == 0] <- 0.000000001 #Smoothing

  kl <- entropy::KL.plugin(g1, g2)

  # Kolmogorov-Smirnov distance
  ks <- ks.test(dataf[dataf$`[Type]` == "EC", quantitative[i]],
                dataf[dataf$`[Type]` == "CS", quantitative[i]],
                exact = FALSE)

  # Anderson-Darling distance
  ad <- kSamples::ad.test(dataf[dataf$`[Type]` == "EC", quantitative[i]],
                          dataf[dataf$`[Type]` == "CS", quantitative[i]],
                          dist = TRUE)

  outdf[[i]] <- data.frame(Trait = quantitative[i],
                   `KL_Distance` = kl,
                   `KS_Distance` = ks$statistic,
                   `KS_pvalue` = ks$p.value,
                   `AD_Distance` = ad$ad["version 1:", "AD"],
                   `AD_pvalue` = ad$ad["version 1:", " asympt. P-value"],
                    stringsAsFactors = FALSE)

  rm(nbinscs, g1, g2, kl)
  }

  outdf <- dplyr::bind_rows(outdf)

  outdf$KS_significance <- ifelse(outdf$KS_pvalue <= 0.01, "**",
                                   ifelse(outdf$KS_pvalue <= 0.05, "*", "ns"))
  outdf$AD_significance <- ifelse(outdf$AD_pvalue <= 0.01, "**",
                                  ifelse(outdf$AD_pvalue <= 0.05, "*", "ns"))

  rownames(outdf) <- NULL

  return(outdf)

}

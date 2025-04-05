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


#' Phenotypic Correlations
#'
#' Compute phenotypic correlations \insertCite{pearson_note_1895}{EvaluateCore}
#' between traits, plot correlation matrices as correlograms
#' \insertCite{friendly_corrgrams_2002}{EvaluateCore} and calculate mantel
#' correlation \insertCite{legendre_interpretation_2012}{EvaluateCore} between
#' them to compare entire collection (EC) and core set (CS). \loadmathjax
#'
#' @inheritParams snk.evaluate.core
#' @inheritParams chisquare.evaluate.core
#'
#' @return A list with the following components. \item{Correlation Matrix}{The
#'   matrix with phenotypic correlations between traits in EC (below diagonal)
#'   and CS (above diagonal).} \item{Correologram}{A correlogram of phenotypic
#'   correlations between traits in EC (below diagonal) and CS (above diagonal)
#'   as a \code{ggplot} object.} \item{Mantel Correlation}{A data frame with
#'   Mantel correlation coefficient (\mjseqn{r}) between EC and CS phenotypic
#'   correlation matrices, it's p value and significance (*: p \mjseqn{\leq}
#'   0.01; **: p \mjseqn{\leq} 0.05; ns: p \mjseqn{ > } 0.05).}
#'
#' @seealso \code{\link[stats]{cor}},
#'   \code{\link[ggcorrplot:ggcorrplot]{cor_pmat}}
#'   \code{\link[ggcorrplot]{ggcorrplot}}, \code{\link[vegan]{mantel}}
#'
#' @import ggcorrplot
#' @import ggplot2
#' @importFrom stats cor
#' @importFrom vegan mantel
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
#' corr.evaluate.core(data = ec, names = "genotypes", quantitative = quant,
#'                    qualitative = qual, selected = core)
#'
corr.evaluate.core <- function(data, names, quantitative, qualitative,
                               selected) {

  if (missing(quantitative)) {
    quantitative <- NULL
  }

  if (missing(qualitative)) {
    qualitative <- NULL
  }

  if (length(c(quantitative, qualitative)) == 1) {
    stop("Only one trait specified")
  }

  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       qualitative = qualitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  dataf <- data[, c(names, quantitative, qualitative)]

  datafcore <- dataf[dataf[, names] %in% selected, ]

  dataf$`[Type]` <- "EC"
  datafcore$`[Type]` <- "CS"

  dataf <- rbind(dataf, datafcore)
  rm(datafcore)

  dataf[, qualitative] <- lapply(dataf[, qualitative],
                                 function(x) as.numeric(as.factor(x)))

  # EC corr
  #########
  eccorr <- stats::cor(dataf[dataf$`[Type]` == "EC",
                             c(quantitative, qualitative)])
  ecpmat <- ggcorrplot::cor_pmat(dataf[dataf$`[Type]` == "EC",
                                       c(quantitative, qualitative)])

  eccorrdf <- formatC(round(eccorr, 2), digits = 2, format = "f")
  eccorrdf[] <- paste0(eccorrdf,
                        ifelse(ecpmat < .01, "**",
                               ifelse(ecpmat < .05, "*", "")),
                        sep = "")

 # CS corr
  #########
  cscorr <- stats::cor(dataf[dataf$`[Type]` == "CS",
                             c(quantitative, qualitative)])
  cspmat <- ggcorrplot::cor_pmat(dataf[dataf$`[Type]` == "CS",
                                       c(quantitative, qualitative)])

  cscorrdf <- formatC(round(cscorr, 2), digits = 2, format = "f")
  cscorrdf[] <- paste0(cscorrdf,
                       ifelse(cspmat < .01, "**",
                              ifelse(cspmat < .05, "*", "")),
                       sep = "")

  # Combine
  #########
  corrdf <- eccorrdf
  corrdf[upper.tri(cscorrdf)] <- cscorrdf[upper.tri(cscorrdf)]
  diag(corrdf) <- NA

  corrdf <- data.frame(corrdf, stringsAsFactors = FALSE)


  corr <- eccorr
  corr[upper.tri(corr)] <- cscorr[upper.tri(cscorr)]
  diag(corr) <- NA

  pmat <- ecpmat
  pmat[upper.tri(pmat)] <- cspmat[upper.tri(cspmat)]
  diag(pmat) <- NA


  corrg <- ggcorrplot(corr, hc.order = FALSE, type = "full",
                      outline.color = "white", p.mat = pmat,
                      ggtheme = theme_bw, show.diag = TRUE,
                      lab = TRUE, legend.title = "Corr")
  corrg <- corrg +
    ggtitle("Below diagonal:EC\nAbove diagonal:CS")

  corrg <- corrg +
    theme(axis.text = element_text(colour = "black"))

 # lower tri - EC below diagonal
 # upper tri - CS above diagonal

  # Mantel test
  ##############

  mcorr <- vegan::mantel(eccorr, cscorr)

  outlist <- list(`Correlation Matrix` = corrdf,
                  `Correologram` = corrg,
                  `Mantel Correlation` = data.frame(r = mcorr$statistic,
                                                    p.value = mcorr$signif,
                                                    significance =
                                                      ifelse(mcorr$signif <= 0.01, "**",
                                                             ifelse(mcorr$signif <= 0.05, "*", "ns"))))


  return(outlist)

}

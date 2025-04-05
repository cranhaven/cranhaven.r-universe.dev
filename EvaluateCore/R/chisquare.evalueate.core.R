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


#' Chi-squared Test for Homogeneity
#'
#' Compare the distribution frequencies of qualitative traits between entire
#' collection (EC) and core set (CS) by Chi-squared test for homogeneity
#' \insertCite{pearson_x._1900,snedecor_chi-square_1933}{EvaluateCore}.
#' \loadmathjax
#'
#' @inheritParams snk.evaluate.core
#' @param qualitative Name of columns with the qualitative traits as a character
#'   vector.
#'
#' @return A a data frame with the following columns. \item{Trait}{The
#'   qualitative trait.} \item{EC_No.Classes}{The number of classes in the trait
#'   for EC.} \item{EC_Classes}{The frequency of the classes in the trait for
#'   EC.} \item{CS_No.Classes}{The number of classes in the trait for CS.}
#'   \item{CS_Classes}{The frequency of the classes in the trait for CS.}
#'   \item{chisq_statistic}{The \mjseqn{\chi^{2}} test statistic.}
#'   \item{chisq_pvalue}{The p value for the test statistic.}
#'   \item{chisq_significance}{The significance of the test statistic (*: p
#'   \mjseqn{\leq} 0.01; **: p \mjseqn{\leq} 0.05; ns: p \mjseqn{ > } 0.05).}
#'
#' @seealso \code{\link[stats]{chisq.test}}
#'
#' @importFrom dplyr bind_rows
#' @importFrom stats chisq.test
#' @importFrom stats pchisq
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
#' chisquare.evaluate.core(data = ec, names = "genotypes",
#'                         qualitative = qual, selected = core)
#'
chisquare.evaluate.core <- function(data, names, qualitative, selected) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       qualitative = qualitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  dataf <- data[, c(names, qualitative)]

  datafcore <- dataf[dataf[, names] %in% selected, ]

  dataf$`[Type]` <- "EC"
  datafcore$`[Type]` <- "CS"

  dataf <- rbind(dataf, datafcore)
  rm(datafcore)

  dataf$`[Type]` <- as.factor(dataf$`[Type]`)

  outdf <- vector(mode = "list", length = length(qualitative))
  names(outdf) <- qualitative

  for (i in seq_along(qualitative)) {

    chiout <- stats::chisq.test(dataf$`[Type]`, dataf[, qualitative[i]],
                         simulate.p.value = TRUE, B = 10000)
    ECclasses <- as.data.frame(table((dataf[dataf$`[Type]` == "EC",
                                            qualitative[i]])))
    CSclasses <- as.data.frame(table((dataf[dataf$`[Type]` == "CS",
                                            qualitative[i]])))
    ECclasses$classfreq <- paste(ECclasses$Var1, "(", ECclasses$Freq, ")",
                                 sep = "")
    CSclasses$classfreq <- paste(CSclasses$Var1, "(", CSclasses$Freq, ")",
                                 sep = "")
    outdf[[qualitative[i]]] <- data.frame(`Trait` = qualitative[i],
                                          `EC_No.Classes` = length(levels(droplevels(dataf[dataf$`[Type]` == "EC",
                                                                                           qualitative[i]]))),
                                          `EC_Classes` =
                                            paste(ECclasses$classfreq,
                                                  collapse = "; "),
                                          `CS_No.Classes` = length(levels(droplevels(dataf[dataf$`[Type]` == "CS",
                                                                                           qualitative[i]]))),
                                          `CS_Classes` =
                                            paste(CSclasses$classfreq,
                                                  collapse = "; "),
                                          `chisq_statistic` = chiout$statistic,
                                          `chisq_pvalue` = chiout$p.value,
                                           stringsAsFactors = FALSE)
    rm(chiout, ECclasses, CSclasses)
  }

  outdf <- dplyr::bind_rows(outdf)

  outdf$chisq_significance <- ifelse(outdf$chisq_pvalue <= 0.01, "**",
                                      ifelse(outdf$chisq_pvalue <= 0.05, "*",
                                             "ns"))

  rownames(outdf) <- NULL

  return(outdf)

}

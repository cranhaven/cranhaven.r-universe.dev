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


#' Percentage Difference of Means and Variances
#'
#' Compute the following differences between the entire collection (EC) and core
#' set (CS). \itemize{ \item Percentage of significant differences of mean
#' (\mjteqn{MD\\\%_{Hu}}{MD\\\\\\\%_{Hu}}{MD\%_{Hu}})
#' \insertCite{hu_methods_2000}{EvaluateCore} \item Percentage of significant
#' differences of variance (\mjteqn{VD\\\%_{Hu}}{VD\\\\\\\%_{Hu}}{VD\%_{Hu}})
#' \insertCite{hu_methods_2000}{EvaluateCore} \item Average of absolute
#' differences between means
#' (\mjteqn{MD\\\%_{Kim}}{MD\\\\\\\%_{Kim}}{MD\%_{Kim}})
#' \insertCite{kim_powercore_2007}{EvaluateCore} \item Average of absolute
#' differences between variances
#' (\mjteqn{VD\\\%_{Kim}}{VD\\\\\\\%_{Kim}}{VD\%_{Kim}})
#' \insertCite{kim_powercore_2007}{EvaluateCore} \item Percentage difference
#' between the mean squared Euclidean distance among accessions
#' (\mjteqn{\overline{d}D\\\%}{\overline{d}D\\\\\\\%}{\overline{d}D\%})
#' \insertCite{studnicki_comparing_2013}{EvaluateCore} } \loadmathjax
#'
#' The differences are computed as follows.
#'
#' \mjtdeqn{MD\\\%_{Hu} = \left ( \frac{S_{t}}{n} \right ) \times
#' 100}{MD\\\\\\\%_{Hu} = \left ( \frac{S_{t}}{n} \right ) \times 100}{MD\%_{Hu}
#' = \left ( \frac{S_{t}}{n} \right ) \times 100}
#'
#' Where, \mjseqn{S_{t}} is the number of traits with a significant difference
#' between the means of the EC and the CS and \mjseqn{n} is the total number of
#' traits. A representative core should have
#' \mjteqn{MD\\\%_{Hu}}{MD\\\\\\\%_{Hu}}{MD\%_{Hu}} < 20 \% and \mjseqn{CR} > 80
#' \% \insertCite{hu_methods_2000}{EvaluateCore}.
#'
#' \mjtdeqn{VD\\\%_{Hu} = \left ( \frac{S_{F}}{n} \right ) \times
#' 100}{VD\\\\\\\%_{Hu} = \left ( \frac{S_{F}}{n} \right ) \times 100}{VD\%_{Hu}
#' = \left ( \frac{S_{F}}{n} \right ) \times 100}
#'
#' Where, \mjseqn{S_{F}} is the number of traits with a significant difference
#' between the variances of the EC and the CS and \mjseqn{n} is the total number
#' of traits. Larger \mjteqn{VD\\\%_{Hu}}{VD\\\\\\\%_{Hu}}{VD\%_{Hu}} value
#' indicates a more diverse core set.
#'
#' \mjtdeqn{MD\\\%_{Kim} = \left ( \frac{1}{n}\sum_{i=1}^{n} \frac{\left |
#' M_{EC_{i}}-M_{CS_{i}} \right |}{M_{CS_{i}}} \right ) \times
#' 100}{MD\\\\\\\%_{Kim} = \left ( \frac{1}{n}\sum_{i=1}^{n} \frac{\left |
#' M_{EC_{i}}-M_{CS_{i}} \right |}{M_{CS_{i}}} \right ) \times 100}{MD\%_{Kim} =
#' \left ( \frac{1}{n}\sum_{i=1}^{n} \frac{\left | M_{EC_{i}}-M_{CS_{i}} \right
#' |}{M_{CS_{i}}} \right ) \times 100}
#'
#' Where, \mjseqn{M_{EC_{i}}} is the mean of the EC for the \mjseqn{i}th trait,
#' \mjseqn{M_{CS_{i}}} is the mean of the CS for the \mjseqn{i}th trait and
#' \mjseqn{n} is the total number of traits.
#'
#' \mjtdeqn{VD\\\%_{Kim} = \left ( \frac{1}{n}\sum_{i=1}^{n} \frac{\left |
#' V_{EC_{i}}-V_{CS_{i}} \right |}{V_{CS_{i}}} \right ) \times
#' 100}{VD\\\\\\\%_{Kim} = \left ( \frac{1}{n}\sum_{i=1}^{n} \frac{\left |
#' V_{EC_{i}}-V_{CS_{i}} \right |}{V_{CS_{i}}} \right ) \times 100}{VD\%_{Kim} =
#' \left ( \frac{1}{n}\sum_{i=1}^{n} \frac{\left | V_{EC_{i}}-V_{CS_{i}} \right
#' |}{V_{CS_{i}}} \right ) \times 100}
#'
#' Where, \mjseqn{V_{EC_{i}}} is the variance of the EC for the \mjseqn{i}th
#' trait, \mjseqn{V_{CS_{i}}} is the variance of the CS for the \mjseqn{i}th
#' trait and \mjseqn{n} is the total number of traits.
#'
#' \mjtdeqn{\overline{d}D\\\% =
#' \frac{\overline{d}_{CS}-\overline{d}_{EC}}{\overline{d}_{EC}} \times
#' 100}{\overline{d}D\\\\\\\% =
#' \frac{\overline{d}_{CS}-\overline{d}_{EC}}{\overline{d}_{EC}} \times
#' 100}{\overline{d}D\\% =
#' \frac{\overline{d}_{CS}-\overline{d}_{EC}}{\overline{d}_{EC}} \times 100}
#'
#' Where, \mjseqn{\overline{d}_{CS}} is the mean squared Euclidean distance
#' among accessions in the CS and \mjseqn{\overline{d}_{EC}} is the mean squared
#' Euclidean distance among accessions in the EC.
#'
#' @inheritParams snk.evaluate.core
#' @param alpha Type I error probability (Significance level) of difference.
#'
#' @return A data frame with the values of
#'   \mjteqn{MD\\\%_{Hu}}{MD\\\\\\\%_{Hu}}{MD\%_{Hu}},
#'   \mjteqn{VD\\\%_{Hu}}{VD\\\\\\\%_{Hu}}{VD\%_{Hu}},
#'   \mjteqn{MD\\\%_{Kim}}{MD\\\\\\\%_{Kim}}{MD\%_{Kim}},
#'   \mjteqn{VD\\\%_{Kim}}{VD\\\\\\\%_{Kim}}{VD\%_{Kim}} and
#'   \mjteqn{\overline{d}D\\\%}{\overline{d}D\\\\\\\%}{\overline{d}D\%}.
#'
#' @seealso \code{\link[EvaluateCore]{snk.evaluate.core}},
#'   \code{\link[EvaluateCore]{snk.evaluate.core}}
#'
#' @importFrom cluster daisy
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
#' percentdiff.evaluate.core(data = ec, names = "genotypes",
#'                           quantitative = quant, selected = core)
#'
percentdiff.evaluate.core <- function(data, names, quantitative,
                              selected, alpha = 0.05) {
  # Checks
  checks.evaluate.core(data = data, names = names,
                       quantitative = quantitative,
                       selected = selected)

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  # Check alpha value
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha < 1)')
  }

  dataf <- data[, c(names, quantitative)]

  datafcore <- dataf[dataf[, names] %in% selected, ]

  dataf$`[Type]` <- "EC"
  datafcore$`[Type]` <- "CS"

  dataf <- rbind(dataf, datafcore)
  rm(datafcore)

  dataf$`[Type]` <- as.factor(dataf$`[Type]`)

  d_EC <- mean(cluster::daisy(dataf[dataf$`[Type]` == "EC", quantitative],
                              metric = "euclidean"))
  d_CS <- mean(cluster::daisy(dataf[dataf$`[Type]` == "CS", quantitative],
                              metric = "euclidean"))

  mdiff <- snk.evaluate.core(data, names, quantitative, selected)
  vdiff <- levene.evaluate.core(data, names, quantitative, selected)

  outdf <- data.frame(MDPercent_Hu =
                        (sum(mdiff$SNK_pvalue <= alpha) /
                           length(quantitative)) * 100,
                      VDPercent_Hu =
                        (sum(vdiff$Levene_pvalue <= alpha) /
                           length(quantitative)) * 100,
                      MDPercent_Kim =
                        (sum(abs(mdiff$EC_Mean - mdiff$CS_Mean) /
                               mdiff$CS_Mean) / length(quantitative)) * 100,
                      VDPercent_Kim =
                        (sum(abs(vdiff$EC_V - vdiff$CS_V) /
                               vdiff$CS_V) / length(quantitative)) * 100,
                      DDPercent = ((d_CS - d_EC) / d_EC) * 100)

  return(outdf)

}

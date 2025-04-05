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


#' Diversity Indices
#'
#' Compute the following diversity indices and perform corresponding statistical
#' tests to compare the phenotypic diversity for qualitative traits between
#' entire collection (EC) and core set (CS). \itemize{ \item{Simpson's and
#' related indices} \itemize{ \item{Simpson's Index (\mjseqn{d})
#' \insertCite{simpson_measurement_1949,peet_measurement_1974}{EvaluateCore}}
#' \item{Simpson's Index of Diversity or Gini's Diversity Index or Gini-Simpson
#' Index or Nei's Diversity Index or Nei's Variation Index (\mjseqn{D})
#' \insertCite{gini_variabilita_1912,gini_variabilita_1912-2,greenberg_measurement_1956,berger_diversity_1970,nei_analysis_1973,peet_measurement_1974}{EvaluateCore}}
#' \item{Maximum Simpson's Index of Diversity or Maximum Nei's
#' Diversity/Variation Index (\mjseqn{D_{max}})
#' \insertCite{hennink_interpretation_1990}{EvaluateCore}} \item{Simpson's
#' Reciprocal Index or Hill's \mjseqn{N_{2}} (\mjseqn{D_{R}})
#' \insertCite{williams_patterns_1964,hill_diversity_1973}{EvaluateCore}}
#' \item{Relative Simpson's Index of Diversity or Relative Nei's
#' Diversity/Variation Index (\mjseqn{D'})
#' \insertCite{hennink_interpretation_1990}{EvaluateCore}} }
#' \item{Shannon-Weaver and related indices} \itemize{ \item{Shannon or
#' Shannon-Weaver or Shannon-Weiner Diversity Index (\mjseqn{H})
#' \insertCite{shannon_mathematical_1949,peet_measurement_1974}{EvaluateCore}}
#' \item{Maximum Shannon-Weaver Diversity Index (\mjseqn{H_{max}})
#' \insertCite{hennink_interpretation_1990}{EvaluateCore}} \item{Relative
#' Shannon-Weaver Diversity Index or Shannon Equitability Index (\mjseqn{H'})
#' \insertCite{hennink_interpretation_1990}{EvaluateCore}} } \item{McIntosh
#' Diversity Index} \itemize{ \item{McIntosh Diversity Index (\mjseqn{D_{Mc}})
#' \insertCite{mcintosh_index_1967,peet_measurement_1974}{EvaluateCore}} } }
#' \loadmathjax
#'
#' @section Details: The diversity indices and the corresponding statistical
#'   tests implemented in \code{diversity.evaluate.core} are as follows.
#'
#'   \subsection{Simpson's and related indices}{Simpson's index (\mjseqn{d})
#'   which estimates the probability that two accessions randomly selected will
#'   belong to the same phenotypic class of a trait, is computed as follows
#'   \insertCite{simpson_measurement_1949,peet_measurement_1974}{EvaluateCore}.
#'
#'   \mjsdeqn{d = \sum_{i = 1}^{k}p_{i}^{2}}
#'
#'   Where, \mjseqn{p_{i}} denotes the proportion/fraction/frequency of
#'   accessions in the \mjseqn{i}th phenotypic class for a trait and \mjseqn{k}
#'   is the number of phenotypic classes for the trait.
#'
#'   The value of \mjseqn{d} can range from 0 to 1 with 0 representing maximum
#'   diversity and 1, no diversity.
#'
#'   \mjseqn{d} is subtracted from 1 to give Simpson's index of diversity
#'   (\mjseqn{D})
#'   \insertCite{greenberg_measurement_1956,berger_diversity_1970,peet_measurement_1974,hennink_interpretation_1990}{EvaluateCore}
#'    originally suggested by
#'   \insertCite{gini_variabilita_1912,gini_variabilita_1912-2;textual}{EvaluateCore}
#'    and described in literature as Gini's diversity index or Gini-Simpson
#'   index. It is the same as Nei's diversity index or Nei's variation index
#'   \insertCite{nei_analysis_1973,hennink_interpretation_1990}{EvaluateCore}.
#'   Greater the value of \mjseqn{D}, greater the diversity with a range from 0
#'   to 1.
#'
#'   \mjsdeqn{D = 1 - d}
#'
#'   The maximum value of \mjseqn{D}, \mjseqn{D_{max}} occurs when accessions
#'   are uniformly distributed across the phenotypic classes and is computed as
#'   follows \insertCite{hennink_interpretation_1990}{EvaluateCore}.
#'
#'   \mjsdeqn{D_{max} = 1 - \frac{1}{k}}
#'
#'   Reciprocal of \mjseqn{d} gives the Simpson's reciprocal index
#'   (\mjseqn{D_{R}})
#'   \insertCite{williams_patterns_1964,hennink_interpretation_1990}{EvaluateCore}
#'    and can range from 1 to \mjseqn{k}. This was also described in
#'   \insertCite{hill_diversity_1973;textual}{EvaluateCore} as (\mjseqn{N_{2}}).
#'
#'   \mjsdeqn{D_{R} = \frac{1}{d}}
#'
#'   Relative Simpson's index of diversity or Relative Nei's diversity/variation
#'   index (\mjseqn{H'}) \insertCite{hennink_interpretation_1990}{EvaluateCore}
#'   is defined as follows \insertCite{peet_measurement_1974}{EvaluateCore}.
#'
#'   \mjsdeqn{D' = \frac{D}{D_{max}}}
#'
#'   Differences in Simpson's diversity index for qualitative traits of EC and
#'   CS can be tested by a t-test using the associated variance estimate
#'   described in \insertCite{simpson_measurement_1949;textual}{EvaluateCore}
#'   \insertCite{lyons_c20_1978}{EvaluateCore}.
#'
#'   The t statistic is computed as follows.
#'
#'   \mjsdeqn{t = \frac{d_{EC} - d_{CS}}{\sqrt{V_{d_{EC}} + V_{d_{CS}}}}}
#'
#'   Where, the variance of \mjseqn{d} (\mjseqn{V_{d}}) is,
#'
#'   \mjsdeqn{V_{d} = \frac{4N(N-1)(N-2)\sum_{i=1}^{k}(p_{i})^{3} +
#'   2N(N-1)\sum_{i=1}^{k}(p_{i})^{2} - 2N(N-1)(2N-3) \left(
#'   \sum_{i=1}^{k}(p_{i})^{2} \right)^{2}}{[N(N-1)]^{2}}}
#'
#'   The associated degrees of freedom is computed as follows.
#'
#'   \mjsdeqn{df = (k_{EC} - 1) + (k_{CS} - 1)}
#'
#'   Where, \mjseqn{k_{EC}} and \mjseqn{k_{CS}} are the number of phenotypic
#'   classes in the trait for EC and CS respectively.
#'
#'   }
#'
#'   \subsection{Shannon-Weaver and related indices}{An index of information
#'   \mjseqn{H}, was described by
#'   \insertCite{shannon_mathematical_1949;textual}{EvaluateCore} as follows.
#'
#'   \mjsdeqn{H = -\sum_{i=1}^{k}p_{i} \log_{2}(p_{i})}
#'
#'   \mjseqn{H} is described as Shannon or Shannon-Weaver or Shannon-Weiner
#'   diversity index in literature.
#'
#'   Alternatively, \mjseqn{H} is also computed using natural logarithm instead
#'   of logarithm to base 2.
#'
#'   \mjsdeqn{H = -\sum_{i=1}^{k}p_{i} \ln(p_{i})}
#'
#'   The maximum value of \mjseqn{H} (\mjseqn{H_{max}}) is \mjseqn{\ln(k)}. This
#'   value occurs when each phenotypic class for a trait has the same proportion
#'   of accessions.
#'
#'   \mjsdeqn{H_{max} = \log_{2}(k)\;\; \textrm{OR} \;\; H_{max} = \ln(k)}
#'
#'   The relative Shannon-Weaver diversity index or Shannon equitability index
#'   (\mjseqn{H'}) is the Shannon diversity index (\mjseqn{I}) divided by the
#'   maximum diversity (\mjseqn{H_{max}}).
#'
#'   \mjsdeqn{H' = \frac{H}{H_{max}}}
#'
#'   Differences in Shannon-Weaver diversity index for qualitative traits of EC
#'   and CS can be tested by Hutcheson t-test
#'   \insertCite{hutcheson_test_1970}{EvaluateCore}.
#'
#'   The Hutcheson t statistic is computed as follows.
#'
#'   \mjsdeqn{t = \frac{H_{EC} - H_{CS}}{\sqrt{V_{H_{EC}} + V_{H_{CS}}}}}
#'
#'   Where, the variance of \mjseqn{H} (\mjseqn{V_{H}}) is,
#'
#'   \mjsdeqn{V_{H} = \frac{\sum_{i=1}^{k}n_{i}(\log_{2}{n_{i}})^{2}
#'   \frac{(\sum_{i=1}^{k}\log_{2}{n_{i}})^2}{N}}{N^{2}}}
#'
#'   \mjsdeqn{\textrm{OR}}
#'
#'   \mjsdeqn{V_{H} = \frac{\sum_{i=1}^{k}n_{i}(\ln{n_{i}})^{2}
#'   \frac{(\sum_{i=1}^{k}\ln{n_{i}})^2}{N}}{N^{2}}}
#'
#'   The associated degrees of freedom is approximated as follows.
#'
#'   \mjsdeqn{df = \frac{(V_{H_{EC}} +
#'   V_{H_{CS}})^{2}}{\frac{V_{H_{EC}}^{2}}{N_{EC}} +
#'   \frac{V_{H_{CS}}^{2}}{N_{CS}}}}
#'
#'   }
#'
#'   \subsection{McIntosh Diversity Index}{A similar index of diversity was
#'   described by \insertCite{mcintosh_index_1967;textual}{EvaluateCore} as
#'   follows (\mjseqn{D_{Mc}}) \insertCite{peet_measurement_1974}{EvaluateCore}.
#'
#'   \mjsdeqn{D_{Mc} = \frac{N - \sqrt{\sum_{i=1}^{k}n_{i}^2}}{N - \sqrt{N}}}
#'
#'   Where, \mjseqn{n_{i}} denotes the number of accessions in the \mjseqn{i}th
#'   phenotypic class for a trait and \mjseqn{N} is the total number of
#'   accessions so that \mjseqn{p_{i} = {n_{i}}/{N}}.}
#'
#'   \subsection{Testing for difference with bootstrapping}{Bootstrap statistics
#'   are employed to test the difference between the Simpson, Shannon-Weaver and
#'   McIntosh indices for  qualitative traits of EC and CS
#'   \insertCite{solow_simple_1993}{EvaluateCore}.
#'
#'   If \mjseqn{I_{EC}} and \mjseqn{I_{CS}} are the diversity indices with the
#'   original number of accessions, then random samples of the same size as the
#'   original are repeatedly generated (with replacement) \mjseqn{R} times and
#'   the corresponding diversity index is computed for each sample.
#'
#'   \mjsdeqn{I_{EC}^{*} = \lbrace H_{EC_{1}}, H_{EC_{}}, \cdots, H_{EC_{R}}
#'   \rbrace}
#'
#'   \mjsdeqn{I_{CS}^{*} = \lbrace H_{CS_{1}}, H_{CS_{}}, \cdots, H_{CS_{R}}
#'   \rbrace}
#'
#'   Then the bootstrap null sample {\mjseqn{I_{0}}} is computed as follows.
#'
#'   \mjsdeqn{\Delta^{*} = I_{EC}^{*} - I_{CS}^{*}}
#'
#'   \mjsdeqn{I_{0} = \Delta^{*} - \overline{\Delta^{*}}}
#'
#'   Where, \mjseqn{\overline{\Delta^{*}}} is the mean of \mjseqn{\Delta^{*}}.
#'
#'   Now the original difference in diversity indices (\mjseqn{\Delta_{0} =
#'   I_{EC} - I_{CS}}) is tested against mean of bootstrap null sample
#'   (\mjseqn{I_{0}}) by a z test. The z score test statistic is computed as
#'   follows.
#'
#'   \mjsdeqn{z = \frac{\Delta_{0} - \overline{H_{0}}}{\sqrt{V_{H_{0}}}}}
#'
#'   Where,  \mjseqn{\overline{H_{0}}} and \mjseqn{V_{H_{0}}} are the mean and
#'   variance of the bootstrap null sample \mjseqn{H_{0}}.
#'
#'   The corresponding degrees of freedom is estimated as follows.
#'
#'   \mjsdeqn{df = (k_{EC} - 1) + (k_{CS} - 1)}
#'
#'   }
#'
#' @inheritParams chisquare.evaluate.core
#' @param base The logarithm base to be used for computation of Shannon-Weaver
#'   Diversity Index (\mjseqn{I}). Default is 2.
#' @param R The number of bootstrap replicates. Default is 1000.
#'
#' @return A list with three data frames as follows. \item{simpson}{
#'   \describe{\item{Trait}{The qualitative trait.}
#'   \item{EC_No.Classes}{The number of classes in the trait for EC.}
#'   \item{CS_No.Classes}{The number of classes in the trait for CS.}
#'   \item{EC_d}{The Simpson's Index (\mjseqn{d}) for EC.}
#'   \item{EC_D}{The Simpson's Index of Diversity (\mjseqn{D}) for EC.}
#'   \item{EC_D.max}{The Maximum Simpson's Index of Diversity
#'   (\mjseqn{D_{max}}) for EC.} \item{EC_D.inv}{The Simpson's Reciprocal
#'   Index (\mjseqn{D_{R}}) for EC.} \item{EC_D.rel}{The Relative
#'   Reciprocal Index (\mjseqn{D'}) for EC.} \item{EC_d.V}{The variance
#'   of \mjseqn{d} for EC according to
#'   \insertCite{simpson_measurement_1949}{EvaluateCore}.}
#'   \item{EC_d.boot.V}{The bootstrap variance of \mjseqn{d} for EC.}
#'   \item{CS_d}{The Simpson's Index (\mjseqn{d}) for CS.}
#'   \item{CS_D}{The Simpson's Index of Diversity (\mjseqn{D}) for CS.}
#'   \item{CS_D.max}{The Maximum Simpson's Index of Diversity
#'   (\mjseqn{D_{max}}) for CS.} \item{CS_D.inv}{The Simpson's Reciprocal
#'   Index (\mjseqn{D_{R}}) for CS.} \item{CS_D.rel}{The Relative
#'   Reciprocal Index (\mjseqn{D'}) for CS.} \item{CS_d.V}{The variance
#'   of \mjseqn{d} for CS according to
#'   \insertCite{simpson_measurement_1949}{EvaluateCore}.}
#'   \item{CS_d.boot.V}{The bootstrap variance of \mjseqn{d} for CS.}
#'   \item{d.t.df}{The degrees of freedom for t test.}
#'   \item{d.t.stat}{The t statistic.} \item{d.t.pvalue}{The p
#'   value for t test.} \item{d.t.significance}{The  significance of t
#'   test for t-test} \item{d.boot.z.df}{The degrees of freedom for
#'   bootstrap z score.} \item{d.boot.z.stat}{The bootstrap z score.}
#'   \item{d.boot.z.pvalue}{The p value of z score.}
#'   \item{d.boot.z.significance}{The significance of z score.}} }
#'   \item{shannon}{ \describe{\item{Trait}{The qualitative
#'   trait.} \item{EC_No.Classes}{The number of classes in the trait for
#'   EC.} \item{CS_No.Classes}{The number of classes in the trait for
#'   CS.} \item{EC_I}{The Shannon-Weaver Diversity Index (\mjseqn{I}) for
#'   EC.} \item{EC_I.max}{The Maximum Shannon-Weaver Diversity Index
#'   (\mjseqn{I_{max}}) for EC.} \item{EC_I.rel}{The Relative
#'   Shannon-Weaver Diversity Index (\mjseqn{I'}) for EC.}
#'   \item{EC_I.V}{The variance of \mjseqn{I} for EC according to
#'   \insertCite{hutcheson_test_1970}{EvaluateCore}.}
#'   \item{EC_I.boot.V}{The bootstrap variance of \mjseqn{I} for EC.}
#'   \item{CS_I}{The Shannon-Weaver Diversity Index (\mjseqn{I}) for CS.}
#'   \item{CS_I.max}{The Maximum Shannon-Weaver Diversity Index
#'   (\mjseqn{I_{max}}) for CS.} \item{CS_I.rel}{The Relative
#'   Shannon-Weaver Diversity Index (\mjseqn{I'}) for CS.}
#'   \item{CS_I.V}{The variance of \mjseqn{I} for CS according to
#'   \insertCite{hutcheson_test_1970}{EvaluateCore}.}
#'   \item{CS_I.boot.V}{The bootstrap variance of \mjseqn{I} for CS.}
#'   \item{I.t.stat}{The t statistic.} \item{I.t.df}{The degrees
#'   of freedom for t test.} \item{I.t.pvalue}{The p value for t test.}
#'   \item{I.t.significance}{The  significance of t test for t-test}
#'   \item{I.boot.z.df}{The degrees of freedom for bootstrap z score.}
#'   \item{I.boot.z.stat}{The bootstrap z score.}
#'   \item{I.boot.z.pvalue}{The p value of z score.}
#'   \item{I.boot.z.significance}{The significance of z score.}} }
#'   \item{mcintosh}{ \describe{\item{EC_No.Classes}{The number of
#'   classes in the trait for EC.} \item{CS_No.Classes}{The number of
#'   classes in the trait for CS.} \item{EC_D.Mc}{The McIntosh Index
#'   (\mjseqn{D_{Mc}}) for EC.} \item{CS_D.Mc}{The McIntosh Index
#'   (\mjseqn{D_{Mc}}) for CS.} \item{M.boot.z.stat}{The bootstrap z
#'   score.} \item{M.boot.z.df}{The degrees of freedom for bootstrap z
#'   score.} \item{M.boot.z.pvalue}{The p value of z score.}
#'   \item{M.boot.z.significance}{The significance of z score.}} }
#'
#' @seealso \code{\link[psych:misc]{shannon}}, \code{\link[vegan]{diversity}},
#'   \code{\link[boot]{boot}}
#'
#' @importFrom boot boot
#' @importFrom stats pt
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
#' \donttest{
#' diversity.evaluate.core(data = ec, names = "genotypes",
#'                         qualitative = qual, selected = core)
#' }
#'
diversity.evaluate.core <- function(data, names, qualitative, selected,
                                    base = 2, R = 1000) {

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

  div.indices <- lapply(dataf[, !colnames(dataf) %in% c(names, "[Type]")],
                        function(x) {
                          diversity.test(ECx = x[dataf$`[Type]` == "EC"],
                                         CSx = x[dataf$`[Type]` == "CS"],
                                         base = base, R = R)
                        })

  simpson <- do.call(rbind,
                     lapply(div.indices,
                            function(x) {x[["simpson"]]}))
  shannon <- do.call(rbind,
                     lapply(div.indices,
                            function(x) {x[["shannon"]]}))
  mcintosh <- do.call(rbind,
                     lapply(div.indices,
                            function(x) {x[["mcintosh"]]}))

  out <- list(simpson = data.frame(simpson),
              shannon = data.frame(shannon),
              mcintosh = data.frame(mcintosh))

  out <- lapply(out, function(x) {
    x$Trait <- rownames(x)
    rownames(x) <- NULL
    cols <- setdiff(colnames(x), "Trait")
    x <- x[, c("Trait", cols)]
  })

  return(out)

}


diversity.test <- function(ECx, CSx, base = 2, R = 1000) {
  if (is.factor(ECx) && is.factor(CSx)) {

    x1 <- droplevels(ECx)
    x2 <- droplevels(CSx)

    count1 <- as.vector(table(x1))
    total.count1 <- sum(count1, na.rm = TRUE)

    count2 <- as.vector(table(x2))
    total.count2 <- sum(count2, na.rm = TRUE)

    k1 <- length(count1)
    k2 <- length(count2)

    prob1 <- count1 / total.count1
    prob2 <- count2 / total.count2

    total.count <- total.count1 + total.count2

    # Simpson's Index (d)
    #---------------------------------------------------------------------------

    EC_d <- sum(prob1 ^ 2)
    CS_d <- sum(prob2 ^ 2)

    # EC_d <- sum(count1 * (count1 - 1)) / (total.count1 * (total.count1 - 1))
    # CS_d <- sum(count2 * (count2 - 1)) / (total.count2 * (total.count2 - 1))

    # Simpson's Index of Diversity
    #---------------------------------------------------------------------------

    EC_D <- 1 - EC_d
    CS_D <- 1 - CS_d

    # Max Simpson's index of diversity
    #---------------------------------------------------------------------------

    EC_D.max <- 1 - (1 / k1)
    CS_D.max <- 1 - (1 / k2)

    # Reciprocal Simpson's Index
    #---------------------------------------------------------------------------

    EC_D.inv <- 1 / EC_d
    CS_D.inv <- 1 / CS_d

    # Relative Simpson's Index
    #---------------------------------------------------------------------------

    EC_D.rel <- EC_D / EC_D.max
    CS_D.rel <- CS_D / CS_D.max

    # t-test for Simpson's Index
    #---------------------------------------------------------------------------

    EC_d.V <- ((4 * total.count1 * (total.count1 - 1) *
                  (total.count1 - 2) * sum(prob1 ^ 3)) +
                 (2 * total.count1 * (total.count1 - 1) * sum(prob1 ^ 2)) -
                 (2 * total.count1 * (total.count1 - 1) *
                    ((2 * total.count1) - 3) * (sum(prob1 ^ 2) ^ 2))) /
      ((total.count1 * (total.count1 - 1)) ^ 2)
    CS_d.V <- ((4 * total.count2 * (total.count2 - 1) *
                  (total.count2 - 2) * sum(prob2 ^ 3)) +
                 (2 * total.count2 * (total.count2 - 1) * sum(prob2 ^ 2)) -
                 (2 * total.count2 * (total.count2 - 1) *
                    ((2 * total.count2) - 3) * (sum(prob2 ^ 2) ^ 2))) /
      ((total.count2 * (total.count2 - 1)) ^ 2)

    # EC_d.V <- (4 / total.count1) * (sum(prob2 ^ 3) - (sum(prob2 ^ 2) ^ 2))
    # CS_d.V <- (4 / total.count2) * (sum(prob2 ^ 3) - (sum(prob2 ^ 2) ^ 2))

    # d.t.df <- (total.count1 - 1) + (total.count2 - 1)
    d.t.df <- (k1 - 1) + (k2 - 1)
    d.t.stat <- (EC_d - CS_d) / (sqrt(EC_d.V + CS_d.V))
    d.t.pvalue <- 2 * pt(-abs(d.t.stat), d.t.df)

    # Bootstrap test for Simpson's Index
    #---------------------------------------------------------------------------

    simpson.boot <- function(data, i) {
      data <- data[i]
      data <- droplevels(data)

      count <- as.vector(table(data))
      total.count <- sum(count, na.rm = TRUE)

      prob <- count / total.count

      sum(prob ^ 2)
    }

    EC_d.boot <- boot(data = x1,
                      statistic = simpson.boot,
                      R = 1000)

    CS_d.boot <-  boot(data = x2,
                       statistic = simpson.boot,
                       R = 1000)

    EC_d.boot.V <- as.vector(var(EC_d.boot$t))
    CS_d.boot.V <- as.vector(var(CS_d.boot$t))

    d.diff0 <- abs(EC_d.boot$t0 - CS_d.boot$t0)
    d.diff <- EC_d.boot$t - CS_d.boot$t
    d.H0 <- d.diff - mean(d.diff)

    d.boot.z.stat <- (d.diff0 - mean(d.H0)) / sd(d.H0)
    # d.boot.z.df <- (total.count1 - 1) + (total.count2 - 1)
    d.boot.z.df <- (k1 - 1) + (k2 - 1)
    d.boot.z.pvalue <- 2 * pt(-abs(d.boot.z.stat), d.boot.z.df)

    # Shannon-Weaver Diversity Index (H)
    #---------------------------------------------------------------------------

    EC_I <- - sum(prob1 * log(prob1, base = base))
    CS_I <- - sum(prob2 * log(prob2, base = base))

    # Maximum Shannon-Weaver Diversity Index
    #---------------------------------------------------------------------------

    EC_I.max <- log(k1)
    CS_I.max <- log(k2)

    # Relative Shannon-Weaver Diversity Index
    #---------------------------------------------------------------------------

    EC_I.rel <- EC_I / EC_I.max
    CS_I.rel <- CS_I / CS_I.max

    # Hutcheson t-test (SShannon-Weaver Index)
    #---------------------------------------------------------------------------

    EC_I.V <- ((sum(count1 * ((log(count1, base = base)) ^ 2))) -
                 (((sum(count1 * log(count1, base = base))) ^ 2) /
                    total.count1)) /
      (total.count1 ^ 2)
    CS_I.V <- ((sum(count2 * ((log(count2, base = base)) ^ 2))) -
                 (((sum(count2 * log(count2, base = base))) ^ 2) /
                    total.count2)) /
      (total.count2 ^ 2)

    I.t.stat <- (EC_I - CS_I) / (sqrt(EC_I.V + CS_I.V))
    I.t.df <- ((EC_I.V + CS_I.V) ^ 2) /
      (((EC_I.V ^ 2) / total.count1) + ((CS_I.V ^ 2) / total.count2))
    I.t.pvalue <- 2 * pt(-abs(I.t.stat), I.t.df)


    # Bootstrap test (Shannon Weaver Index)
    #---------------------------------------------------------------------------

    shannon.boot <- function(data, i, base) {
      data <- data[i]
      data <- droplevels(data)

      count <- as.vector(table(data))
      total.count <- sum(count, na.rm = TRUE)

      prob <- count / total.count

      - sum(prob * log(prob, base = base))
    }

    EC_I.boot <- boot(data = x1,
                    statistic = shannon.boot,
                    R = 1000,
                    base = base)

    CS_I.boot <-  boot(data = x2,
                     statistic = shannon.boot,
                     R = 1000,
                     base = base)

    EC_I.boot.V <- as.vector(var(EC_I.boot$t))
    CS_I.boot.V <- as.vector(var(CS_I.boot$t))

    I.diff0 <- abs(EC_I.boot$t0 - CS_I.boot$t0)
    I.diff <- EC_I.boot$t - CS_I.boot$t
    I.H0 <- I.diff - mean(I.diff)

    I.boot.z.stat <- (I.diff0 - mean(I.H0)) / sd(I.H0)
    # I.boot.z.df <- (total.count1 - 1) + (total.count2 - 1)
    I.boot.z.df <- (k1 - 1) + (k2 - 1)
    I.boot.z.pvalue <- 2 * pt(-abs(I.boot.z.stat), I.boot.z.df)

    # McIntosh Index
    #---------------------------------------------------------------------------

    EC_D.Mc <- (total.count1 - sqrt(sum(count1 ^ 2))) /
      (total.count1 - sqrt(total.count1))
    CS_D.Mc <- (total.count2 - sqrt(sum(count2 ^ 2))) /
      (total.count2 - sqrt(total.count2))

    # Bootstrap test (McIntosh Index)
    #---------------------------------------------------------------------------

    mcintosh.boot <- function(data, i) {
      data <- data[i]
      data <- droplevels(data)

      count <- as.vector(table(data))
      total.count <- sum(count, na.rm = TRUE)

      (total.count - sqrt(sum(count ^ 2))) /
        (total.count - sqrt(total.count))
    }

    EC_M.boot <- boot(data = x1,
                      statistic = mcintosh.boot,
                      R = 1000)

    CS_M.boot <- boot(data = x2,
                      statistic = mcintosh.boot,
                      R = 1000)

    M.diff0 <- abs(EC_M.boot$t0 - CS_M.boot$t0)
    M.diff <- EC_M.boot$t - CS_M.boot$t
    M.H0 <- M.diff - mean(M.diff)

    M.boot.z.stat <- (M.diff0 - mean(M.H0)) / sd(M.H0)
    # M.boot.z.df <- (total.count1 - 1) + (total.count2 - 1)
    M.boot.z.df <- (k1 - 1) + (k2 - 1)
    M.boot.z.pvalue <- 2 * pt(-abs(M.boot.z.stat), M.boot.z.df)

    #---------------------------------------------------------------------------

    out <- list(simpson = c(EC_No.Classes = k1,
                            CS_No.Classes = k2,
                            EC_d = EC_d,
                            EC_D = EC_D,
                            EC_D.max = EC_D.max,
                            EC_D.inv = EC_D.inv,
                            EC_D.rel = EC_D.rel,
                            EC_d.V = EC_d.V,
                            EC_d.boot.V = EC_d.boot.V,
                            CS_d = CS_d,
                            CS_D = CS_D,
                            CS_D.max = CS_D.max,
                            CS_D.inv = CS_D.inv,
                            CS_D.rel = CS_D.rel,
                            CS_d.V = CS_d.V,
                            CS_d.boot.V = CS_d.boot.V,
                            d.t.df = d.t.df,
                            d.t.stat = d.t.stat,
                            d.t.pvalue = d.t.pvalue,
                            d.t.significance = ifelse(d.t.pvalue <= 0.01, "**",
                                                      ifelse(d.t.pvalue <= 0.05,
                                                             "*", "ns")),
                            d.boot.z.df = d.boot.z.df,
                            d.boot.z.stat = d.boot.z.stat,
                            d.boot.z.pvalue = d.boot.z.pvalue,
                            d.boot.z.significance = ifelse(d.boot.z.pvalue <= 0.01,
                                                           "**",
                                                           ifelse(d.boot.z.pvalue <= 0.05,
                                                                  "*", "ns"))),
                shannon = c(EC_No.Classes = k1,
                            CS_No.Classes = k2,
                            EC_I = EC_I,
                            EC_I.max = EC_I.max,
                            EC_I.rel = EC_I.rel,
                            EC_I.V = EC_I.V,
                            EC_I.boot.V = EC_I.boot.V,
                            CS_I = CS_I,
                            CS_I.max = CS_I.max,
                            CS_I.rel = CS_I.rel,
                            CS_I.V = CS_I.V,
                            CS_I.boot.V = CS_I.boot.V,
                            I.t.stat = I.t.stat,
                            I.t.df = I.t.df,
                            I.t.pvalue = I.t.pvalue,
                            I.t.significance = ifelse(I.t.pvalue <= 0.01, "**",
                                                      ifelse(I.t.pvalue <= 0.05,
                                                             "*", "ns")),
                            I.boot.z.df = I.boot.z.df,
                            I.boot.z.stat = I.boot.z.stat,
                            I.boot.z.pvalue = I.boot.z.pvalue,
                            I.boot.z.significance = ifelse(I.boot.z.pvalue <= 0.01, "**",
                                                      ifelse(I.boot.z.pvalue <= 0.05,
                                                             "*", "ns"))),
                mcintosh = c(EC_No.Classes = k1,
                             CS_No.Classes = k2,
                             EC_D.Mc = EC_D.Mc,
                             CS_D.Mc = CS_D.Mc,
                             M.boot.z.stat = M.boot.z.stat,
                             M.boot.z.df = M.boot.z.df,
                             M.boot.z.pvalue = M.boot.z.pvalue,
                             M.boot.z.significance = ifelse(M.boot.z.pvalue <= 0.01, "**",
                                                            ifelse(M.boot.z.pvalue <= 0.05,
                                                                   "*", "ns"))))

    return(out)

  } else{
    stop('"ECx" and "CSx" should be vectors of type factor.')
  }
}

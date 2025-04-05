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


#' Shannon-Weaver Diversity Index
#'
#' Compute the Shannon-Weaver Diversity Index (\mjseqn{H'}), Maximum diversity
#' (\mjseqn{H'_{max}}) and Shannon Equitability Index (\mjseqn{E_{H}})
#' \insertCite{shannon_mathematical_1949}{EvaluateCore} to compare the
#' phenotypic diversity for qualitative traits between entire collection (EC)
#' and core set (CS). \loadmathjax
#'
#' Shannon-Weaver Diversity Index (\mjseqn{H'}) is computed as follows.
#'
#' \mjsdeqn{H' = -\sum_{i=1}^{k}p_{i} \ln(p_{i})}
#'
#' Where \mjseqn{p_{i}} denotes the proportion in the group \mjseqn{k}.
#'
#' The maximum value of the index (\mjseqn{H'_{max}}) is \mjseqn{\ln(k)}. This
#' value occurs when each group has the same frequency.
#'
#' The Shannon equitability index (\mjseqn{E_{H}}) is the Shannon diversity
#' index divided by the maximum diversity.
#'
#' \mjsdeqn{E_{H} = \frac{H'}{\ln{(k)}}}
#'
#'
#' @return A data frame with the following columns. \item{Trait}{The qualitative
#'   trait.} \item{EC_H}{The Shannon-Weaver Diversity Index (\mjseqn{H'}) for
#'   EC.} \item{EC_H}{The Shannon-Weaver Diversity Index (\mjseqn{H'}) for CS.}
#'   \item{EC_Hmax}{The Maximum diversity value (\mjseqn{H'_{max}}) for EC.}
#'   \item{CS_Hmax}{The Maximum diversity value (\mjseqn{H'_{max}}) for CS.}
#'   \item{EC_EH}{The Shannon Equitability Index (\mjseqn{E_{H}}) for EC.}
#'   \item{CS_EH}{The Shannon Equitability Index (\mjseqn{E_{H}}) for CS.}
#'
#' @seealso \code{\link[psych:misc]{shannon}}
#'
#' @importFrom psych shannon
#' @importFrom dplyr bind_rows
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
#' shannon.evaluate.core(data = ec, names = "genotypes",
#'                       qualitative = qual, selected = core)
#'
#' @name shannon.evaluate.core-deprecated
#' @usage shannon.evaluate.core(data, names, qualitative, selected)
#' @seealso \code{\link{EvaluateCore-deprecated}}
#' @keywords internal
NULL

#' @rdname EvaluateCore-deprecated
#' @section \code{shannon.evaluate.core}:
#' For \code{shannon.evaluate.core}, use \code{\link{diversity.evaluate.core}}.
#'
#' @export
shannon.evaluate.core <- function(data, names, qualitative, selected) {

  .Deprecated("diversity.evaluate.core",
              msg = c("`shannon.evaluate.core()` was deprecated in EvaluateCore 0.1.3.\n",
                      "Please use `diversity.evaluate.core()` instead."))

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

  # dataf[, qualitative] <- lapply(dataf[, qualitative], as.integer)

  EC_H <- psych::shannon(dataf[dataf$`[Type]` == "EC",
                               !colnames(dataf) %in% c(names, "[Type]")],
                         base =  exp(1), correct = FALSE)
  CS_H <- psych::shannon(dataf[dataf$`[Type]` == "CS",
                               !colnames(dataf) %in% c(names, "[Type]")],
                         base =  exp(1), correct = FALSE)

  EC_EH <- psych::shannon(dataf[dataf$`[Type]` == "EC",
                                !colnames(dataf) %in% c(names, "[Type]")],
                         base =  exp(1), correct = TRUE)
  CS_EH <- psych::shannon(dataf[dataf$`[Type]` == "CS",
                                !colnames(dataf) %in% c(names, "[Type]")],
                          base =  exp(1), correct = TRUE)

  EC_No.Classes <- unlist(lapply(dataf[dataf$`[Type]` == "EC",
                                       !colnames(dataf) %in% c(names,
                                                               "[Type]")],
                          function(x) length(levels(droplevels(x)))))
  CS_No.Classes <- unlist(lapply(dataf[dataf$`[Type]` == "CS",
                                       !colnames(dataf) %in% c(names,
                                                               "[Type]")],
                          function(x) length(levels(droplevels(x)))))

  outdf <- cbind(EC_H = EC_H[qualitative],
                 CS_H = CS_H[qualitative],
                 EC_Hmax = log(EC_No.Classes[qualitative]),
                 CS_Hmax = log(CS_No.Classes[qualitative]),
                 EC_EH = EC_EH[qualitative],
                 CS_EH = CS_EH[qualitative])
  outdf <- data.frame(outdf)

  outdf$Trait <- rownames(outdf)
  rownames(outdf) <- NULL
  cols <- setdiff(colnames(outdf), "Trait")
  outdf <- outdf[, c("Trait", cols)]

  return(outdf)

}

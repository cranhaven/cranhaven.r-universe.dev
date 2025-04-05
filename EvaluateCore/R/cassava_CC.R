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

#' IITA Cassava Germplasm Data - Core Collection
#'
#' An example germplasm characterisation data of a core collection generated
#' from 1591 accessions of IITA Cassava collection
#' \insertCite{benjamin_cassava_2019}{EvaluateCore} using 10 quantitative and 48
#' qualitative trait data with CoreHunter3
#' (\code{\link[corehunter]{corehunter}}). The core set was generated using
#' distance based measures giving equal weightage to Average
#' entry-to-nearest-entry distance (EN) and Average accession-to-nearest-entry
#' distance (AN). Includes data on 26 descriptors for 168 (10 \% of
#' \code{\link[EvaluateCore]{cassava_EC}}) accessions. It is used to demonstrate
#' the various functions of \code{EvaluateCore} package.
#'
#' Further details on how the example dataset was built from the original data
#' is available
#' \href{https://aravind-j.github.io/EvaluateCore/articles/additional/Example\%20Core\%20Data.html}{online}.
#'
#' @format A data frame with 58 columns: \describe{ \item{CUAL}{Colour of
#'   unexpanded apical leaves} \item{LNGS}{Length of stipules}
#'   \item{PTLC}{Petiole colour} \item{DSTA}{Distribution of anthocyanin}
#'   \item{LFRT}{Leaf retention} \item{LBTEF}{Level of branching at the end of
#'   flowering} \item{CBTR}{Colour of boiled tuberous root} \item{NMLB}{Number
#'   of levels of branching} \item{ANGB}{Angle of branching}
#'   \item{CUAL9M}{Colours of unexpanded apical leaves at 9 months}
#'   \item{LVC9M}{Leaf vein colour at 9 months} \item{TNPR9M}{Total number of
#'   plants remaining per accession at 9 months} \item{PL9M}{Petiole length at 9
#'   months} \item{STRP}{Storage root peduncle} \item{STRC}{Storage root
#'   constrictions} \item{PSTR}{Position of root} \item{NMSR}{Number of storage
#'   root per plant} \item{TTRN}{Total root number per plant} \item{TFWSR}{Total
#'   fresh weight of storage root per plant} \item{TTRW}{Total root weight per
#'   plant} \item{TFWSS}{Total fresh weight of storage shoot per plant}
#'   \item{TTSW}{Total shoot weight per plant} \item{TTPW}{Total plant weight}
#'   \item{AVPW}{Average plant weight} \item{ARSR}{Amount of rotted storage root
#'   per plant} \item{SRDM}{Storage root dry matter} }
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
#' data(cassava_CC)
#' summary(cassava_CC)
#'
#' quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
#'            "ARSR", "SRDM")
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#'
#' lapply(seq_along(cassava_CC[, qual]),
#'        function(i) barplot(table(cassava_CC[, qual][, i]),
#'                            xlab = names(cassava_CC[, qual])[i]))
#'
#' lapply(seq_along(cassava_CC[, quant]),
#'        function(i) hist(table(cassava_CC[, quant][, i]),
#'                         xlab = names(cassava_CC[, quant])[i],
#'                         main = ""))
#'
"cassava_CC"

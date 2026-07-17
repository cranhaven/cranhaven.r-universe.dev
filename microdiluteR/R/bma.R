#' @title Absorption values from six broth microdilution assays conducted on 96-well plates
#' @description A list of six sample data sets with absorption values from broth
#' microdilution assays on 96-well plates, applied to two groups with one experiment
#' each at two time points T0 and T3.
#' @details The data was derived from two broth microdilution assay experiments testing the
#' growth performance of \emph{Botrytis cinerea} conidia on two \emph{Tanacetum vulgare}
#' chemotypes (defined as \code{groups}). Leaf extracts from chemotypes were fractionated using
#' solid-phase extraction with a water-methanol polarity gradient (defined as \code{treatment})
#' and fractions were subjected to assays in two concentrations (100 ppm and 200 ppm) plus
#' a positive control for each concentration level. The 96-well plate design was assigned in
#' horizontal direction (provided as \code{plate_axis}) and is stored in the metadata attribute
#' of the list. The data was generated for teaching purposes and is unrestricted by any
#' licensing constraints.
#' @name bma
#' @format A list with six data frames:
#'  \describe{
#'      \item{bma_grp1_exp2_T0}{Absorption values from a broth microdilution assay
#'      applied on group 1 from experiment 2 on 96-well plate at timepoint T0.}
#'      \item{bma_grp1_exp2_T3}{Absorption values from a broth microdilution assay
#'      applied on group 1 from experiment 2 on 96-well plate at timepoint T3.}
#'      \item{bma_grp2_exp1_T0}{Absorption values from a broth microdilution assay
#'      applied on group 2 from experiment 1 on 96-well plate at timepoint T0.}
#'      \item{bma_grp2_exp1_T3}{Absorption values from a broth microdilution assay
#'      applied on group 2 from experiment 1 on 96-well plate at timepoint T3.}
#'      }
#' @examples
#' data(bma)
#' attr(bma, "metadata")
"bma"

#' Genetic Management Functions
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
###############################################################################
#' @description Primary Data Structure --- Pedigree
#'
#' Contains studbook information for a number of individuals.
#' ASSUME: All IDs listed in the sire or dam columns must have a row entry in
#'   the id column
#' @seealso \code{\link{getIncludeColumns}} to get set of columns that can
#' be used in a pedigree file
#'
#' A Pedigree is a data frame within the \code{R} environment with the following
#' possible columns:
#' \itemize{
#' \item\{id\} \{-- character vector with unique identifier for an individual\}
#' \item\{sire\} \{-- character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).\}
#' \item\{dam\} \{-- character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).\}
#' \item\{sex\} \{-- factor \{levels: "M", "F", "U"\} Sex specifier for an
#'  individual\}
#' \item\{gen\} \{-- integer vector with the generation number of the
#'  individual\}
#' \item\{birth\} \{-- Date or \code{NA} (optional) with the individual's birth
#' date\}
#' \item\{exit\} \{-- Date or \code{NA} (optional) with the individual's exit
#' date (death, or departure if applicable)\}
#' \item\{ancestry\} \{-- character vector or \code{NA} (optional) that
#' indicates the geographic population to which the individual belongs.\}
#' \item\{age\} \{-- numeric or \code{NA} (optional) indicating the individual's
#' current age or age at exit.\}
#' \item\{population\} \{-- logical (optional)
#' Is the id part of the extant population?\}
#' \item\{origin\} \{-- character vector or \code{NA} (optional) that indicates
#' the name of the facility that the individual was imported from if other than
#' local.\code{NA} indicates the individual was not imported.\}
#' }
#'
#' Pedigree File Testing Functions
#'
#' \itemize{
#' \item\{\link{qcStudbook}\} \{--- Main pedigree curation function that
#' performs basic quality control on pedigree information\}
#' \item\{\link{fixColumnNames}\} \{--- Changes original column names and into
#' standardized names.\}
#' \item\{\link{checkRequiredCols}\} \{--- Examines column names, cols, to see
#' if all required column names are present.\}
#' \item\{\link{correctParentSex}\} \{--- Sets sex for animals listed as either
#' a sire or dam.\}
#' \item\{\link{getDateErrorsAndConvertDatesInPed}\} \{--- Converts columns of
#' dates in text form to \code{Date} object columns\}
#' \item\{\link{checkParentAge}\} \{--- Check parent ages to be at least
#'  \code{minParentAge}\}
#' \item\{\link{removeDuplicates}\} \{--- Remove duplicate records from
#'  pedigree\}
#' }
#' Gene Dropping Function
#'
#' \itemize{
#' \item\{\link{geneDrop}\} \{--- Performs a gene drop simulation based on the
#' provided pedigree information\}
#' }
#' Genetic Value Analysis Functions
#'
#' Contains functions to calculate the kinship coefficient and genome
#' uniqueness for animals listed in a Pedigree table.
#' \itemize{
#' \item\{\link{meanKinship}\} \{--- Calculates the mean kinship for each animal
#' in a kinship matrix\}
#' \item\{\link{calcA}\} \{--- Calculates \code{a}, the number of an
#' individual's alleles that are rare in each simulation.\}
#' \item\{\link{alleleFreq}\} \{--- Calculates the count of each allele in the
#' provided vector.\}
#' \item\{\link{calcFE}\} \{--- Calculates founder equivalents.\}
#' \item\{\link{calcFG}\} \{--- Calculates founder genome equivalents.\}
#' \item\{\link{calcFEFG}\} \{--- Returns founder equivalents \code{FE} and
#' \code{FG} as elements in a list.\}
#' \item\{\link{calcGU}\} \{--- Calculates genome uniqueness for each ID that is
#' part of the population.\}
#' \item\{\link{geneDrop}\} \{--- Performs a gene drop simulation based on the
#' pedigree information.\}
#' \item\{\link{chooseAlleles}\} \{--- Combines two vectors of alleles by
#' randomly selecting one allele or the other at each position.\}
#' \item\{\link{calcRetention}\} \{--- Calculates allelic retention.\}
#' \item\{\link{filterKinMatrix}\} \{--- Filters a kinship matrix to include
#' only the egos listed in 'ids'\}
#' \item\{\link{kinship}\} \{--- Generates a kinship matrix\}
#' \item\{\link{reportGV}\} \{--- Generates a genetic value report for a
#' provided pedigree.\}
#' }
#' Plotting Functions
#'
#' \itemize{
#' \item\{\link{meanKinship}\} \{--- Calculates the mean kinship for each animal
#' in a kinship matrix\}
#' }
#' Breeding Group Formation Functions
#'
#' \itemize{
#' \item\{\link{meanKinship}\} \{--- Calculates the mean kinship for each animal
#' in a kinship matrix\}
#' }
#' @keywords internal
"_PACKAGE"
#' @name nprcgenekeepr
#' @title Genetic Tools for Colony Management
NULL

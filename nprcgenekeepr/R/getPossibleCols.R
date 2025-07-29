#' Get possible column names for a studbook.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Pedigree curation function
#'
#' @return A character vector of the possible columns that can be in a
#'          studbook. The possible columns are as follows:
#' \item{id}{ -- character vector with unique identifier for an individual}
#' \item{sire}{ -- character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).}
#' \item{dam}{ -- character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).}
#' \item{sex}{ -- factor (levels: "M", "F", "U") Sex specifier for an
#' individual}
#' \item{gen}{ -- integer vector with the generation number of the
#' individual}
#' \item{birth}{ -- Date or \code{N} (optional) with the individual's birth
#' date}
#' \item{exit}{ -- Date or \code{NA} (optional) with the individual's exit
#'  date
#' (death, or departure if applicable)}
#' \item{ancestry}{ -- character vector or \code{NA} (optional) that
#' indicates the geographic population to which the individual belongs.}
#' \item{age}{ -- numeric or \code{NA} (optional) indicating the individual's
#' current age or age at exit.}
#' \item{population}{ -- an optional logical argument indicating whether or
#' not the \code{id} is part of the extant population.}
#' \item{origin}{ -- character vector or \code{NA} (optional) that indicates
#' the name of the facility that the individual was imported from.
#' \code{NA} indicates the individual was not imported.}
#' \item{status}{ -- an optional factor indicating the status of an
#' individual with levels \code{ALIVE}, \code{DEAD}, and \code{SHIPPED}.}
#' \item{condition}{ --  character vector or \code{NA} (optional) that
#' indicates the restricted status of an animal. "Nonrestricted" animals
#' are generally assumed to be naive.}
#' \item{spf}{ -- character vector or \code{NA} (optional) indicating the
#' specific pathogen-free status of an individual.}
#' \item{vasxOvx}{ -- character vector indicating the vasectomy/ovariectomy
#' status of an animal where \code{NA} indicates an intact animal and all other
#' values indicate surgical alteration.}
#' \item{pedNum}{ -- integer vector indicating generation numbers for each
#' id, starting at 0 for individuals lacking IDs for both parents.}
#' @export
#' @examples
#' library(nprcgenekeepr)
#' getPossibleCols()
getPossibleCols <- function() {
  c(
    "id", "sire", "dam", "sex", "gen", "birth", "exit", "death", "age",
    "ancestry", "population", "origin", "status", "condition", "departure",
    "spf", "vasxOvx", "pedNum", "first", "second", "first_name",
    "second_name", "recordStatus"
  )
}

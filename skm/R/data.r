#------------------------------------------------------------------------------#
#--------------------------------- skm::data.r --------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ data ------------------------------------#
#------------------------------------------------------------------------------#
#' zip2012
#'
#' @description
#'  a zip code database with latitude, longitude, population and income.
#'
#' @format
#'  A data table with 28844 rows and 9 variables:
#' \describe{
#'   \item{zip}{zip code, 5 digits zip code in U.S.}
#'   \item{lat}{latitude}
#'   \item{lng}{longitude}
#'   \item{pop}{population}
#'   \item{ink}{income}
#'   \item{city}{city}
#'   \item{state}{state}
#'   \item{p_pop}{percentage of population w.r.t total population}
#'   \item{p_ink}{percentage of income w.r.t total income}
#' }
#'
#' @source \url{http://federalgovernmentzipcodes.us/}
#'
"zip2012"

#' source_zip_list
#'
#' @description
#'  a list of zip code used in skm package demonstration.
#'
#' @format
#'  a character vector of length 51 includes one 5 digits zip code selected from
#'  each state, where the most central zip code in each state selected.
#'
"source_zip_list"
#------------------------------------------------------------------------------#


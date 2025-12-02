#' Reliability Test Data
#'
#' A dataset containing example reliability test data from the military report
#' "Reliability Growth Prediction" (1986) by The Analytical Sciences Corporation.
#' This dataset includes cumulative ETI, failure counts, cumulative MTBF, report
#' numbers, flags, and causes for two different LRUs (G1 and G2).
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G5.0} The data set is a standard data set from a published paper.
#' @srrstats {G5.1} The data set is created within and used to test the package.
#' The data set is exported so that users can confirm tests and run examples.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @format @format ## `testdata`
#' A data frame with 25 rows and 6 variables:
#' \describe{
#'  \item{LRU}{The Line Replaceable Unit identifier (G1 or G2).}
#'  \item{Cum_ETI}{Cumulative Equivalent Test Hours (ETI).}
#'  \item{Failure_Count}{Cumulative number of failures observed.}
#'  \item{Cum_MTBF}{Cumulative Mean Time Between Failures (MTBF).}
#'  \item{Report_No}{Report number associated with the failure.}
#'  \item{Flag}{A flag indicating special conditions or notes.}
#'  \item{Cause}{Cause of the failure (e.g., D for Design, M for Manufacturing, R for Random, NR for No Report).}
#'  }
#'  @usage data(testdata)
#' @examples
#' data(testdata)
#' head(testdata)
#' summary(testdata)
#' str(testdata)
#'
"testdata"

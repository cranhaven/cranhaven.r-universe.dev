#' lifelogr: A package for life logging in R.
#'
#' The lifelogr package provides a framework for creating visualizations 
#' and experimenting on onself using self-tracking health data from multiple
#' sources. It provides an example of what a user's combined dataset 
#' might look like: \code{EX}.
#' 
#' To learn more about lifelogr, start with the vignette: 
#' \code{browseVignettes(package = "lifelogr")}
#'
#' @docType package
#' @name lifelogr
#' @importFrom utils globalVariables
NULL

utils::globalVariables(names = c("day_of_week",
                                 "sleepDurationHrs",
                                 "minAsleepHrs",
                                 "group",
                                 "cor",
                                 "as.formula",
                                 "anova",
                                 "time",
                                 "hours",
                                 "measure",
                                 "anova"
                                 ))
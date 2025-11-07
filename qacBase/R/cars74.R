#' @title Motor Trend car road tests
#'
#' @description
#' The data was extracted from the 1974 Motor Trend US magazine, and
#' comprises fuel consumption and 10 aspects of automobile design and
#' performance for 32 automobiles (1973-74 models).
#'
#' @details
#' This dataset is the \code{\link[datasets]{mtcars}} dataset that comes
#' with base R. However, \code{cyl}, \code{vs}, \code{am}, \code{gear}
#' and \code{carb} have been converted
#' to factors and rownames have been converted to the variable \code{auto}.
#' A description of the variables by Soren Heitmann can be found
#' \href{http://rstudio-pubs-static.s3.amazonaws.com/61800_faea93548c6b49cc91cd0c5ef5059894.html}{here}.
#'
#' @docType data
#' @keywords datasets
#' @name cars74
#' @usage cars74
#'
#' @format A data frame with 32 rows and 11 variables. The variables are
#' as follows:
#' \describe{
#'   \item{auto}{highway miles per gallon}
#'   \item{mpg}{Miles/(US) gallon}
#'   \item{cyl}{Number of cylinders}
#'   \item{disp}{Displacement (cu.in.)}
#'   \item{hp}{Gross horsepower}
#'   \item{drat}{Rear axle ratio}
#'   \item{wt}{Weight (1000 lbs)}
#'   \item{qsec}{1/4 mile time}
#'   \item{vs}{Engine cylinder configuration}
#'   \item{am}{Transmission type}
#'   \item{gear}{Number of forward gears}
#'   \item{carb}{Number of carburetors}
#' }
#'
#' @source Henderson and Velleman (1981), Building multiple
#' regression models interactively. Biometrics, 37, 391-411.
#'
#' @examples
#' summary(cars74)
NULL

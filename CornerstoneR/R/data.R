#' @title Data from carstats
#' @description
#'   Dataset of different cars and various values.
#' @name carstats
#' @docType data
#' @format
#'   A \code{\link{data.table}} object with 406 observations and 9 variables.
#'   The variables and their scale of measurement are as follows:
#'   \itemize{
#'     \item{Model: }{nominal}
#'     \item{Origin: }{nominal}
#'     \item{MPG: }{interval}
#'     \item{Cylinders: }{ordinal}
#'     \item{Displacement: }{interval}
#'     \item{Horsepower: }{interval}
#'     \item{Weight: }{interval}
#'     \item{Acceleration: }{interval}
#'     \item{Model.Year: }{interval}
#'   }
#' @source Cornerstone sample dataset
NULL 

#' @title Voltage of 26 Different Runs
#' @description
#'   The datasets contains 26 different runs in each case measuring the voltage over time. The S-shaped
#'   curves are different for each run.
#' @name rundata
#' @docType data
#' @format
#'   A \code{\link{data.table}} object with 7826 observations and 3 variables.
#'   The discrete variable 'FileName' has 26 levels corresponding to the file name of the source data
#'   from Cornerstone. The remaining other two variables are continuous.
#' @source Cornerstone sample dataset
NULL

#' @title Survival of Passengers on the Titanic
#' @description
#'   This data set provides information on the fate of passengers on the fatal maiden voyage 
#'   of the ocean liner Titanic, summarized according to economic status (Class), Age, Sex, and Survival.
#' @name titanic
#' @docType data
#' @format
#'   A \code{\link{data.table}} object with 2201 observations and 4 variables.
#'   The discrete variables and their levels are as follows:
#'   \itemize{
#'     \item{Class: }{1st, 2nd, 3rd, Crew}
#'     \item{Age: }{Child, Adult}
#'     \item{Sex: }{Male, Female}
#'     \item{Survived: }{Yes, No}
#'   }
#' @source Cornerstone sample dataset
#' @references 
#'   Dawson, Robert J. MacG. (1995), The Unusual Episode Data Revisited. 
#'   \emph{Journal of Statistics Education}, \strong{3}. 
#'   \url{ http://jse.amstat.org/v3n3/datasets.dawson.html}
#' 
NULL

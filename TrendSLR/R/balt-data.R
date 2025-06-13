#' Ocean water level data for Baltimore, USA
#'
#' Annual average ocean water level data from Permanent Service for Mean Sea
#' Level (Holgate \emph{et al.,} 2013).
#'
#' @docType data
#'
#' @usage data(Balt)
#'
#' @format Time series data file with the first column the year and the second
#' column the corresponding annual average ocean water level (in millimetres).
#' File contains 115 records spanning the period from 1904 to 2018 with a single
#' missing value in 1990.
#'
#' @details The raw (*.csv) form of this data set when converted to a time series
#' object (refer \code{\link[stats]{ts}}) is used extensively in the examples
#' throughout this manual.
#'
#' @references Holgate, S.J., Matthews, A., Woodworth, P.L., Rickards, L.J.,
#' Tamisiea, M.E., Bradshaw, E., Foden, P.R., Gordon, K.M., Jevrejeva, S. and
#' Pugh, J., 2013. New data systems and products at the Permanent Service for
#' Mean Sea Level. \emph{Journal of Coastal Research}, 29(3), pp. 493-504.
#'
#' @source \href{http://www.psmsl.org/data/obtaining/map.html}{Permanent Service
#' for Mean Sea Level (2019)}
#'
#' @seealso \code{\link{custom.trend}}, \code{\link{gap.fillview}},
#' \code{\link{msl.trend}}, \code{\link{msl.fileplot}}, \code{\link{msl.screenplot}},
#' \code{\link{summary}}, \code{\link{s}}, \code{\link{t}}.
#'
#' @examples
#' data(Balt) # typical data file structure
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # convert to time series object
#' plot(ts1, type = "l", xlab = "Year", ylab = "Annual Average Mean Sea Level (mm)",
#' main = 'BALTIMORE, USA')
#' str(Balt) # check structure of data file
"Balt"

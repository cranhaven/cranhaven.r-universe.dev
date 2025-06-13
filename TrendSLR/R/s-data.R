#' sample 'msl.trend' object
#'
#' Output of call to \code{\link{msl.trend}} used in examples throughout this Manual.
#'
#' @docType data
#'
#' @usage data(s)
#'
#' @format msl.trend object
#'
#' @details This \code{\link{msl.trend}} object is used extensively in the
#' examples throughout this manual in order to call the object direct rather than
#' producing the same via original code which can be computationally expensive. This
#' object results from a decomposition of the Baltimore (USA) record, filling gaps with
#' spline interpolation and using 500 iterations to generate error margins via
#' bootstrapping.
#'
#' \strong{Note: }Ordinarily the user would first create an annaul average time series
#' object from the data, to then create an \code{\link{msl.trend}} object using
#' the general form of sample code advised in the example (see below).
#'
#' @seealso \code{\link{msl.trend}}, \code{\link{msl.fileplot}},
#' \code{\link{msl.screenplot}}, \code{\link{summary}}, \code{\link{Balt}}.
#'
#' @examples
#'
#' data(Balt) # Baltimore mean sea level record
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
#' \donttest{s <- msl.trend(ts1, fillgaps = 3, iter = 500, 'BALTIMORE, USA')}
#'
#' data(s)
#' str(s) # check structure of object
#' msl.screenplot(s) # check screen output
"s"

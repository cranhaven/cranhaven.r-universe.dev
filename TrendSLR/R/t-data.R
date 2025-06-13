#' sample 'custom.trend' object
#'
#' Output of call to \code{\link{custom.trend}} used in examples throughout this
#' Manual.
#'
#' @docType data
#'
#' @usage data(t)
#'
#' @format custom.trend object
#'
#' @details This \code{\link{custom.trend}} object is used extensively in the
#' examples throughout this manual in order to call the object direct rather than
#' producing the same via original code which can be computationally expensive. This
#' object results from a decomposition of the Baltimore (USA) record, filling gaps
#' firstly with the default SSA option in the \code{\link{gap.fillview}} function.
#' The \code{\link{gap.fillview}} object has then been used to otimise the trend
#' and DOF settings via the \code{\link{check.decomp}} function. The \code{\link{custom.trend}}
#' function is then applied with the desired settings.
#'
#' \strong{Note: }The above-mentioned workflow is used to create the \code{\link{custom.trend}}
#' object using the general form of sample code advised in the example (see below).
#'
#' @seealso \code{\link{custom.trend}}, \code{\link{msl.fileplot}},
#' \code{\link{msl.screenplot}}, \code{\link{summary}}, \code{\link{Balt}}.
#'
#' @examples
#'
#' data(Balt) # Baltimore mean sea level record
#' ts1 <- ts(Balt[2], start = Balt[1, 1]) # create time series input object
#' g <- gap.fillview(ts1, station_name = "Baltimore", fillgaps = 1) # SSA gap fill
#' \donttest{t <- custom.trend(g, station_name = "Baltimore (USA)", iter = 500, trend = c(1,2),
#' vlm = 0.6)}
#'
#' data(t)
#' str(t) # check structure of object
"t"

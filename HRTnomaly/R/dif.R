#' @name dif
#' @aliases dif
#' @title Deep Isolation Forest
#' @description
#' The function builds a deep isolation forest that uses fuzzy logic to determine if a record is anomalous or not.
#' The function takes a wide-format \code{data.frame} object as input and returns it with two appended vectors.
#' The first vector contains the anomaly scores as numbers between zero and one, and the second vector provides
#' a set of logical values indicating whether the records are outliers (\code{TRUE}) or not (\code{FALSE}).
#' @usage dif(dta, nt = 100L, nss = NULL, threshold = 0.95)
#' @param dta A wide-format \code{data.frame} object with records (stored by row).
#' @param nt Number of deep isolation trees to build to form the forest. By default, it is set to \code{100}.
#' @param nss Number of subsamples used to build a single deep isolation tree.
#' If set (by default) to \code{NULL}, the program will randomly select 25\% of the records provided to the \code{dta} argument.
#' @param threshold A number between zero and one used as a threshold when identifying outliers from the anomaly scores.
#' By default, this argument is set to \code{0.95}, so that 5\% of the records is going to be classified as anomalous.
#' @details
#' The argument \code{dta} is provided as an object of class \code{data.frame}.
#' This object is considered as a wide-format \code{data.frame}.
#' The use of the R-packages \code{dplyr}, \code{purrr}, and \code{tidyr} is highly recommended to simplify the conversion of datasets between long and wide formats.
#' @return The wide-format \code{data.frame} is provided as input data and contains extra columns:
#'   \describe{
#'     \item{scores}{A numeric vector of anomaly scores ranging from 0 to 1, where values closer to 1 indicate higher anomaly.}
#'     \item{flags}{A logical vector indicating whether each record is flagged as an outlier (\code{TRUE}) or not (\code{FALSE}) based on the specified \code{threshold}.}
#'   }
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' \dontrun{
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Detect outliers in the `iris` dataset
#' res <- dif(iris)
#' }
#' @keywords outliers distribution probability
#' @export
dif <- function(dta, nt = 100L, nss = NULL, threshold = 0.95) {
	if (!is.data.frame(dta))
		stop("The argument `dta` must be a `data.frame` object.")
	nt <- as.integer(nt)
	if (nt < 1) stop("Provide a positive number of deep isolation trees")
	if (is.null(nss)) nss <- nrow(dta) * 0.25
	nss <- as.integer(nss)
	if (nss < 3 || nrow(dta) < 3) stop("Provide more data points or increase the size of the subsamples")
	if (nss > nrow(dta)) nss <- nrow(dta)
	whchr <- sapply(dta, is.character)
	dtchr <- as.data.frame(lapply(dta[, whchr], as.factor))
	dtchr <- if (prod(dim(dtchr)) > 0) model.matrix(~ . + 0, data = dtchr) else c()
	dtnum <- dta[, !whchr]
	dtnum <- if (prod(dim(dtnum)) > 0) model.matrix(~ . + 0, data = dtnum) else c()
	dtnum <- cbind(dtnum, dtchr)
	storage.mode(dtnum) <- "double"
	dimD <- dim(dtnum)
	s <- .C("dif", s = double(dimD[1]), dtnum, dimD, nt, nss, pakcage = "HRTnomaly")$s
	dta <- cbind.data.frame(dta, scores = s, flags = s > quantile(s, prob = threshold))
	return(dta)
}

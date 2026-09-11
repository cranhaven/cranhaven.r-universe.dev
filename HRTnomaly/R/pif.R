#' @name pif
#' @aliases pif
#' @title Proximity Isolation Forest
#' @description
#' The function builds a proximity isolation forest that uses fuzzy logic to determine if a record is anomalous or not.
#' The function takes a \code{list} object as input and returns it with two vectors appended as attributes.
#' The first vector contains the anomaly scores as numbers between zero and one, and the second vector provides
#' a set of logical values indicating whether the records are outliers (\code{TRUE}) or not (\code{FALSE}).
#' @usage pif(dta, nt = 100L, nss = NULL, max_depth = 12L, threshold = 0.95,
#'     proximity_type = c("single", "paired", "pivotal"), dist_fun = NULL)
#' @param dta A \code{list} object with records (stored as individual entries on the list).
#' @param nt Number of deep isolation trees to build to form the forest. By default, it is set to \code{100}.
#' @param nss Number of subsamples used to build a proximity isolation tree in the forest.
#' If set (by default) to \code{NULL}, the program will randomly select 25\% of the records provided to the \code{dta} argument.
#' @param max_depth An integer number corresponding to the maximum depth achieved by a proximity isolation tree in the forest.
#' By default, this argument is set to \code{12}.
#' @param threshold A number between zero and one used as a threshold when identifying outliers from the anomaly scores.
#' By default, this argument is set to \code{0.95}, so that 5\% of the records is going to be classified as anomalous.
#' @param proximity_type A character string denoting the number the number of proximity prototypes used by the algorithms (see details for more information).
#' By default, a \code{"single"} prototype is randomly chosen to split a branch in the isolation tree.
#' @param dist_fun A function computing the distance between any pair of components in \code{dta}.
#' If set (by default) to \code{NULL}, the program will select an Euclidean distance for two numerical arrays.
#' @details
#' The argument \code{dta} is provided as an object of class \code{list}.
#' This object is considered as a list of arbitrary R objects that will be analyzed by one of the three algorithms provided with the \code{pif} function.
#'
#' Three algorithms are implemented. The user can choose the proximity type by providing the number of prototypes used to build the isolation trees in the forest. A \code{"single"} prototype uses the distance between an input data point to a single randomly selected prototype at each branching node of the tree. Two prototypes (denoted as \code{"paired"}) are randomly chosen and successively considered as gravitational point of their respective basins of attraction for partitioning the data. An additional \code{"pivotal"} point is randomly selected to enhance the algorithm based on two prototypes. In this case, the two distances between a data point and the two prototypes are normalized through the Steinhaus transformation and the pivotal prototype.
#' @return The original input \code{list} \code{dta} with the following attributes appended:
#'   \describe{
#'     \item{scores}{A numeric vector of anomaly scores, ranging from 0 to 1, where higher values indicate a higher likelihood of being an outlier.}
#'     \item{flag}{A logical vector indicating whether each element in the input list is flagged as an outlier (\code{TRUE}) or not (\code{FALSE}) based on the specified \code{threshold}.}
#'   }
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' \dontrun{
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Personalized distance
#' my_dst <- function(x, y) {
#'   xn <- as.numeric(x[[1]][1:4])
#'   yn <- as.numeric(y[[1]][1:4])
#'   num <- mean((xn - yn)^2)
#'   den <- median((xn - yn)^2)
#'   return(num / (1 + den))
#' }
#' # Converting the dataset iris to a list
#' ir <- apply(iris, 1, list)
#' # Detect outliers in the `iris` dataset
#' res_sng <- pif(ir, 5L, 18L, 5L, .85, "single", my_dst)
#' res_prd <- pif(ir, 5L, 18L, 5L, .85, "paired", my_dst)
#' res_prx <- pif(ir, 5L, 18L, 5L, .85, "pivotal", my_dst)
#' # count identified anomalies
#' print(sum(attr(res_prd, "flag")))
#' }
#' @keywords outliers distribution probability
#' @export
pif <- function(dta, nt = 100L, nss = NULL,
                max_depth = 12L, threshold = 0.95,
                proximity_type = c("single", "paired", "pivotal"),
                dist_fun = NULL) {
  if (is.null(dist_fun)) {
    dist_fun <- function(x, y) sum((x - y)^2)
  } else {
    if (!is.function(dist_fun))
      stop("The argument `dist_fun` must be `function(x, y)`.")
  }
  prx <- switch(proximity_type, paired = 2L,
                pivotal = 3L, 1L) # "single" (by default value) = 1L
  max_depth <- as.integer(max_depth)
  if (max_depth < 1)
    stop("The argument `max_depth` must be a positive integer number.")
  if (!is.list(dta))
    stop("The argument `dta` must be a `list` object.")
  nt <- as.integer(nt)
  if (nt < 1) stop("Provide a positive number of proximity isolation trees.")
  n <- length(dta)
  if (is.null(nss)) nss <- n * 0.25
  nss <- as.integer(nss)
  if (nss < (prx + 3L) || length(dta) < (prx + 3L))
    stop("Provide more data points or increase the size of the subsamples.")
  if (nss > length(dta))
    nss <- length(dta)
  rnv <- new.env()
  rnv <- parent.env(rnv)
  s <- .Call("pif", dta, prx, nt, nss, max_depth,
             quote(dist_fun(dta[[i]], dta[[j]])),
             rnv, pakcage = "HRTnomaly")
  attr(dta, "flag") <- s > quantile(s, prob = threshold)
  attr(dta, "scores") <- s
  return(dta)
}

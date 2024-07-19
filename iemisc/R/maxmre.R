#' Maximum Mean relative error (MAXRE)
#'
#' This function computes the maximum mean relative error (MAXRE).
#'
#' (MAXRE) is expressed as
#'
#'	\deqn{max \sum \limits_{i=1}^N \Bigg | \frac{P_i - O_i}{O_i} \Bigg |}
#'
#' \describe{
#'	\item{\emph{N}}{the number of observations}
#'	\item{\emph{P_i}}{the predicted values}
#'	\item{\emph{O_i}}{the observed or reference values}
#' }
#'
#'
#'
#' @param predicted numeric vector that contains the model predicted
#'   data points (1st parameters)
#' @param observed numeric vector that contains the observed data
#'   points (2nd parameters)
#' @param na.rm logical vector that determines whether the missing
#'   values should be removed or not.
#'
#' @return maximum mean relative error (MAXRE) as a numeric vector using the same
#'    units as the given variables. The default choice is that any NA values
#'    will be kept (\code{na.rm = FALSE}). This can be changed by
#'    specifying \code{na.rm = TRUE}, such as \code{maxmre(pre, obs, na.rm = TRUE)}.
#'
#'
#'
#'
#'
#' @references
#' Huang, J. (2018). "A Simple Accurate Formula for Calculating Saturation Vapor Pressure of Water and Ice", \emph{Journal of Applied Meteorology and Climatology}, 57(6), 1265-1272. Retrieved Nov 4, 2021, \url{https://web.archive.org/web/20221024040058/https://journals.ametsoc.org/view/journals/apme/57/6/jamc-d-17-0334.1.xml}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @seealso \code{\link[ie2misc]{mape}} for mean absolute percent error (MAPE), \code{\link[ie2misc]{madstat}} for
#'  mean-absolute deviation (MAD), \code{\link[ie2misc]{dr}} for "index of agreement (dr)", \code{\link[ie2misc]{vnse}}
#'  for Nash-Sutcliffe model efficiency (NSE), \code{\link{rmse}} for
#'  root mean square error (RMSE), and \code{\link{mre}} for the mean relative
#'  error (MRE).
#'
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Example 1
#' 
#' library(iemisc)
#' 
#' obs <- 1:10 # observed
#' pre <- 2:11 # predicted
#' maxmre(pre, obs)
#' 
#' 
#' 
#' 
#' # Example 2
#' 
#' install.load::load_package("iemisc", "rando", "data.table")
#' 
#' set_n(100) # makes the example reproducible
#' obs1 <- r_norm(.seed = 605) # observed
#' pre1 <- r_norm(.seed = 364) # predicted
#'
#'
#' # using the vectors pre1 and obs1
#' maxmre(pre1, obs1)
#'
#'
#' # using a matrix of the numeric vectors pre1 and obs1
#' mat1 <- matrix(data = c(obs1, pre1), nrow = length(pre1), ncol = 2,
#' byrow = FALSE, dimnames = list(c(rep("", length(pre1))),
#' c("Predicted", "Observed")))
#' maxmre(mat1[, 2], mat1[, 1])
#'
#' # mat1[, 1] # observed values from column 1 of mat1
#' # mat1[, 2] # predicted values from column 2 of mat1
#'
#'
#' # using a data.frame of the numeric vectors pre1 and obs1
#' df1 <- data.frame(obs1, pre1)
#' maxmre(df1[, 2], df1[, 1])
#'
#' # df1[, 1] # observed values from column 1 of df1
#' # df1[, 2] # predicted values from column 2 of df1
#'
#'
#'
#' # using a data.table of the numeric vectors pre1 and obs1
#' df2 <- data.table(obs1, pre1)
#' maxmre(df2[, 2, with = FALSE][[1]], df2[, 1, with = FALSE][[1]])
#'
#' # df2[, 1, with = FALSE][[1]] # observed values from column 1 of df2
#' # df2[, 2, with = FALSE][[1]] # predicted values from column 2 of df2
#'
#'
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#'
#' @export
maxmre <- function (predicted, observed, na.rm = FALSE) {

checks <- c(predicted, observed)

# Check
assert_that(!any(qtest(checks, "N+(,)") == FALSE), msg = "Either x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(identical(length(predicted), length(observed)), msg = "Check the length of the predicted and observed vectors since they don't match. Use a different set of values and then try again.")
# provide an error message if the length of the numeric vectors do not match

assert_that(qtest(na.rm, "B==1"), msg = "There is not a logical value for na.rm or more than 1 logical value for na.rm.")
# only process with enough known variables and provide an error message if the check fails


# The base::mean.default code has been helpful with regards to the treatment
# of non-numeric values

# The moments::kurtosis code has been helpful with regards to the treatment of
# na.rm

if (na.rm == TRUE) {

  observed <- observed[!is.na(observed)]

  predicted <- predicted[!is.na(predicted)]

  n <- length(predicted)

 max(sum(abs((predicted - observed) / observed), na.rm = na.rm))

} else {

  n <- length(predicted)

max(sum(abs((predicted - observed) / observed), na.rm = na.rm))

}
}

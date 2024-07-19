#' Manning's n for natural channels
#'
#' @description
#' This function computes Manning's n for natural channels.
#'
#' @details
#' "Roughness values for channels and flood plains should be determined
#' separately. The composition, physical shape, and vegetation of a flood
#' plain can be quite different from those of a channel." Source: USGS.
#'
#' The equation to find Manning's n for natural channels is expressed as
#'
#' \deqn{n = \left(n_b + n_1 + n_2 + n_3 + n_4\right)m}
#'
#' \describe{
#'	\item{\emph{n}}{Manning's n}
#'	\item{\emph{\eqn{n_b}}}{"the base value for a straight, uniform channel"}
#'	\item{\emph{\eqn{n_1}}}{"correction for surface irregularities"}
#'	\item{\emph{\eqn{n_2}}}{"correction for variations in the shape and size
#'	     of the cross section"}
#'	\item{\emph{\eqn{n_3}}}{"correction for obstructions"}
#'	\item{\emph{\eqn{n_4}}}{"correction for vegetation and flow conditions"}
#'	\item{\emph{m}}{"correction factor for channel meandering"}
#' }
#'
#' Source: Sturm page 114.
#'
#'
#' @param nb numeric vector that contains "the base value for a straight,
#'   uniform channel", if needed
#' @param n1 numeric vector that contains "correction for surface
#'   irregularities", if needed
#' @param n2 numeric vector that contains "correction for variations in the
#'   shape and size of the cross section", if needed
#' @param n3 numeric vector that contains "correction for obstructions", if
#'   needed
#' @param n4 numeric vector that contains "correction for vegetation and
#'   flow conditions", if needed
#' @param m numeric vector that contains "correction factor for channel
#'   meandering", if needed
#'
#' @return n as Manning's n for a natural channel as a numeric vector.
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 114.
#'    \item Guide for Selecting Manning's Roughness Coefficients for Natural Channels and Flood Plains, United States Geological Survey Water-supply Paper 2339 Metric Version
#'    \item George J. Arcement, Jr., and Verne R. Schneider, United States Geological Survey Water-Supply Paper 2339, "Guide for Selecting Manning's Roughness Coefficients for Natural Channels and Flood Plains", 1989, \url{https://pubs.usgs.gov/wsp/2339/report.pdf}.
#' }
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
#' @seealso \code{\link{nc1}} for Horton method for composite Manning's n, \code{\link{nc2}} for
#'  Einstein and Banks method for composite Manning's n, \code{\link{nc3}} for
#'  Lotter method for composite Manning's n, and \code{\link{nc4}} for
#'  Krishnamurthy and Christensen method for composite Manning's n.
#'
#'
#'
#'
#' @examples
#' library(iemisc)
#' 
#' # Example from Table 4. from the USGS Reference text page 35
#' n(nb = 0.025, n4 = 0.005, m = 1.00)
#'
#' 
#' 
#' 
#' 
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
n <- function (nb = NULL, n1 = NULL, n2 = NULL, n3 = NULL, n4 = NULL, m = NULL) {

nb <- nb

n1 <- n1

n2 <- n2

n3 <- n3

n4 <- n4

m <- m

checks <- c(nb, n1, n2, n3, n4, m)

assert_that(!any(qtest(checks, "N+(,)") == FALSE), msg = "Either P or n is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

sum(c(nb, n1, n2, n3, n4), na.rm = TRUE) * m

}






#' Horton method for composite Manning's n
#'
#' This function computes the composite Manning's n using the Horton method.
#'
#' "A composite value of Manning's n for a single channel; that is, for the
#' main channel only of a compound channel or a canal with laterally varying
#' roughness." Source: Sturm page 118.
#'
#' The equation to find Manning's composite n using the Horton method is
#'
#' \deqn{n_c = \left[\frac{\sum \limits_{i=1}^N P_i n_i^\frac{3}{2}}{P}\right] ^ \frac{2}{3}}
#'
#' \describe{
#'	\item{\emph{\eqn{n_c}}}{Manning's composite n}
#'	\item{\emph{P}}{"wetted perimeters of the entire cross section"}
#'	\item{\emph{\eqn{P_i}}}{"wetted perimeters of any section i"}
#'	\item{\emph{\eqn{n_i}}}{"Manning's n of any section i"}
#'	\item{\emph{N}}{"total number of sections into which the wetted perimeters
#'        is divided"}
#' }
#'
#' Source: Sturm page 118.
#'
#'
#' @param P numeric vector that contains "wetted perimeters of any section i"
#' @param n numeric vector that contains "Manning's n of any section i"
#'
#' @return numeric vector that contains nc1 as Manning's composite n.
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 118.
#'    \item Dan Moore, P.E., NRCS Water Quality and Quantity Technology Development Team, Portland Oregon, "Using Mannings Equation with Natural Streams", August 2011, \url{https://web.archive.org/web/20210416091858/https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/xsec/manningsNaturally.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
#'
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
#' @seealso \code{\link{n}} for Manning's n for natural channels, \code{\link{nc2}} for
#'  Einstein and Banks method for composite Manning's n, \code{\link{nc3}} for
#'  Lotter method for composite Manning's n, and \code{\link{nc4}} for
#'  Krishnamurthy and Christensen method for composite Manning's n.
#'
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example from the Moore Reference text
#' nc1(n = c(0.05, 0.035, 0.05, 0.04), P = c(22.22, 34.78, 2.00, 6.08))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
nc1 <- function (P, n) {

checks <- c(P, n)

# check on P and n
assert_that(!any(qtest(checks, "N+(,)") == FALSE), msg = "Either P or n is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


Ptotal <- sum(P)

assert_that(!any(qtest(Ptotal, "N+(0,)") == FALSE), msg = "Either Ptotal is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

((sum(P * n ^ (3 / 2))) / Ptotal) ^ (2 / 3)
# Horton

}







#' Einstein and Banks method for composite Manning's n
#'
#' This function computes the composite Manning's n using the Einstein and
#' Banks method.
#'
#' "A composite value of Manning's n for a single channel; that is, for the
#' main channel only of a compound channel or a canal with laterally varying
#' roughness." Source: Sturm page 118.
#'
#' The equation to find Manning's composite n using the Einstein and Banks
#' method is
#'
#' \deqn{n_c = \left[\frac{\sum \limits_{i=1}^N P_i n_i^2}{P}\right] ^ \frac{1}{2}}
#'
#' \describe{
#'	\item{\emph{\eqn{n_c}}}{Manning's composite n}
#'	\item{\emph{P}}{"wetted perimeters of the entire cross section"}
#'	\item{\emph{\eqn{P_i}}}{"wetted perimeters of any section i"}
#'	\item{\emph{\eqn{n_i}}}{"Manning's n of any section i"}
#'	\item{\emph{N}}{"total number of sections into which the wetted perimeters
#'        is divided"}
#' }
#'
#' Source: Sturm page 118.
#'
#'
#' @param P numeric vector that contains "wetted perimeters of any section i"
#' @param n numeric vector that contains "Manning's n of any section i"
#'
#' @return numeric vector that contains nc2 as Manning's composite n.
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 118-119.
#'    \item Dan Moore, P.E., NRCS Water Quality and Quantity Technology Development Team, Portland Oregon, "Using Mannings Equation with Natural Streams", August 2011, \url{https://web.archive.org/web/20210416091858/https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/xsec/manningsNaturally.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
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
#' @seealso \code{\link{n}} for Manning's n for natural channels, \code{\link{nc1}} for
#'   Horton method for composite Manning's n, \code{\link{nc3}} for Lotter method for
#'   composite Manning's n, and \code{\link{nc4}} for Krishnamurthy and Christensen
#'   method for composite Manning's n.
#'
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example from the Moore Reference text
#' nc2(n = c(0.05, 0.035, 0.05, 0.04), P = c(22.22, 34.78, 2.00, 6.08))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
nc2 <- function (P, n) {

checks <- c(P, n)

# check on P and n
assert_that(!any(qtest(checks, "N+(,)") == FALSE), msg = "Either P or n is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails



Ptotal <- sum(P)

assert_that(!any(qtest(Ptotal, "N+(0,)") == FALSE), msg = "Either Ptotal is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

sqrt(sum(P * n ^ 2) / Ptotal)
# Einstein and Banks

}









#' Lotter method for composite Manning's n
#'
#' This function computes the composite Manning's n using the Lotter method.
#'
#' "A composite value of Manning's n for a single channel; that is, for the
#' main channel only of a compound channel or a canal with laterally varying
#' roughness."
#'
#' The equation to find Manning's composite n using the Lotter method is
#'
#' \deqn{n_c = \frac{PR^\frac{5}{3}}{\sum \limits_{i=1}^N \frac{P_i R_i^\frac{5}{3}}{n_i}}}
#'
#' \describe{
#'	\item{\emph{\eqn{n_c}}}{Manning's composite n}
#'	\item{\emph{P}}{"wetted perimeters of the entire cross section"}
#'	\item{\emph{R}}{"hydraulic radius of the entire cross section"}
#'	\item{\emph{\eqn{P_i}}}{"wetted perimeters of any section i"}
#'	\item{\emph{\eqn{R_i}}}{"hydraulic radius of any section i"}
#'	\item{\emph{\eqn{n_i}}}{"Manning's n of any section i"}
#'	\item{\emph{N}}{"total number of sections into which the wetted
#'        perimeters and hydraulic radius are divided"}
#' }
#'
#'
#'
#' @param P numeric vector that contains "wetted perimeters of any section i"
#' @param R numeric vector that contains "hydraulic radius of any section i"
#' @param n numeric vector that contains "Manning's n of any section i"
#'
#' @return numeric vector that contains nc3 as Manning's composite n.
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 118-119.
#' }
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
#' @seealso \code{\link{n}} for Manning's n for natural channels, \code{\link{nc1}}
#'   for Horton method for composite Manning's n, \code{\link{nc2}} for
#'   Einstein and Banks method for composite Manning's n, and \code{\link{nc4}}
#'   for Krishnamurthy and Christensen method for composite Manning's n.
#'
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' nc3(n = c(0.0024, 0.035), P = c(23.65, 36.08), R = c(2.02, 6.23))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
nc3 <- function (P, n, R) {

checks <- c(P, n, R)

# check on P, n, and R
assert_that(!any(qtest(checks, "N+(,)") == FALSE), msg = "Either P, n, or R is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


Ptotal <- sum(P)
Rtotal <- sum(R)

(Ptotal * Rtotal ^ (5 / 3)) / sum(P * R ^ (5 / 3) / n)
# Lotter

}







#' Krishnamurthy and Christensen method for composite Manning's n
#'
#' This function computes the composite Manning's n using the Krishnamurthy
#' and Christensen method.
#'
#' "A composite value of Manning's n for a single channel; that is, for the
#' main channel only of a compound channel or a canal with laterally varying
#' roughness."
#'
#' The equation to find Manning's composite n using the Krishnamurthy and
#' Christensen method is
#'
#' \deqn{\ln n_c = \frac{\sum \limits_{i=1}^N P_iy_i^\frac{3}{2} \ln n_i}{\sum \limits_{i=1}^N P_i y_i^\frac{3}{2}}}
#'
#' \describe{
#'	\item{\emph{\eqn{n_c}}}{Manning's composite n}
#'	\item{\emph{\eqn{P_i}}}{"wetted perimeters of any section i"}
#'	\item{\emph{\eqn{y_i}}}{"flow depth in the ith section"}
#'	\item{\emph{\eqn{n_i}}}{"Manning's n of any section i"}
#'	\item{\emph{N}}{"total number of sections into which the wetted
#'        perimeters and hydraulic radius are divided"}
#' }
#'
#'
#'
#' @param P numeric vector that contains "wetted perimeters of any section i"
#' @param y numeric vector that contains "flow depth in the ith section"
#' @param n numeric vector that contains "Manning's n of any section i"
#'
#' @return numeric vector that contains nc4 as Manning's composite n.
#'
#'
#' @references
#' \enumerate{
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, page 118-119.
#' }
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
#' @seealso \code{\link{n}} for Manning's n for natural channels, \code{\link{nc1}} for
#'   Horton method for composite Manning's n, \code{\link{nc2}} for Einstein and Banks
#'   method for composite Manning's n, and \code{\link{nc3}} for Lotter method for
#'   composite Manning's n.
#'
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' nc4(n = c(0.0024, 0.035), P = c(23.65, 36.08), y = c(10.23, 7.38))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
nc4 <- function (P, n, y) {

checks <- c(P, n, y)

# check on P, n, and y
assert_that(!any(qtest(checks, "N+(,)") == FALSE), msg = "Either P, n, or y is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


exp(sum(P * y ^ (3 / 2) * log(n)) / sum(P * y ^ (3 / 2)))

# Krishnamurthy and Christensen

}

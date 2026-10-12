#' Plot Confidential Interval
#'
#' A function to plot confidential interval for
#' such as \code{htest}, \code{TukeyHSD},
#' \code{glht} (\pkg{multcomp}),
#' \code{glm} (logistic regression only!)
#' and \code{posthocTGH} (\pkg{userfriendlyscience}) objects.
#'
#' @importFrom stats coefficients confint
#' @importFrom graphics abline arrows axis box plot points
#'
#' @export
#'
#'
#' @note \code{CIplot} was made based on \code{plot.TukeyHSD}.
#'  \preformatted{
#'  #  File src/library/stats/R/TukeyHSD.R
#'  #  Part of the R package, https://www.R-project.org
#'  #
#'  #  Copyright (C) 2000-2001  Douglas M. Bates
#'  #  Copyright (C) 2002-2015  The R Core Team
#' }
#'
#' @seealso \code{plot}, \code{axis}, \code{points}, \code{par}.
#'
#' @keywords plot
#' 
CIplot <-
    function(x, ...) UseMethod("CIplot")


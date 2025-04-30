#' @title Specify a smooth, decreasing and concave shape-restriction in a \pkg{bcgam} formula
#'
#' @description A symbolic routine to define that the systematic component \eqn{\eta} is
#' smooth, decreasing and concave with respect to a predictor in a bcgam formula.  	
#'
#' @param x a numeric predictor of length \eqn{n}.
#' @param numknots number of knots used to constrain \code{x}. It is ignored when the \code{knots}
#' argument is specified by the user. If neither \code{numknots} nor \code{knots} are specified by the user,
#' then \code{numknots} is \eqn{floor{4+n^(1/7)}}. The default is \code{0}.
#' @param knots knots used to constrain \code{x}. If they are not specified by the user, then they will 
#' be automatically created based on \code{numknots} and \code{space}. The default is \code{0}.  
#' @param space a character specifying the method to create knots. It is ignored when the \code{knots}
#' argument is specified by the user. If \code{space="E"}, then equally spaced knots will be created; 
#' if \code{space="Q"}, then a vector of equal quantiles will be created based on \code{x}
#' with duplicate elements removed. The default is \code{"Q"}.
#'
#' @export
#'
#' @details \code{sm.decr.conc} returns the vector \code{x} and assigns five attributes to it: name, shape (8 for  
#' "smooth, decreasing and concave"), numknots, knots and space. 
#'
#' This routine does not create the splines basis vectors by itself.
#' 
#' @return 
#' \item{x}{The numeric predictor \code{x}.} 
#'
#' @author Cristian Oliva-Aviles and Mary C. Meyer
#'
#' @references Meyer, M. C. (2008) Inference using shape-restricted regression splines. 
#' \emph{Annals of Applied Statistics} \strong{2(3)}, 1013-1033.
#'
#' @seealso NULL
#'
#' @examples
#' data(duncan)
#'
#' prestige <- duncan$prestige
#'
#' # specify knots
#' sm.decr.conc(prestige, knots=c(3,9,30,57,86,97))
#'
#' # specify number of knots
#' prestige.smdecrconc <- sm.decr.conc(prestige, numknots=7)
#'
#' # check attributes
#' attributes(prestige.smdecrconc)
sm.decr.conc <- function (x, numknots = 0, knots = 0, space = "Q") 
{
    cl <- match.call()
    pars <- match.call()[-1]
    attr(x, "nm") <- deparse(pars$x)
    attr(x, "shape") <- 8
    attr(x, "numknots") <- numknots
    attr(x, "knots") <- knots
    attr(x, "space") <- space
    x
}
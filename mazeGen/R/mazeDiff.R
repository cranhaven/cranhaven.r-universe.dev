#' @export
#' @param nodePosition This is the distribution of the colour node positions.
#' @param model There are three types of model to select from: "m1", "m2" or "m3".
#' @description This function tells us the difficulty level of the rank given a saturation and black node distribution
#' @details This function tells us the difficulty level of the rank given a saturation and black node distribution. The calculation of the difficulty level follows the Davies & Davies (1965) paper. In the article, there are three ways to calculate maze difficulty.
#' In Model 1, only two parameters were considered: rank and the number of possible paths through the maximum number of routes.
#'
#' \deqn{log(2^{R}/U_{\hat{m}})}
#'
#' where \eqn{2^R} is the total number of paths and \eqn{U_{\hat{m}}} is the paths through the maximum number of dots. Model 2 includes the saturation parameter. This is calculated based on:
#'
#' \deqn{log(2^{R}*s^{a}/U_{\hat{m}})}
#'
#' where \eqn{s} is the saturation and \eqn{a = 4}. The a value is recommended in the paper after using various values. Model 3 extends the second formula to include the minimum number of steps to pass through \eqn{\hat{m}}.
#'
#' \deqn{log(2^{R}*s^{a}*l^{b}/U_{\hat{m}})}
#'
#' where \eqn{l} is the minimum steps to pass through \eqn{\hat{m}} and \eqn{b=4}. The b value is recommended in the paper after using various values.
#'
#' We included all three approaches to calculate maze difficulty. It was to incorporated all the possible parameters of the task features that may potentially influence maze difficulty.
#' @author Aiden Loe and Maria Sanchez
#' @title Maze Diffculty
#' @seealso \code{\link{mazeEst}}, \code{\link{mazeAbility}}, \code{\link{np}}
#' @references
#' Davies, A. D., & Davies, M. G. (1965). The difficulty and graded scoing of Elithorn\verb{'s} perceptual maze test. \emph{British Journal of Psychology, 56(2-3)}, 295-302. \cr
#' @examples
#'
#' #Black nodes distribution
#' nodePosition <- np(rank=5,satPercent=0.5,seed=1)
#'
#' #calculate difficulty
#' mazeDiff(nodePosition, model="m1")
#'

mazeDiff <- function(nodePosition, model = "m1"){

  if(exists("nodePosition")==FALSE){
    stop("Please include an object for nodePosition")
  }

    if("np" %in% class(nodePosition) == FALSE){
      stop("nodePosition must be calculated using the np function.")
    }

#   if(rank != nodePosition$rank){
#     stop("The input rank and the rank to calculate the colour node positions are not the same.")
#   }

  if(model != "m1" && model != "m2" && model != "m3"){
    stop("Please select model as either m1, m2 or m3.")
  }

  rank <- nodePosition$rank

  u_Mhat <-maxScore(nodePosition) # number of optimised routes
  legs <- minStep(nodePosition)
  if(model=="m1"){
    diff<- log((2^rank)/u_Mhat)
  }else if(model=="m2"){
    diff<- log((2^rank * nodePosition$satPercent^4)/u_Mhat)
  }else{
  diff<- log((2^rank * nodePosition$satPercent^4 * legs^4)/u_Mhat)
  }
  return(diff)
}


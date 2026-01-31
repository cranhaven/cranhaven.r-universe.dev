#' Predict response from fitted nnet, using new data
#'
#' @param object The surveynnet object (returned by `surveynnet()`)
#' @param newdat The matrix or data frame of test examples. Must be of the same structure
#'  as the data matrix used to fit the surveynnet object.
#' @param ... arguments passed to or from other methods
#'
#' @return The matrix/vector of values returned by the trained network. Note: it is possible
#'  to pass type = "raw" or type = "class" as appropriate. See `predict.nnet()` for more details.
#'
#' @method predict surveynnet
#' @export
#'
#' @examples
#' # From the example in `surveynnet` help file:
#' y <- body_fat$pct_body_fat
#' x <- body_fat[,c("Weight_kg", "Height_cm", "Age")]
#' weight <- body_fat$survey_wt
#' strat <- body_fat$stratum
#' clust <- body_fat$cluster
#' y[strat==1] <- y[strat==1] + 30*0.00015*rnorm(sum(strat==1))
#' y[strat==2] <- y[strat==2] + 30*0.15*rnorm(sum(strat==2))
#' myout <- surveynnet(x,y,weight = weight, strat = strat, clust=clust)
#' newdat <-  2*x+rnorm(dim(x)[1])
#' predict(myout, newdat = newdat)
#'
predict.surveynnet <- function(object, newdat, ...){
  y <- object$results$target # the og y, for scale transf
  scale.y <- max(y)-min(y)
  center.y <- min(y)
  x.scale <- apply(newdat, 2, function(x2) (x2 - min(x2))/(max(x2) - min(x2)))
  nnet.object <- object$nnet.surv
  weights <- object$results$deff_wt
  out <- stats::predict(nnet.object, newdata = x.scale,
                 weights = weights)*scale.y+center.y
  return(out)
}

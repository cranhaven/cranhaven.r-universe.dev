#' Merge pre-computed components of f(theta1 | theta2, X)
#'
#' For use in the parallel call in wtdMix()
#' 
#' @param x Output from one of the parallel calls in wtdMix()
#' @param y Another output from one of the parallel calls in wtdMix()
#' 
mergePars = function(x, y) {
  x$mix = rbind(x$mix, y$mix)
  x$wts = c(x$wts, y$wts)
  x$wts.e = c(x$wts.e, y$wts.e)
  x$C1 = c(x$C1, y$C1)
  x$nodes.backtransformed = rbind(x$nodes.backtransformed, 
                                  y$nodes.backtransformed)
  x
}
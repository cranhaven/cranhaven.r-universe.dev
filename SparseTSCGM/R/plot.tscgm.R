#' Plot method for tscgm objects
#'
#' Plots matrices from tscgm objects. You can choose to plot the precision matrix
#' or the autoregressive coefficient matrix.
#'
#' @param x An object of class \code{tscgm}.
#' @param mat Character. Which matrix to plot. Choices are \code{"precision"} or \code{"autoregression"}.
#' @param ... Additional plotting arguments passed to the plotting function.
#'
#' @export
#' @method plot tscgm
plot.tscgm <- function(x, mat=c("precision","autoregression"),...){
  mat = match.arg(mat)
  if (mat == "precision") {
      prec <- x$theta
      #colnames(prec) <- rownames(prec) <- colnames(data)
      nw_full <- network(prec)
      plot.network(nw_full,label = network.vertex.names(nw_full), usearrows = FALSE,
         displayisolates = FALSE,...)
   }
 if (mat == "autoregression") {
      autoR <- x$gamma
      #colnames(autoR) <- rownames(autoR) <- colnames(data)
      nw_full <- network(autoR, loops=TRUE)
      plot.network(nw_full,label = network.vertex.names(nw_full), usearrows = TRUE,
        displayisolates = FALSE,...)
   }
}

#' @rdname pcoa.sig
#' @encoding UTF-8
#' @export
summary.pcoasig <- function(object, choices = c(1, 2), ...){
  if (length(choices) != 2) {
    stop("\n Choices must have length equal to two \n")
  }
  res <- object
  max1 <- max(object$vectors[, choices[1]])
  max2 <- max(object$vectors[, choices[2]])
  min1 <- min(object$vectors[, choices[1]])
  min2 <- min(object$vectors[, choices[2]])
  scores1 <- ifelse(object$correlations[,choices[1]]>0, object$correlations[,choices[1]]*max1, object$correlations[,choices[1]]*abs(min1))
  scores2 <- ifelse(object$correlations[,choices[2]]>0, object$correlations[,choices[2]]*max2, object$correlations[,choices[2]]*abs(min2))
  rscores <- data.frame(scores1, scores2)
  colnames(rscores) <- colnames(object$vectors[, choices])
  res$scores <- list(scores.sites = object$vectors[, choices], scores.species = rscores)
  class(res) <- "summarypcoasig"
  return(res)
}
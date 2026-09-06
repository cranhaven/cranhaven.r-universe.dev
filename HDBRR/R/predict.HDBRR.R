#' @rdname predict
#' @export

predict.HDBRR <- function(object,...){
  if(!inherits(object, "HDBRR")) stop("This function only works for objects of class 'HDBRR'\n");
  pred <- object$yhat
  if(length(pred) > 250){
    predic <- as.vector(round(pred[1:250],5))
  }
  else{
    predic <- as.vector(round(pred,5))
  }
  cat("\nPredicted values:\n")
  print(predic)
  if(length(pred) > 250){
    r <- length(pred) - 250
    cat("\n...",r,"values was omitted")
  }
  cat("\n")
}

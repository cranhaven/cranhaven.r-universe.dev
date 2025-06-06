AIC.expectreg <-
function(object, ..., k = 2)
{
	if(inherits(object, "boost") || inherits(object, "noncross")) 
	  stop("AIC only available for least squares methods.")
	
	w = matrix(object$asymmetries, nrow = length(object$response), 
        ncol = length(object$asymmetries), byrow = T)
	w = abs(w - 1 * (object$response < object$fitted))
	
    log(colSums(w * (object$response - object$fitted)^2)/length(object$response)) * 
        length(object$response) + k * colSums(object$diag.hatma)
}

meancenteraddon <-
function(params, x, batch) {

  if(any(is.na(x)))
	stop("Data contains missing values.")
  if(!is.factor(batch))
    stop("'batch' has to be of class 'factor'.")  
  if(!is.matrix(x))
    stop("'x' has to be of class 'matrix'.") 

  if(!inherits(params, "meancenter"))
     stop("Input parameter 'params' has to be of class 'meancenter'.")

  if(ncol(params$xadj) != ncol(x))
    stop("Number of variables in test data matrix different to that of training data matrix.")  
	 
   batches = levels(batch)
   nbatches = length(batches)
   
   if(nbatches > 1) {
     # Adjust for additive batch effects:
     adjustmentmod = lm(x~batch)
     design = model.matrix(~batch)
     adjustmentcoef = coef(adjustmentmod)
     xadj = x-design%*%adjustmentcoef
   }
   else {
	 adjustmentcoef <- colMeans(x)
     xadj = scale(x, center=adjustmentcoef, scale=FALSE)	 
   }
	 
  return(xadj) 

}

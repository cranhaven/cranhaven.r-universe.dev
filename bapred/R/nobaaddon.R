nobaaddon <-
function(params, x, batch) {

  if(any(is.na(x)))
	stop("Data contains missing values.")
  if(!is.factor(batch))
    stop("'batch' has to be of class 'factor'.")  
  if(!is.matrix(x))
    stop("'x' has to be of class 'matrix'.") 

  if(!inherits(params, "noba"))
    stop("Input parameter 'params' has to be of class 'noba'.")
	
  if(ncol(params$xadj) != ncol(x))
    stop("Number of variables in test data matrix different to that of training data matrix.")	 	

  xadj <- x
	
  return(xadj)

}

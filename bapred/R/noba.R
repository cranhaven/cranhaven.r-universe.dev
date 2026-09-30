noba <-
function(x, batch) {

  if(any(is.na(x)))
	stop("Data contains missing values.")
  if(!(is.factor(batch)))
    stop("'batch' has to be of class 'factor'.")  
  if(!is.matrix(x))
    stop("'x' has to be of class 'matrix'.") 
  
  params <- list(xadj=x)
  params$nbatches <- length(unique(batch))
  params$batch <- batch  
  
  class(params) <- "noba"
   
  return(params)

}

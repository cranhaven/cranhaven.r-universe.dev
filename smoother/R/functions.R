#SOME UTILITY FUNCTIONS FOR USE IN THIS PACKAGE

#Function to determine the actual window length from possibly decimal (fractional) window length


.determineWindowLength = function (x, w) 
{
  if (!is.numeric(x) | !is.numeric(w)) {
    stop("Arguments 'x' and 'w' must be numeric")
  }
  l = length(x)
  w.orig = w
  if (w > 0 && w < 1) {
    w = w * l
  }
  ret = as.integer(max(abs(w[1]), 1))
  if (length(x) <= ret | ret < 1) {
    stop(paste("Resultant window length is out of range (", 
               ret, "), must be >= 1", sep = ""), call. = FALSE)
  }
  return(ret)
}

.normalize = function(x){
    if (!is.numeric(x)) {
      stop("argument 'x' must be numeric")
    }
    if (length(x) == 1) {
      return(1)
    }
    total = sum(x)
    if (total == 0) {
      stop("argument 'x' must not sum to zero")
    }
    return(x/total)
}
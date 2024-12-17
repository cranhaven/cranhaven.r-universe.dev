makedata.corr = function(n = 10, k = 2, CORR, sample = FALSE){

# Function to generate random correlation matrix
rnd.cov = function(k){
  x = matrix(rnorm(k^2), nrow=k, ncol=k)
  x = x/sqrt(rowSums(x^2))
  x = x %*% t(x)
  return(x)
}

if(missing(CORR)){CORR = rnd.cov(k)}
if(dim(CORR)[1] != dim(CORR)[2]){
  stop("Correlation Matrix is not a square matrix")
}
if(dim(CORR)[1] != k | dim(CORR)[2] != k){
  stop("Correlation Matrix has not dimension k x k")
}

var.names = c(paste("x", 1:k, sep = ""))
X = matrix(rnorm(k * n), n)
if (sample){
  X = sweep(X, 2, colMeans(X), "-")
  # X = X %*% chol2inv(chol(cov(X))) # NOT WORKING??
  X = X %*% solve(chol(cov(X)))
}
X = X %*% chol(CORR)
colnames(X) = var.names
return(as.data.frame(X))
}

#'@title Generate Spectral Data
#'
#'@description Generate a dataset with spectral distribution.
#'
#'@references Bicici, E., & Yuret, D. (2007). Locally Scaled Density Based Clustering. International Conference on Adaptive and Natural Computing Algorithms (pp. 739-748). Berlin: Springer.
#'
#'@return dataframe
#'
#'@example
#'##Generate a dataset with 1000 data##
#'get_spectral(1000)
#'
#'@export

get_spectral <- function(n){
  x <- matrix(NA, 5*n, 2)
  x1 <- c()
  x2 <- c()
  distance <- vector(length = 5*n)

  for(i in 1:(5*n)){
    x[i,] <- runif(2,-1,1)
    distance[i] <- sqrt((x[i,1]^2)+(x[i,2]^2))

    if(distance[i] <= 1 && distance[i] >=0.9){
      x1 <- c(x1,x[i,1])
      x2 <- c(x2,x[i,2])
    } else if(distance[i] <= 0.5 && distance[i] >=0.4){
      x1 <- c(x1,x[i,1])
      x2 <- c(x2,x[i,2])
    } else if(distance[i] <= 0.1){
      x1 <- c(x1,x[i,1])
      x2 <- c(x2,x[i,2])
    }
  }

  return(cbind(x1[1:n],x2[1:n]))
}

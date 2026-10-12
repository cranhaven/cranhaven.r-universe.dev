#' Group sparse optimization
#' 
#' Group sparse optimization (GSparO) for least squares regression by using the proximal gradient algorithm to solve the L_{2,1/2} regularization model.
#'
#' GSparO is group sparse optimization for least squares regression described in [Hu et al(2017)], in which the proximal gradient algorithm is implemented to solve the L_{2,1/2} regularization model. GSparO is an iterative algorithm consisting of a gradient step for the least squares regression and a proximal steps for the L_{2,1/2} penalty, which is analytically formulated in this function. Also, GSparO can solve sparse variable selection problem in absence of group structure. In particular, setting group in GSparO be a vector of ones, GSparO is reduced to the iterative half thresholding algorithm introduced in [Xu et al (2012)].
#' Copyright by Dr. Yaohua Hu, College of Mathematics and Statistics, Shenzhen University.
#' Email: mayhhu@szu.edu.cn
#' 
#' @param A decoding matrix (matrix of predictors)
#' @param b noised signal (response)
#' @param Initial an initial point of iteration, recommend to set as a column vector of zeros
#' @param group group information, a column vector consisting of the length of each group
#' @param MaxIter the maximum number of iterations (a stopping criterion), recommend to set as 200
#' @param sparsity a guess of the group sparsity level (the number of nonzero groups)
#' @export
#' @examples
#' m <- 256
#' n <- 1024
#' sparsity <- 6
#' gLen <- 16
#' MaxIter <- 200
#' gNo <- 1024/gLen
#' group <- gLen*matrix(1,gNo,1)
#' A <- matrix(rnorm(m*n,0,1),m,n)
#' library(ThreeWay)
#' A <- orth(t(A))
#' A <- t(A)
#' gNo1 <- 1:gNo
#' ActInd <- sample(gNo1,gNo)
#' Bs <- matrix(0,n,1)
#' c <- matrix(rnorm(n,0,1),n,1)
#' for (i in 1:sparsity){
#'  Bs[((ActInd[i]-1)*gLen+1):(ActInd[i]*gLen)] <- matrix(1,gLen,1)}
#' c <- Bs*c
#' sigma <- 1e-3
#' b <- A%*%c + sigma*matrix(runif(m,min=0,max=1),m,1)
#' Initial <- matrix(0,n,1)
#' GSparO(A,b,Initial,group,MaxIter,sparsity)

GSparO <- function(A,b,Initial,group,MaxIter,sparsity){

k <- 1
x <- Initial
p <- length(group)
s <- sparsity
v <- 0.5
Va1 <- (2/3)^(1.5)/v
Bu1 <- 2*v*t(A)%*%b
Bu2 <- 2*v*t(A)%*%A
BuG <- matrix(0,p,1)

while (k < MaxIter){
  j <- 1
  t <- 0
  Bu <- x+Bu1-Bu2%*%x       #the gradient step for \|Ax-b\|^2
  
  while (j<=p){
   BuG[j] <- norm(Bu[(t+1):(t+group[j])],type = c("2"))
   t <- t+group[j]
   j <- j+1
  }
  
  BuGO <- sort(BuG)
  BuV <- BuGO[p-s]^(1.5)
  lambda <- Va1%*%BuV
  criterion <- BuGO[p-s]
  j <- 1
  t <- 0
  while (j<=p){    #the proximal step for \lambda \|x\|_{2,1/2}
    Y1 <- norm(Bu[(t+1):(t+group[j])],type = c("2"))
    
  if (Y1>criterion){
    q <- lambda*v/4
    phi <- acos(q%*%(3/Y1)^(1.5))
    eta <- 16*Y1^(3/2)%*%cos((pi-phi)/3)^3
    x[(t+1):(t+group[j])] <- (eta/(3*sqrt(3)*lambda*v+eta))*Bu[(t+1):(t+group[j])]
  }else{
    x[(t+1):(t+group[j])] <- matrix(0,group[j],1)
  }
    
    t <- t+group[j]
    j <- j+1
  }
  
  k <- k + 1
}
# collect history information
  x <<- x
  err <- norm(A%*%x-b,type = c("2"))^2
  err <<- err
}
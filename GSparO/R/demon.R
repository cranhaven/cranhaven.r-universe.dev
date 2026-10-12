#' The example for GSparO
#'
#' demon is a function that implements GSparO for an example of least squares regression with A and b being Gaussian ensembles. A figure plotting the true signal and estimation by GSparO is illustrated in Plots, and the errors of least squares regression and obtained solution are printed. Two packages ThreeWay and ggplot2 should be installed for implementing demon.
#'
#' Copyright by Dr. Yaohua Hu, College of Mathematics and Statistics, Shenzhen University.
#' Email: mayhhu@szu.edu.cn
#'
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom ThreeWay orth
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' 
#' @export
#' @examples
#' demon()

demon <- function(){

m <- 256
n <- 1024
sparsity <- 6
gLen <- 16
MaxIter <- 200
gNo <- 1024/gLen
group <- gLen*matrix(1,gNo,1)
A <- matrix(rnorm(m*n,0,1),m,n)

requireNamespace("ThreeWay")
A <- orth(t(A))
A <- t(A)
gNo1 <- 1:gNo
ActInd <- sample(gNo1,gNo)
Bs <- matrix(0,n,1)
c <- matrix(rnorm(n,0,1),n,1)

for (i in 1:sparsity){
  Bs[((ActInd[i]-1)*gLen+1):(ActInd[i]*gLen)] <- matrix(1,gLen,1)
}

c <- Bs*c
sigma <- 1e-3
b <- A%*%c + sigma*matrix(runif(m,min=0,max=1),m,1)
Initial <- matrix(0,n,1)

# run GSparO
GSparO(A,b,Initial,group,MaxIter,sparsity)

# plot
# using the ggplot2 package to plot
X1.n <- 1:n
x <- x
err <- err 
da1 <- data.frame(X1.n,c)
da2 <- data.frame(X1.n,x)

# illusration of results
print(sprintf("The error of linear system obtained by this GSparO is %f.",err))
print(sprintf("The relative error of selected features this GSparO is %f.",norm(x-c,type = c("2"))/norm(c,type = c("2"))))

requireNamespace("ggplot2")
p <- ggplot(data=da1, aes(X1.n,c))+
    geom_line(aes(color="True signal"))+
    geom_point(data=da2, aes(X1.n,x,color="GSparO"),size=1)+
    labs(color="Legend text")+
    labs(x="",y="",title="")
return(p)

}
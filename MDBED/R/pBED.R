#' @title Joint CDF of the bivariate exponential distribution (BED) based on the Moran-Downton model
#'
#' @description Given the values of the parameters, this function provides the value of the joint CDF of the BED for a positive pair or pairs
#' (x,y). The required inputs are the correlation coefficient, the scale parameters of the marginal distributions, and the pair/s (x,y).
#'
#' @usage pBED(rho,Betax,Betay,x,y)
#'
#' @param rho Correlation coefficient between the marginal distributions of x and y.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param x A value or set of values (vector) of the marginal distribution of x. It must be the same size of y.
#' @param y A value or set of values (vector) of the marginal distribution of y. It must be the same size of x.
#'
#' @importFrom orthopolynom glaguerre.polynomials
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach %dopar%
#'
#' @details The values of the joint density function are computed based on Eq.10.54 described in \insertCite{Balakrishna2009;textual}{MDBED}.
#'
#' @return The value of the joint CDF of the pair/s (x,y).
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @examples pBED(rho=0.85,Betax=1,Betay=1, x=0.6,y=0.8)
#' @export
pBED<-function(rho,Betax,Betay,x,y){

# define Global variables
i<-NULL


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 1. Checking of vector sizes

  if (length(x)-length(y)!=0){stop("Different vector's sizes")}

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 2. Obtain the standard variable

  x=x/Betax
  y=y/Betay

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 3. Theoretical Joint CDF
# Eq. 10.54 "Continuous Bivariate Distributions" Balakrishnan and Lai (2009)

  CDF<-function(rho,x1,y1){

    # Term of infinite sum
    n<-50 # number of sums
    lg<-orthopolynom::glaguerre.polynomials(n=n, alpha=1, normalized=FALSE)
    Term<-rep(0,n)

    for (j in 1:n){
      lgj<-lg[[j]]
      lgj<- as.function(lgj)
      k<-j-1
      Term[j]<-((rho^(k+1))/(k+1)^2)*lgj(x1)*lgj(y1)*x1*y1*exp(-(x1+y1))}

    InfSum<-sum(Term)
    P<-(1-exp(-x1))*(1-exp(-y1))+InfSum
    return(P)

  }

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 4. Apply the CDF function to each pair of values

  numCores <- ifelse(parallel::detectCores()>=2,2,1)
  doParallel::registerDoParallel(numCores)

  p<-foreach::foreach(i=1:length(x),.combine=c) %dopar% {
    p<-CDF(rho,x[i],y[i])}

  doParallel::stopImplicitCluster()

  return(p)

}

##  A table of binomial distribution using chebyshey polynomials using rescaling
##  This is an algorithm for efficient and exact calculation of binomial probabilities using
##  Chebyshev polynomials. For a fixed population size n and probability of "success" p, such calculations
##  simultaneously produce distributions for all possible values of the population number of "successes" x.
# Input population size n
#  probability of "success" p
# Output a matrix containing the required values of the hypergeometric probabilities indexed by the columns x=0,1,..,n.

binomialtable<-function(n,p,output='density') {


  # Calculate small gamma matrix
  small_gamma=matrix(data =0, nrow = 1, ncol = (n+1))

  Cheb_Polyn<-rep(0,n+1)
  Cheb_Polyn[1]<-1
  Cheb_Polyn[2]<-1-2*p
  for (l in 3:(n+1)) {
    i<-l-2
    Cheb_Polyn[l]<-((2*i+1)*(1-2*p)*Cheb_Polyn[l-1]-i*Cheb_Polyn[l-2])/(i+1)
  }
  small_gamma[1,]<-Cheb_Polyn

  # Calculate the big gamma matrix

  Calculate_row_big_gamma<-function(r){
    Cheb_Polyn_star<-rep(0,n+1)
    x=r-1
    Cheb_Polyn_star[1]<-exp(0.5)
    Cheb_Polyn_star[2]<-1-2*x/n
    for (l in 3:(n+1)) {
      i<-l-2
      Cheb_Polyn_star_current<-((2*i+1)*(n-2*x)*(exp(-0.5))*Cheb_Polyn_star[l-1]-i*(n+i+1)*(exp(-1))*Cheb_Polyn_star[l-2])/((i+1)*(n-i))
      Cheb_Polyn_star_current
      # Cheb_Polyn_star[l]<-round(Cheb_Polyn_star_current,20)
      if (Cheb_Polyn_star_current>=+1e+250) {
        Cheb_Polyn_star[l]<-+1e+250
      } else if (Cheb_Polyn_star_current<=-1e+250) {
        Cheb_Polyn_star[l]<--1e+250
      } else {
        Cheb_Polyn_star[l]<-Cheb_Polyn_star_current
      }
    }
    return(Cheb_Polyn_star)
  }

  big_gamma_without_normalized<-t(apply(t(t(1:(n+1))),1,Calculate_row_big_gamma))


  # Normailize big gamma


  big_gamma=matrix(data =0, nrow = (n+1), ncol = (n+1))
  for (c in 1:(n+1)) {
    i=c-1
    eps<-big_gamma_without_normalized[,c]
    norm_term<-t(eps)%*%eps
    big_gamma[,c]<-eps/(norm_term*exp((i-1)/2))

  }

  ##  Table of binomial
  BIG_pi_large_N=small_gamma%*%t(big_gamma)
  ## Output
  if (output=='density') {
    Table_result<-round(t(BIG_pi_large_N),8)
    rownames(Table_result)<-c("x=0",1:n)
    colnames(Table_result)<-c("Density")
    return(Table_result)
  } else if (output=='distribution') {
    Table_distribution=matrix(data =0, nrow = 1, ncol = (n+1))
    density<-BIG_pi_large_N
    distribution<-rep(0,n+1)
    distribution[1]=density[1]
    for (i in 2:(n+1)) {
      distribution[i]=distribution[i-1]+density[i]
    }
    Table_distribution[1,]<-distribution

    Table_result<-round(t(Table_distribution),8)
    rownames(Table_result)<-c("x=0",1:n)
    colnames(Table_result)<-c("Distribution")
    return(Table_result)

  } else if (output=='both') {
    Table_den<-round(t(BIG_pi_large_N),8)
    rownames(Table_den)<-c("x=0",1:n)
    colnames(Table_den)<-c("Density")

    Table_distribution=matrix(data =0, nrow = 1, ncol = (n+1))
    density<-BIG_pi_large_N
    distribution<-rep(0,n+1)
    distribution[1]=density[1]
    for (i in 2:(n+1)) {
      distribution[i]=distribution[i-1]+density[i]
    }
    Table_distribution[1,]<-distribution

    Table_dis<-round(t(Table_distribution),8)
    rownames(Table_dis)<-c("x=0",1:n)
    colnames(Table_dis)<-c("cdf")
    return(list(Table_den,Table_dis))
  } else {
    stop("No such output option")
  }




}


## Function for creating A table of hypergeometric distribution using chebyshey polynomials using rescaling
##  This is an algorithm for efficient and exact calculation of hypergeometric probabilities using
##  Chebyshev polynomials. For a fixed population size N and fixed sample size n, such calculations
##  simultaneously produce distributions for all possible values of the population number of "successes" M.
# Input population size N
# Input sample size n
# Output a matrix containing in row M=0,1,...,N, the required values of the hypergeometric probabilities
# (or distribution if output='distribution') indexed by the columns x=0,1,..,n.

hypergeotable<-function(N,n,output='density') {

  # Calculate small gamma matrix
  # small_gamma=matrix(data =0, nrow = (N+1), ncol = (n+1))

  Calculate_row_small_gamma<-function(r){
    Cheb_Polyn<-rep(0,n+1)
    x=r-1
    Cheb_Polyn[1]<-1
    Cheb_Polyn[2]<-1-2*x/N
    for (l in 3:(n+1)) {
      i<-l-2
      Cheb_Polyn[l]<-((2*i+1)*(N-2*x)*Cheb_Polyn[l-1]-i*(N+i+1)*Cheb_Polyn[l-2])/((i+1)*(N-i))
    }
    return(t(Cheb_Polyn))
  }

  small_gamma<-t(apply(t(t(1:(N+1))),1,Calculate_row_small_gamma))

  for (r in 1:(N+1)) {
    Cheb_Polyn<-rep(0,n+1)
    x=r-1
    Cheb_Polyn[1]<-1
    Cheb_Polyn[2]<-1-2*x/N
    for (l in 3:(n+1)) {
      i<-l-2
      Cheb_Polyn[l]<-((2*i+1)*(N-2*x)*Cheb_Polyn[l-1]-i*(N+i+1)*Cheb_Polyn[l-2])/((i+1)*(N-i))
    }
    small_gamma[r,]=Cheb_Polyn
  }

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


  big_gamma=matrix(data =0, nrow = (n+1), ncol = (n+1))
  for (c in 1:(n+1)) {
    i=c-1
    eps<-big_gamma_without_normalized[,c]
    norm_term<-t(eps)%*%eps
    big_gamma[,c]<-eps/(norm_term*exp((i-1)/2))

  }
  ##  Table of hypergeometric
  BIG_pi_large_N=small_gamma%*%t(big_gamma)


  if (output=='density') {
    Table_result<-round(BIG_pi_large_N,8)
    rownames(Table_result)<-c("Density M=0",1:N)
    colnames(Table_result)<-c("x=0",1:n)
    return(Table_result)
  } else if (output=='distribution') {
    Table_distribution=matrix(data =0, nrow = (N+1), ncol = (n+1))
    for (r in 1:(N+1)) {
      density<-BIG_pi_large_N[r,]
      distribution<-rep(0,n+1)
      distribution[1]=density[1]
      for (i in 2:(n+1)) {
        distribution[i]=distribution[i-1]+density[i]
      }
      Table_distribution[r,]<-distribution
    }
    Table_result<-round(Table_distribution,8)
    rownames(Table_result)<-c("Distribution M=0",1:N)
    colnames(Table_result)<-c("x=0",1:n)
    return(Table_result)

  } else if (output=='both') {
    Table_den<-round(BIG_pi_large_N,8)
    rownames(Table_den)<-c("Density M=0",1:N)
    colnames(Table_den)<-c("x=0",1:n)

    Table_distribution=matrix(data =0, nrow = (N+1), ncol = (n+1))
    for (r in 1:(N+1)) {
      density<-BIG_pi_large_N[r,]
      distribution<-rep(0,n+1)
      distribution[1]=density[1]
      for (i in 2:(n+1)) {
        distribution[i]=distribution[i-1]+density[i]
      }
      Table_distribution[r,]<-distribution
    }
    Table_dis<-round(Table_distribution,8)
    rownames(Table_dis)<-c("cdf M=0",1:N)
    colnames(Table_dis)<-c("x=0",1:n)
    return(list(Table_den,Table_dis))
  } else {
    stop("No such output option")
    }


}


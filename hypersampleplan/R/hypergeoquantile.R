## Function for creating A Quantile table of hypergeometric distribution using chebyshey polynomials
## For a fixed population size N and fixed sample size n, such calculations simultaneously produce Quantiles
## for all possible values of the population number of "successes" M.
# Input population size N
# Input sample size n
# Input quantile required q
# Output a matrix containing in row M=0,1,...,N, the required values of the hypergeometric quantiles
hypergeoquantile<-function(q,N,n) {

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
  # rownames(Table_result)<-c("Distribution M=0",1:N)
  # colnames(Table_result)<-c("x=0",1:n)
  # Table_result

  ## Finding Quantile
  current_quantile=0
  Table_Quantile=matrix(data =0, nrow = (N+1), ncol = 1)
  for (r in 1:(N+1)) {
    current_quantile=0
    for (c in 1:(n+1)){
      if (Table_result[r,c]>q) {
        break
      } else if (Table_result[r,c]==q) {
        current_quantile=c-1
        break
      } else {
        current_quantile=c-1
      }

    }
    Table_Quantile[r,1]=current_quantile

  }

  Table_Quantile
  rownames(Table_Quantile)<-c("M=0",1:N)
  colnames(Table_Quantile)<-paste("Quantile q=",q)
  return(Table_Quantile)
}

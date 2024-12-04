## Function for creating A Quantile table of binomial distribution using chebyshey polynomials
## For a fixed population size n and probability of "success" p, such calculations simultaneously produce Quantiles
## for all possible values of the number of "successes" x.
# Input population size n
# Probability of "success" p
# Input quantile required q
# Output the required values of the binomial quantiles
binomialquantile<-function(q,n,p) {

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

  Table_distribution=matrix(data =0, nrow = 1, ncol = (n+1))
  density<-BIG_pi_large_N
  distribution<-rep(0,n+1)
  distribution[1]=density[1]
  for (i in 2:(n+1)) {
    distribution[i]=distribution[i-1]+density[i]
  }
  Table_distribution[1,]<-distribution

  Table_result<-round(Table_distribution,8)

  ## Finding Quantile
  current_quantile=0
  Table_Quantile=matrix(data =0, nrow =1, ncol = 1)
    current_quantile=0
    for (c in 1:(n+1)){
      if (Table_result[1,c]>q) {
        break
      } else if (Table_result[1,c]==q) {
        current_quantile=c-1
        break
      } else {
        current_quantile=c-1
      }

    }
    Table_Quantile[1,1]=current_quantile

  Table_Quantile
  rownames(Table_Quantile)<-c("x")
  colnames(Table_Quantile)<-paste("Quantile q=",q)
  return(Table_Quantile)
}

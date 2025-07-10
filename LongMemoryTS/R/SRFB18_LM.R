##########################################################################################
####            Testing fractional cointegration                                   #######
####               Souza, Reisen, Franco, Bondon (2018)                            #######                     
##########################################################################################


#' @title Frequency-domain test for fractional cointegration (Souza, Reise, Franco, Bondon (2018))
#' @description \code{FCI_CH06} Semiparametric frequency-domain test for fractional cointegration by Souza, Reise, Franco, Bondon (2018).
#'  Returns test statistic, critical value, testing decision and estimate of the cointegrating strength. Null hypothesis: no fractional cointegration.
# #' @details add details here.
#' @param X bivariate data matrix.
#' @param d known common memory parameter. However, simulations indicate that consistent memory estimation does not invalidate the test.
#' @param m bandwith parameter specifying the number of Fourier frequencies
#' used for the estimation, usually \code{floor(1+T^delta)}, where 0<delta<1.
#' @param r integer trimming parameter, \code{r>0}.
#' @param alpha desired significance level. Default is \code{alpha=0.05}.
#' @references Souza, I. V. M., Reisen, V. A., Franco, G. d. C. and Bondon, P. (2018): The estimation and testing of
#' the cointegration order based on the frequency domain. Journal of Business & Economic Statistics, Vol. 36, No. 4, pp. 695 - 704.
#' @author Michelle Voges
#' @examples
#' T<-1000
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.1,0.7), B=rbind(c(1,-1),c(0,1)))
#' FCI_SRFB18(series, d=0.7, m=floor(T^0.75), r=1)
#' series<-FI.sim(T=T, q=2, rho=0.4, d=c(0.4,0.4))
#' FCI_SRFB18(series, d=0.4, m=floor(T^0.75), r=1)
#' @export

FCI_SRFB18<-function(X, d, m, r, alpha=0.05){

if (which.max(dim(X)) == 1) {X <- t(X)}        # change dimension of data if necessary
T<-ncol(X)                                     # number of observations
 if(nrow(X)>2)
    stop("Only bivariate data is allowed.")
diff_X <- apply(X, 1, diffseries, d)           # difference data with known (and equal) d

I_j <- Peri(diff_X)                            # calculate periodogram of differenced data

l<-r+1                                         # trimming (start at this Fourier frequency)
a<-c(0, seq(1:m))                              # helper for j determination
j<- c((l+a*(2*r+1))[(l+a*(2*r+1))<m],m)        # Frequencies considered
lambdaj<-2*pi*j/T                              # trimmed and truncated Fourier frequencies
mm<-length(j)                                  # number of frequencies considered
 
I_aux<-array(0, dim=c(2,2,mm))                 # helper
F_hat<-array(0, dim=c(2,2,mm))                 # helper
D_hat<-numeric(length=mm)                      # helper

for(h in 1:mm)                                # calculation of F_hat and D_hat over relevant frequencies
{ 
  jj<-j[h]                                                        # Frequencies
  for( ii in (jj-r):(jj+r)){ I_aux[,,h] <- I_aux[,,h] + I_j[,,ii]}# sum over Periodogram symmetric around j's
  F_hat[,,h] <- 1/(2*r+1) * I_aux[,,h]                            # spectral estimate (averaged sum of trimmed and truncated periodogram)
  D_hat[h] <- prod(eigen(F_hat[,,h])$values)                      # calculation of determinant of spectral estimate at each frequency,
}                                                                 # works for complex numbers in this case because anti diagonale is conjugate of each other

# needed for spectral estimate of b
Z_j<-log(2-2*cos(lambdaj))
Z_bar<-mean(Z_j)
Z_tilde<-Z_j-Z_bar
Z_tilde_2<-Z_tilde^2

b_hat<- as.numeric(solve(sum(Z_tilde_2)) * sum(Z_tilde*log(D_hat)))    # b estimate 
sigma<-solve(m)*(trigamma(2*r+1)+trigamma(2*r))            # variance of b_hat

teststat<-as.numeric(b_hat/sqrt(sigma))                                # teststatistic like t-test
krit<-qnorm(1-alpha/2)                                     # critical value from standard normal
dec <- teststat>krit
return(list("T"=teststat, "crit"=krit, "reject"=dec, "b_hat"=b_hat))
}

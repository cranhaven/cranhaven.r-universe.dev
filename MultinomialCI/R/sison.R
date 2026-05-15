##' Simultaneous Confidence Intervals for Multinomial Proportions
##'
##' Simultaneous confidence intervals for multinomial proportions, calculated according 
##' to the method of Sison and Glaz
##'
##' @param x A vector of positive integers representing the number of occurrences of each class. 
##'          The total number of samples equals the sum of such elements.
##' @param alpha The significance level for the confidence intervals. Must be a real number 
##'        in the interval [0, 1].
##' @param verbose A boolean flag indicating whether details should be printed to screen 
##' during the execution of the method. Defaults to FALSE.
##' @details Given a vector of observations with the number of samples falling in each class of a 
##' multinomial distribution, this function builds simultaneous confidence intervals for the 
##' multinomial probabilities according to the method proposed by Sison and Glaz (1995). 
##' The R code has been translated from the SAS code written by May and Johnson (2000).
##' @return A k x 2 real matrix, with k being the number of classes, which matches the 
##' length of the input vector x. Row iof the matrix contains the lower bound (first column) 
##' and upper bound (second column) defining the confidence interval for the probability 
##' of the i-th class, which corresponds to the i-th position of the input vector.
##' @references 
##' Sison, C.P and J. Glaz. Simultaneous confidence intervals and sample size determination
##' for multinomial proportions. Journal of the American Statistical Association, 90:366-369 (1995).
##' <doi:10.1080/01621459.1995.10476521>
##' 
##' Glaz, J. and Sison, C.P. Simultaneous confidence intervals for multinomial proportions. 
##' Journal of Statistical Planning and Inference 82:251-262 (1999). 
##' <doi:10.1016/S0378-3758(99)00047-6>
##' 
##' @examples 
##' # Multinomial distribution with 3 classes, from which 79 samples 
##' # were drawn: 23 of them belong to the first class, 12 to the 
##' # second class and 44 to the third class. Punctual estimations 
##' # of the probabilities from this sample would be 23/79, 12/79 
##' # and 44/79 but we want to build 95% simultaneous confidence intervals 
##' # for the true probabilities
##' m = multinomialCI(c(23,12,44), 0.05)
##' print(paste("First class: [", m[1,1], m[1,2], "]"))
##' print(paste("Second class: [", m[2,1], m[2,2], "]"))
##' print(paste("Third class: [", m[3,1], m[3,2], "]"))
##' @export
multinomialCI <- function(x,alpha,verbose=FALSE){
  
  n = sum(x, na.rm=TRUE)
  k = length(x)
  p = x/n
  c = 0
  pold=0
  for(cc in 1:n){
   p = .truncpoi(cc,x,n,k)
   if(p > 1-alpha && pold < 1-alpha) { 
     c = cc 
     break 
   }
   pold=p
  }

  salida = matrix(0,k,2)
  delta=(1-alpha-pold)/(p-pold)
  out=matrix(0,k,5)
  num=matrix(0,k,1)
  c=c-1  
  vol1=1
  vol2=1
  for(i in 1:k){
   num[i,1]=i
   obsp=x[i]/n
   out[i,1]=obsp
   out[i,2]=obsp-c/n
   out[i,3]=obsp+c/n+2*delta/n
   if(out[i,2]<0){ out[i,2]=0 }
   if(out[i,3]>1){ out[i,3]=1 }
   out[i,4]=obsp-c/n-1/n
   out[i,5]=obsp+c/n+1/n
   if(out[i,4]<0){ out[i,4]=0 }
   if(out[i,5]>1){ out[i,5]=1 }
   vol1=vol1*(out[i,3]-out[i,2])
   vol2=vol2*(out[i,5]-out[i,4])
   
   salida[i,1] = out[i,2]
   salida[i,2] = out[i,3]
  }
  c1=c('PROPORTION', 'LOWER(SG)','UPPER(SG)','LOWER(C+1)','UPPER(C+1)')
  cov=100*(1-alpha)
  sg=(x+delta)/n
  c2=c('SG-midpoint')
  if(verbose==TRUE){
    print('-------------------------------------------------------------')
    print(paste('    ',cov,'% SIMULTANEOUS CONFIDENCE INTERVALS'))
    print('       BASED ON THE METHODS OF SISON AND GLAZ')
    print('-------------------------------------------------------------')
    print(paste('C = ',c))
    print(paste('P(c+1) = ',p))
    print(paste('P(c)   = ',pold))
    print(paste('delta =  ',delta))
    print(paste('Volume(SG) = ',vol1))
    print(paste('Volume(C+1)= ',vol2))
    print(paste(c1))
    print(out)
    print(paste(c2))
    print(sg)
  }
  return(salida)
}
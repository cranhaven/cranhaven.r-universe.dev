
##########################################################################################
##                                                                                      ##
##                Partition (qx1) Vector into Subvectors with equal d                   ##
##                                                                                      ##
##########################################################################################

#library(longmemo)
#library(fracdiff)
#library(partitions)


#setwd("D:\\Dropbox\\Multivariate Breaks\\R codes")  
#setwd("C:\\Users\\leschinski\\Dropbox\\Multivariate Breaks\\R codes")
#setwd("C:\\Users\\Christian\\Dropbox\\Multivariate Breaks\\R codes")

#source("cointegration rank estimation.R")

#' @title Automated partitioning of estimated vector of long memory parameters into
#'        subvectors with equal memory.
#' @description \code{partition.X} conducts a sequence of tests for the equality of
#'              two or more estimated memory parameters to find possible partitions 
#'              of a vector into subvectors with equal memory parameters. 
#'              The procedure follows Robinson and Yajima (2002). 
#' @details add a lot of details.
#' @param data (Txq) data matrix
#' @param d.hat (qx1) vector of d-estimates obtained using a local Whittle method such as
#'        that described in Robinson (1995).
#' @param m the bandwidth parameter to be used for estimation of G
#' @param m1 the bandwidth parameter used for estimation of d.vec with m1>>m
#' @param alpha the desired significance level for the tests
#' @param report either \code{TRUE} or \code{FALSE} determining, whether information
#'        about the partitioning process should be printed to the user. Default is \code{report=FALSE}.
#' @import partitions
#' @examples
#' library(fracdiff)
#' T<-1000
#' d1<-0.2
#' d2<-0.4
#' X<-cbind(fracdiff.sim(n=T,d=d1)$series,fracdiff.sim(n=T,d=d1)$series,
#' fracdiff.sim(n=T,d=d2)$series,fracdiff.sim(n=T,d=d2)$series)
#' alpha<-0.05
#' m1<-floor(1+T^0.75)
#' m<-floor(1+T^0.65)
#' d.hat<-c(local.W(X[,1],m=m1)$d,local.W(X[,2],m=m1)$d,local.W(X[,3],m=m1)$d,local.W(X[,4],m=m1)$d)
#' partition.X(data=X, d.hat=d.hat, m=m, m1=m1, alpha=0.05, report=TRUE)
#' @references Robinson, P. M. (1995): Gaussian semiparametric estimation of long rang
#'              dependence. The Annals of Statistics, Vol. 23, No. 5, pp. 1630-1661.
#'            
#'            Robinson, P. M. and Yajima, Y. (2002): Determination of cointegrating rank
#'            in fractional systems. Journal of Econometrics, Vol. 106, No.2, pp. 217-241. 
#' @seealso \link{partitions}, \link{T.rho}, \link{T0stat}
#' @export


partition.X<-function(data,d.hat,m,m1,alpha=0.05, report=FALSE){

  # Partitioning of q dimensional vector X_t into subvectors with equal d
  # method described in Robinson and Yajima (2002) pp. 224-225 and 227
  
  # data is the (Txq) data matrix
  # d.hat is a (qx1) vector of d-estimates obtained using a local Whittle method such as
  #       those described in Robinson (1995b), Shimotsu and Phillipps (2007)
  # m is the bandwidth parameter used for estimation of G
  # m1 is the bandwidth parameter used for estimation of d.vec with m1>>m
  # alpha is the desired significance level
  
X<-data
q<-min(dim(data))
T<-max(dim(data))

parts_mat<-setparts(q)                        # setparts() from partions finds all possible partitions of the data vector
n_combs<-ncol(parts_mat)-1                    # n_combs is number of all partitions that contain a subset with more than one element
n_subsets<-apply(parts_mat,2,max)             # count number of subsets in every partition

decisions<-rep(NA,n_combs)
test_d<-NULL
p.vals<-NULL

###################       Go through all possible partitions        ##############################

for(i in 1:n_combs){
  
  # stop if number of subsets in new partition increases and there was a non-rejection before
  if(i>1){if(n_subsets[i]>n_subsets[(i-1)]){if(any(decisions[which(n_subsets==n_subsets[(i-1)])]==0)){break}}}
  
  # calculate s' for every partition
  n_l<-0
  for(j in 1:n_subsets[i]){
  vec<-parts_mat[,i]
  n_l[j]<-sum(vec==j)
  }
  s_bar<-sum(n_l>1)

  # Test all T_{\rho_l} in current partition for every subset with more than one element
  rej<-0
  p.vals.subvec<-NULL
  for(j in 1:n_subsets[i]){    
    select<-arrayInd(which(vec==j), .dim=dim(as.matrix(vec)))[,1]                          # which dimensions of X_t are in current subset 
    if(length(select)>1){
      data<-X[,select]                                                                     # extract relevant subvector from X_t
      if(length(select)>2){                                                                # use T0 stat for subvectors with more than 2 elements
      test_d<-T0stat(data=data, d.hat=d.hat[select], m=m, m1=m1, s_bar=s_bar)
      }else{
      test_d<-T.rho(data=data, d.hat=d.hat[select], s_bar=s_bar, m=m,m1=m1)}               # Test T_{\rho_l} based on max of abs(T_{ab})
      if(test_d$p.val<test_d$level){rej[j]<-1}else{rej[j]<-0}                              # evaluate whether hypothesis is rejected
      p.vals.subvec[j]<-test_d$p.val
    }
  }
  if(report==TRUE){
  cat("-------------------------- ","Iteration=",i," --------------------------", "\n")
  cat("\n")
  cat("partition:", vec, "\n")
  cat("rej:", rej, "\n")
  #cat("p.vals.subvec:", p.vals.subvec, "\n")
  #cat("\n")
  }
  p.vals[i]<-prod(p.vals.subvec)
  if(any(rej==1)){decisions[i]<-1}else{decisions[i]<-0}
  
}
if(report==TRUE){cat("Results:","\n","\n")}
return(list("partition"=parts_mat[,which(na.omit(decisions)==0)])) #, "p.vals"=p.vals[which(na.omit(decisions)==0)]
}


####################################################################################

#T<-1000
#d1<-0.2
#d2<-0.4
#X<-cbind(fracdiff.sim(n=T,d=d1)$series,fracdiff.sim(n=T,d=d1)$series,fracdiff.sim(n=T,d=d2)$series,fracdiff.sim(n=T,d=d2)$series)
#alpha<-0.05
#m1<-floor(1+T^0.75)
#m<-floor(1+T^0.65)
#d.hat<-apply(X,2,Hou.Perron, m=m1) 
#partition.X(data=X, d.hat=d.hat, m=m, m1=m1, alpha=0.05, report=TRUE)


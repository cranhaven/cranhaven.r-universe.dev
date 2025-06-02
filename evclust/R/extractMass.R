#' Creates an object of class "credpart"
#'
#' \code{extractMass} computes different ouputs (hard, fuzzy, rough partions, etc.)
#' from a credal partition and creates an object of class "credpart".
#'
#' This function collects varied information on a credal partition and stores it in
#' an object of class "credpart". The lower and upper
#' approximations of clusters define rough partitions. They can be computed in two ways:
#' either from the set of clusters with maximum mass, or from the set of non dominated clusters.
#' A cluster \eqn{\omega_k} is non dominated if \eqn{pl(\omega_k)\ge bel(\omega_l)} for
#' all l different from k. Once a set of cluster \eqn{Y_i} has been computed for each object,
#' object i belongs to the lower approximation of cluster k if \eqn{Y_i={\omega_k}}. It
#' belongs to the upper approximation of cluster k if \eqn{\omega_k \in Y_i}. See
#' Masson and Denoeux (2008) for more details, and Denoeux and Kanjanatarakul (2016) for
#' the interval dominance rule. The function creates an object of class \code{"credpart"}.
#' There are three methods for this class: \code{\link{plot.credpart}},
#' \code{\link{summary.credpart}} and \code{\link{predict.credpart}}.
#'
#'
#' @param mass A credal partition (a matrix of n rows and f columns, where n is the
#' number of objects and f is the number of focal sets).
#' @param F Matrix (f,c) of focal sets. If the empty set is a focal set, it must correspond to
#' the first row of F. 
#' @param g A c x d matrix of prototypes.
#' @param S A list of length f containing the matrices \eqn{S_j} defining the metrics for each cluster and
#' each group of cluster.
#' @param method The method used to construct the credal partition (a character string).
#' @param crit The value of the optimized criterion (depends on the method used).
#' @param Kmat The matrix of degrees of conflict. Same size as D (for method \code{\link{kevclus}}).
#' @param D The normalized dissimilarity matrix (for method \code{\link{kevclus}}).
#' @param trace The trace of criterion values  (for methods \code{\link{kevclus}} and
#' \code{\link{EkNNclus}}).
#' @param W The weight matrix (for method \code{\link{EkNNclus}}).
#' @param J The matrix of indices (for method \code{\link{kevclus}}).
#' @param param A method-dependent list of parameters.
#'
#' @return An object of class "credpart"  with the following components:
#' \describe{
#' \item{method}{The method used to construct the credal partition (a character string).}
#' \item{F}{Matrix of focal sets. The first row always corresponds to the empty set.}
#' \item{conf}{Masses assigned to the empty set, vector of length n.}
#' \item{mass}{Mass functions, matrix of size (n,f).}
#' \item{mass.n}{Normalized mass functions, matrix of size (n,f-1).}
#' \item{g}{The prototypes (if defined).}
#' \item{S}{The matrices \eqn{S_j} defining the metrics for each cluster and each group of cluster
#' (if defined).}
#' \item{pl}{Unnormalized plausibilities of the singletons, matrix of size (n,c).}
#' \item{pl.n}{Normalized plausibilities of the singletons, matrix of size (n,c).}
#' \item{p}{Probabilities derived from pl by the plausibility transformation, matrix of size (n,c).}
#' \item{bel}{Unnormalized beliefs of the singletons, matrix of size (n,c).}
#' \item{bel.n}{Normalized beliefs of the singletons, matrix of size (n,c).}
#' \item{y.pl}{Maximum plausibility clusters, vector of length n.}
#' \item{y.bel}{Maximum belief clusters, vector of length n.}
#' \item{betp}{Unnormalized pignistic probabilities of the singletons, matrix of size (n,c).}
#' \item{betp.n}{Normalized pignistic probabilities of the singletons, matrix of size (n,c).}
#' \item{Y}{Sets of clusters with maximum mass, matrix of size (n,c).}
#' \item{outlier}{n-vector of 0's and 1's, indicating which objects are outliers. An outlier
#' is an object such that the largest mass is assigned to the empty set.}
#' \item{lower.approx}{Lower approximations of clusters, a list of length c.
#'      Each element lower.approx[[i]] is a vector of object indices.}
#' \item{upper.approx}{Upper approximations of clusters, a list of length c.
#'      Each element upper.approx[[i]] is a vector of object indices.}
#' \item{Ynd}{Sets of clusters selected by the interval dominance rule, matrix of size (n,c).}
#' \item{lower.approx.nd}{Lower approximations of clusters using the interval dominance rule,
#' a list of length c. Each element lower.approx.nd[[i]] is a vector of objects.}
#' \item{upper.approx.nd}{Upper approximations of clusters using the interval dominance rule,
#' a list of length c. Each element upper.approx.nd[[i]] is a vector of objects.}
#' \item{N}{Average nonspecificity.}
#' \item{crit}{The value of the optimized criterion (depends on the method used).}
#' \item{Kmat}{The matrix of degrees of conflict. Same size as D (for method \code{\link{kevclus}}).}
#' \item{D}{The normalized dissimilarity matrix (for method \code{\link{kevclus}}).}
#' \item{trace}{The trace of criterion values  (for methods \code{\link{kevclus}} and
#' \code{\link{EkNNclus}}).}
#' \item{W}{The weight matrix (for method \code{\link{EkNNclus}}).}
#' \item{J}{The matrix of indices (for method \code{\link{kevclus}}).}
#' \item{param}{A method-dependent list of parameters.}
#' }
#' @export
#'
#' @seealso \code{\link{plot.credpart}}, \code{\link{summary.credpart}}
#'
#' @references
#'  T. Denoeux and O. Kanjanatarakul. Beyond Fuzzy, Possibilistic and Rough: An
#' Investigation of Belief Functions in Clustering. 8th International conference on soft
#' methods in probability and statistics, Rome, 12-14 September, 2016.
#'
#' M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy c-means algorithm.
#' Pattern Recognition, Vol. 41, Issue 4, pages 1384-1397, 2008.
#'
#' @examples
#' \dontrun{
#' ## Four-class data
#' data(fourclass)
#' x<-fourclass[,1:2]
#' y<-fourclass[,3]
#' D<-as.matrix(dist(x))^2
#' clus<-recm(D,c=4,delta=10,ntrials=1)
#' summary(clus)
#' plot(clus,X=x,mfrow=c(1,1),ytrue=y,Outliers=TRUE)
#' }
extractMass <- function(mass,F,g=NULL,S=NULL,method,crit=NULL,Kmat=NULL,trace=NULL,
                        D=NULL,W=NULL,J=NULL,param=NULL){

  n<-nrow(mass)
  c<-ncol(F)
  if(any(F[1,]==1)){
    F<-rbind(rep(0,c),F) # add the empty set
    mass<-cbind(rep(0,n),mass)
  }
  f<-nrow(F)
  card<-rowSums(F)

  conf<-mass[,1]             # degree of conflict
  C<- 1/(1-conf)
  mass.n<- C*mass[,2:f]   # normalized mass function
  pl<- mass%*% F          # unnormalized plausibility
  pl.n<- C*pl             # normalized plausibility
  p <-pl/rowSums(pl)      # plausibility-derived probability
  bel<- mass[,card==1]    # unnormalized belief
  bel.n<-C*bel            # normalized belief
  y.pl<-max.col(pl)       # maximum plausibility cluster
  y.bel<-max.col(bel)     # maximum belief cluster
  Y<-F[max.col(mass),]    # maximum mass set of clusters
  # non dominated elements
  Ynd<-matrix(0,n,c)
  for (i in 1:n){
    ii<-which(pl[i,]>= bel[i,y.bel[i]])
    Ynd[i,ii]<-1
  }

  P<-F/card
  P[1,]<- rep(0,c)
  betp<- mass %*% P       # unnormalized pignistic probability
  betp.n<- C* betp        # normalized pignistic probability

  lower.approx<-vector(mode='list',length=c)
  upper.approx<-vector(mode='list',length=c)
  lower.approx.nd<-vector(mode='list',length=c)
  upper.approx.nd<-vector(mode='list',length=c)
  nclus<-rowSums(Y)
  outlier<-which(nclus==0) # outliers
  nclus.nd<-rowSums(Ynd)
  for(i in 1:c){
    upper.approx[[i]]<- which(Y[,i]==1)                 # upper approximation
    lower.approx[[i]]<- which((Y[,i]==1) & (nclus==1))  # upper approximation
    upper.approx.nd[[i]]<- which(Ynd[,i]==1)                 # upper approximation
    lower.approx.nd[[i]]<- which((Ynd[,i]==1) & (nclus.nd==1))  # upper approximation
  }
  # Nonspecificity
  card<-c(c,card[2:f])
  Card<-matrix(card,n,f,byrow=TRUE)
  N <- sum(log(Card)*mass)/log(c)/n
  clus<-list(conf=conf,F=F,mass=mass,mass.n=mass.n,pl=pl,pl.n=pl.n,bel=bel,bel.n=bel.n,y.pl=y.pl,
             y.bel=y.bel,Y=Y,betp=betp,betp.n=betp.n,p=p,upper.approx=upper.approx,
             lower.approx=lower.approx,Ynd=Ynd,upper.approx.nd=upper.approx.nd,
             lower.approx.nd=lower.approx.nd,N=N,outlier=outlier,g=g,S=S,
             crit=crit,Kmat=Kmat,trace=trace,D=D,method=method,W=W,J=J,param=param)
  class(clus)<-"credpart"
  return(clus)
}

#' k-EVCLUS algorithm
#'
#'\code{kevclus} computes a credal partition from a dissimilarity matrix using the k-EVCLUS
#'algorithm.
#'
#' This version of the EVCLUS algorithm uses the Iterative Row-wise Quadratic Programming
#' (IRQP) algorithm (see ter Braak et al., 2009). It also makes it possible to use only
#' a random sample of the dissimilarities, reducing the time and space complexity from
#' quadratic to roughly linear (Denoeux et al., 2016). The user must supply:
#' 1) a matrix x or size (n,p) containing the values of p attributes for n objects, or
#' 2) a matrix D of size (n,n) of dissimilarities between n objects, or
#' 3) a matrix D of size (n,k) of dissimilarities between the n objects and k randomly selected
#' objects, AND a matrix J of size (n,k) of indices, such that D[i,j] is the distance between
#' objects i and J[i,j].
#' In cases 1 and 2, the user may supply the number $k$ of distances to be picked randomly for
#' each object. In case 3, k is set to the number of columns of D.
#'
#' @param x nxp matrix of p attributes observed for n objects (optional).
#' @param k Number of distances to compute for each object (default: n-1).
#' @param D nxn or nxk dissimilarity matrix (used only of x is not supplied).
#' @param J nxk matrix of indices. D[i,j] is the distance between objects i and
#' J[i,j]. (Used only if D is supplied and ncol(D)<n; then k is set to ncol(D).)
#' @param c Number of clusters
#' @param type Type of focal sets ("simple": empty set, singletons and Omega;
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all or selected pairs).
#' @param pairs Set of pairs to be included in the focal sets; if NULL, all pairs are
#' included. Used only if type="pairs".
#' @param m0 Initial credal partition. Should be a matrix with n rows and a number of
#' columns equal to the number f of focal sets specified by 'type' and 'pairs'.
#' @param ntrials Number of runs of the optimization algorithm (set to 1 if m0 is supplied
#' and change.order=FALSE).
#' @param disp If TRUE (default), intermediate results are displayed.
#' @param maxit Maximum number of iterations.
#' @param epsi Minimum amount of improvement.
#' @param d0 Parameter used for matrix normalization. The normalized distance corresponding
#' to d0 is 0.95.
#' @param tr If TRUE, a trace of the stress function is returned.
#' @param change.order If TRUE, the order of objects is changed at each iteration of the
#' Iterative Row-wise Quadratic Programming (IRQP) algorithm.
#' @param norm Normalization of distances. 1: division by mean(D^2) (default); 2: division par n*p.
#'
#' @return The credal partition (an object of class \code{"credpart"}). In addition to the
#' usual attributes, the output credal partition has the following attributes:
#'  \describe{
#'   \item{Kmat}{The matrix of degrees of conflict. Same size as D.}
#'   \item{D}{The normalized dissimilarity matrix.}
#'   \item{trace}{Trace of the algorithm (Stress function vs iterations).}
#'   \item{J}{The matrix of indices.}
#'  }
#'
#'
#'@references T. Denoeux and M.-H. Masson. EVCLUS: Evidential Clustering of Proximity Data.
#'IEEE Transactions on Systems, Man and Cybernetics B, Vol. 34, Issue 1, 95--109, 2004.
#'
#'T. Denoeux, S. Sriboonchitta and O. Kanjanatarakul. Evidential clustering of large
#'dissimilarity data. Knowledge-Based Systems, vol. 106, pages 179-195, 2016.
#'
#'C. J. ter Braak, Y. Kourmpetis, H. A. Kiers, and M. C. Bink. Approximating a
#'similarity matrix by a latent class model: A reappraisal of additive fuzzy clustering.
#'Computational Statistics & Data Analysis, 53(8):3183--3193, 2009.
#'
#'
#'@author Thierry Denoeux.
#'
#' @export
#' @import limSolve
#' @importFrom stats runif quantile
#'
#' @seealso \code{\link{createD}}, \code{\link{makeF}}, \code{\link{extractMass}}
#'
#' @examples ## Example with a non metric dissimilarity matrix: the Protein dataset
#'\dontrun{
#' data(protein)
#' clus <- kevclus(D=protein$D,c=4,type='simple',d0=max(protein$D))
#' z<- cmdscale(protein$D,k=2)  # Computation of 2 attributes by Multidimensional Scaling
#' plot(clus,X=z,mfrow=c(2,2),ytrue=protein$y,Outliers=FALSE,Approx=1)
#' ## Example with k=30
#' clus <- kevclus(D=protein$D,k=30,c=4,type='simple',d0=max(protein$D))
#' z<- cmdscale(protein$D,k=2)  # Computation of 2 attributes by Multidimensional Scaling
#' plot(clus,X=z,mfrow=c(2,2),ytrue=protein$y,Outliers=FALSE,Approx=1)
#' }
#'
kevclus<-function(x,k=n-1,D,J,c,type='simple',pairs=NULL,m0=NULL,ntrials=1,disp=TRUE,maxit=1000,
                     epsi=1e-5,d0=quantile(D,0.9),tr=FALSE,change.order=FALSE,norm=1){

  if(!missing(x)){ # x is supplied
    x<-as.matrix(x)
    n<-nrow(x)
    if(k==(n-1)){ # k takes default value
      D<-as.matrix(dist(x))
      J<-matrix(0,n,n-1)
      D1<-J
      for(i in 1:n){
        J[i,]<-(1:n)[-i]
        D1[i,]<-D[i,J[i,]]
      } # end for
      D<-D1
      p<-n-1
    } else{ #k<n-1
      dist<-createD(x,k)
      D<-dist$D
      J<-dist$J
      p<-k
      }
  } else if(!missing(D)){ # D is supplied
    D<-as.matrix(D)
    n<-nrow(D)
    p<-ncol(D)
    if((n==p) & (k==(n-1))){ # D is square and k takes default value
      J<-matrix(0,n,n-1)
      D1<-J
      for(i in 1:n){
        J[i,]<-(1:n)[-i]
        D1[i,]<-D[i,J[i,]]
      } # end for
      D<-D1
      p<-n-1
    } else if((n==p) & (k<n)){ # D is square and k<n
      D1<-matrix(0,n,k)
      J<-D1
      for(i in 1:n){
        ii<-sample((1:n)[-i],k)
        J[i,]<-ii
        D1[i,]<-D[i,ii]
      } # end for
      D<-D1
      p<-k
    } else k<-p #  D and J are supplied
  } else{  # neither x nor D is supplied: ERROR
    print('ERROR: x or D must be supplied')
    return()
  }



  if((ntrials>1) & !is.null(m0) & !change.order){
    print('WARNING: ntrials>1 and m0 provided. Parameter ntrials set to 1.')
    ntrials<-1
  }

 # distance normalization
  g=-log(0.05)/d0^2
  D<-1-exp(-g*D^2)
  if(norm==1) C<-1/sum(D^2) else C<-1/(n*p) 
 
  F<-makeF(c=c,type=type,pairs=pairs)
  f<-nrow(F)
  xi<-matrix(0,f,f)  # the matrix used to compute the degrees of conflict
  for(i in 1:f){
    for(j in 1:f){
      xi[i,j] <- 1-max(pmin(F[i,],F[j,]))
    }
  }

  I<-(1:n)
  Sbest=Inf
  for(N in 1:ntrials){
    if(missing(m0)){
      mass <- matrix(runif(n*f),n,f)
      mass<-mass/rowSums(mass)
    } else {
      mass<-m0
    }
    K<-matrix(0,n,p)
    for(i in 1:n){
      K[i,]=mass[i,] %*% xi %*% t(mass[J[i,],])
    }
    Spred<- C*sum((K-D)^2)
#    inc <- Inf
    gain <- 1
    E<-rep(1,f)
    Feq<-1
    G<-diag(f)
    H<-rep(0,f)
    k<-0
    # Trace
    if(tr){
      Trace<-list(temps=matrix(0,maxit+1,3),fct=rep(0,maxit+1))
      Trace$temps[1,]<-c(0,0,0)
      Trace$fct[1]<-Spred
      ptm<-proc.time()[1:3]
    } else Trace<-NULL
    #----
    while((gain>epsi) & (k<=maxit)){
      S<-0
      k<-k+1
      if(change.order) I<-sample(n,n)
      for(i in 1:n){
        A<-mass[J[I[i],],] %*% xi
        B<- D[I[i],]
        opt<-lsei(A,B,E,Feq,G,H,type=2)
        mass[I[i],]<-opt$X
        S<-S+opt$solutionNorm
        }  # end of 'for' loop on objects
      S<-C*S
      #---
      if(tr){
        Trace$temps[k+1,]<-proc.time()[1:3]-ptm
        Trace$fct[k+1]<-S
      }
      #---
      if(disp) print(sprintf("% i %i %e %e",N,k,S,gain))
      gain <- 0.5*gain+0.5*abs(Spred-S)/(1e-9+abs(Spred))
      Spred<-S
      }  # end of while loop on epochs
    if(S<Sbest) {
      mass.best<-mass
      Sbest<-S
      Tracebest<-Trace
    }
    if(disp){
      if(ntrials>1) print(c(N,S,Sbest)) else print(c(N,S))
    }
  } # end 'for' loop on trials

  for(i in 1:n){
    K[i,]=mass.best[i,] %*% xi %*% t(mass.best[J[i,],])
  }

  clus<-extractMass(mass.best,F,method="kevclus",crit=Sbest,Kmat=K,D=D,
                    trace=Tracebest,J=J)
  return(clus)

}






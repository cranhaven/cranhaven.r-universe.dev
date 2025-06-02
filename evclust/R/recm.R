#' Relational Evidential c-means algorithm
#'
#'\code{recm} computes a credal partition from a dissimilarity matrix using the
#'Relational Evidential c-means (RECM) algorithm.
#'
#'RECM is a relational version of the Evidential c-Means (ECM) algorithm. Convergence is
#'guaranteed only if elements of matrix D are squared Euclidean distances. However, the
#'algorithm is quite robust and generally provides sensible results even if the dissimilarities
#'are not metric. By default, each mass function in the credal partition has \eqn{2^c} focal sets,
#'where c is the supplied number of clusters. We can also limit the number of focal sets to
#'subsets of clusters with cardinalities 0, 1 and c (recommended if c>=10), or to all or some
#'selected pairs of clusters.
#'If an initial credal partition m0 is provided, the number of trials is automatically set to 1.
#'
#' @param D Dissimilarity matrix of size (n,n), where n is the number of objects.
#' Dissimilarities must be squared Euclidean distances to ensure convergence.
#' @param c Number of  clusters.
#' @param type Type of focal sets ("simple": empty set, singletons and Omega;
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all or selected pairs).
#' @param Omega Logical. If TRUE (default), the whole frame is included (for types 'simple' and
#' 'pairs').
#' @param pairs Set of pairs to be included in the focal sets; if NULL, all pairs are
#' included. Used only if type="pairs".
#' @param m0 Initial credal partition. Should be a matrix with n rows and a number of
#' columns equal to the number f of focal sets specified by 'type' and 'pairs'.
#' @param ntrials Number of runs of the optimization algorithm (set to 1 if m0 is  supplied).
#' @param alpha Exponent of the cardinality in the cost function.
#' @param beta Exponent of masses in the cost function.
#' @param delta Distance to the empty set.
#' @param epsi Minimum amount of improvement.
#' @param maxit Maximum number of iterations.
#' @param disp If TRUE (default), intermediate results are displayed.
#'
#' @return The credal partition (an object of class \code{"credpart"}).
#'
#'@references M.-H. Masson and T. Denoeux. RECM: Relational Evidential c-means algorithm.
#'Pattern Recognition Letters, Vol. 30, pages 1015--1026, 2009.
#'
#'@author Thierry Denoeux (from a MATLAB code written by Marie-Helene Masson).
#'
#' @export
#' @importFrom stats quantile runif
#'
#' @seealso \code{\link{makeF}}, \code{\link{extractMass}}, \code{\link{ecm}}
#'
#' @examples ## Clustering of the Butterfly dataset
#' \dontrun{
#' n <- nrow(butterfly)
#' D<-as.matrix(dist(butterfly))^2
#' clus<-recm(D,c=2,delta=sqrt(50))
#' m<-clus$mass
#' plot(1:n,m[,1],type="l",ylim=c(0,1),xlab="objects",ylab="masses")
#' lines(1:n,m[,2],lty=2)
#' lines(1:n,m[,3],lty=3)
#' lines(1:n,m[,4],lty=4)
#'
#'  ## Clustering the protein data
#'  data(protein)
#'  clus <- recm(D=protein$D,c=4,type='full',alpha=0.2,beta=1.1,delta=sqrt(20))
#'
#'  z<- cmdscale(protein$D,k=2)
#'  plot(clus,X=z,mfrow=c(2,2),ytrue=protein$y,Outliers=FALSE,Approx=1)
#'  }

recm<- function(D,c,type='full',pairs=NULL,Omega=TRUE,m0=NULL,ntrials=1,alpha=1,beta=1.5,
                delta=quantile(D[upper.tri(D)|lower.tri(D)],0.95),epsi=1e-4,maxit=5000,
                disp=TRUE){

if((ntrials>1) & !is.null(m0)){
  print('WARNING: ntrials>1 and m0 provided. Parameter ntrials set to 1.')
  ntrials<-1
  }
D<-as.matrix(D)
# Scalar products computation from distances

delta2<-delta^2
names(delta2)<-NULL

n<-ncol(D)
e <- rep(1,n)
Q <- diag(n)-e%*%t(e)/n
XX <- -0.5 * Q %*% D %*% Q


# Initializations
F<-makeF(c,type,pairs,Omega)
f<-nrow(F)
card<- rowSums(F[2:f,])

if(!is.null(m0)){
  if((nrow(m0)!= n) | (ncol(m0)!=f)){
    stop("ERROR: dimension of m0 is not compatible with specified focal sets")
  }
}

# Optimization

Jbest<-Inf
for(itrial in 1:ntrials){
  if(is.null(m0)){
    m <- matrix(runif(n*(f-1)),n,f-1)
    m <- m /rowSums(m)
  } else m<-m0[,2:f]

  pasfini<-TRUE
  Jold <- Inf
  Mold<- matrix(1e9,n,f)
  it <- 0
  while(pasfini & (it < maxit)){
    it <- it + 1
    # Construction of the H matrix
    H <- matrix(0,c,c)
    for(k in 1:c){
      for(l in 1:c){
        truc <- rep(0,c)
        truc[c(k,l)]<-1
        t <- matrix(truc,f,c,byrow=TRUE)
        indices <- which(rowSums((F-t)-abs(F-t))==0)    # indices of all Aj including wk and wl
        indices <- indices - 1
        if(length(indices)==0) H[l,k]<-0 else{
          for(jj in 1:length(indices)){
            j <- indices[jj]
            mj <- m[,j]^beta
            H[l,k]<-H[l,k]+sum(mj)*card[j]^(alpha-2)
          }
        }
      }
    }
    # Construction of the U matrix
    U<-matrix(0,c,n)
    for(l in 1:c){
      truc <- rep(0,c)
      truc[l]<-1
      t <- matrix(truc,f,c,byrow=TRUE)
      indices <- which(rowSums((F-t)-abs(F-t))==0)   # indices of all Aj including wl
      indices <- indices - 1
      mi <- matrix(card[indices]^(alpha-1),n,length(indices),byrow=TRUE) * m[,indices]^beta
      U[l,] <- rowSums(mi)
    }
    B <- U %*% XX
    VX<-solve(H,B)
    B <- U %*% t(VX)
    VV <- solve(H,B)

    # distances to focal elements
    D <- matrix(0,n,f-1)
    for(i in 1:n){
      for(j in 1:(f-1)){
        ff <- F[j+1,]
        truc <- ff %*% t(ff) # indices of pairs (wk,wl) in Aj
        indices <- which(ff==1) # indices of classes in Aj
        D[i,j]<- XX[i,i]-2*sum(VX[indices,i])/card[j]+sum(truc*VV)/(card[j]^2)
      }
    }
    # masses
    m <- matrix(0,n,f-1)
    for(i in 1:n){
      vect0 <- D[i,]
      for(j in 1:(f-1)){
        vect1 <- (rep(D[i,j],f-1)/vect0) ^(1/(beta-1))
        vect2 <-  rep(card[j]^(alpha/(beta-1)),f-1) /(card^(alpha/(beta-1)))
        vect3 <- vect1 * vect2
        m[i,j]<- 1/(  sum(vect3) + (card[j]^alpha * D[i,j]/delta2)^(1/(beta-1))  )
      }
    }
    mvide <- 1-rowSums(m)
    M <- cbind(m,mvide)
    J = sum((m^beta)*D*matrix(card^alpha,n,f-1,byrow=TRUE))+ delta2*sum(mvide^beta)
    DeltaM<-norm(M-Mold,type='f')/n/f
    if(disp) print(c(J,DeltaM))
    pasfini <- (DeltaM > epsi)
    Mold <- M
    Jold <- J
  } # end of 'while' loop
  if(J<Jbest){
    Jbest<-J
    mbest<-m
  }
  res<-c(itrial,J,Jbest)
  names(res)<-NULL
  if (ntrials>1) print(res)
}


# add mass to the empty set
m <- cbind(1-rowSums(mbest),mbest)

clus<-extractMass(m,F,method="recm",crit=Jbest)
return(clus)
}


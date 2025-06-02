#' Belief Peak Evidential Clustering (BPEC)
#'
#'\code{bpec} computes a credal partition from a matrix of attribute data using the
#' Belief Peak Evidential Clustering (BPEC) algorithm.
#'
#'BPEC is identical to ECM, except that the prototypes are computed from delta-Bel graph using function
#'\code{delta_Bel}. The ECM algorithm is then run keeping the prototypes fixed. The distance to the 
#'prototypes can be the Euclidean disatnce or it can be an adaptive Mahalanobis distance as in the CECM
#'algorithm.
#'
#' @param x input matrix of size n x d, where n is the number of objects and d the number of
#' attributes.
#' @param g Matrix of size c x d of prototypes (the belief peaks). 
#' @param type Type of focal sets ("simple": empty set, singletons and Omega;
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all
#' or selected pairs).
#' @param Omega Logical. If TRUE (default), the whole frame is included (for types 'simple' and
#' 'pairs').
#' @param pairs Set of pairs to be included in the focal sets; if NULL, all pairs are
#' included. Used only if type="pairs".
#' @param alpha Exponent of the cardinality in the cost function.
#' @param beta Exponent of masses in the cost function.
#' @param delta Distance to the empty set.
#' @param epsi Minimum amount of improvement.
#' @param disp If TRUE (default), intermediate results are displayed.
#' @param distance Type of distance use: 0=Euclidean, 1=Mahalanobis.
#' @param m0 Initial credal partition. Should be a matrix with n rows and a number of
#' columns equal to the number f of focal sets specified by 'type' and 'pairs'.
#'
#' @return The credal partition (an object of class \code{"credpart"}).
#'
#'@references Z.-G. Su and T. Denoeux. BPEC: Belief-Peaks Evidential Clustering. IEEE Transactions 
#'on Fuzzy Systems, 27(1):111-123, 2019.
#'
#'@author Thierry Denoeux.
#'
#' @export
#'
#' @seealso \code{\link{ecm}}, \code{\link{cecm}}, \code{\link{delta_Bel}}
#'
#' @examples ## Clustering of the Four-class dataset
#' \dontrun{
#' data(fourclass)
#' x<-fourclass[,1:2]
#' y<-fourclass[,3]
#' DB<-delta_Bel(x,100,0.9)
#' plot(x,pch=".")
#' points(DB$g0,pch=3,col="red",cex=2)
#' clus<-bpec(x,DB$g0,type='pairs',delta=3,distance=1)
#' plot(clus,x,mfrow=c(2,2))
#' }
bpec<-function(x,g,type='full',pairs=NULL,Omega=TRUE,alpha=1,
               beta=2,delta=10,epsi=1e-3,disp=TRUE,distance=1,m0=NULL){
  
  #---------------------- initialisations --------------------------------------
  
  x<-as.matrix(x)
  n <-nrow(x)
  d <- ncol(x)
  delta2<-delta^2
  c<-nrow(g)
  
  F<-makeF(c,type,pairs,Omega)
  f<-nrow(F)
  card<- rowSums(F[2:f,])
  
  #------------------------ iterations--------------------------------
  pasfini<-TRUE
  gplus<-matrix(0,f-1,d)
  iter<-0
  for(i in 2:f){
    fi <- F[i,]
    truc <- matrix(fi,c,d)
    gplus[i-1,] <- colSums(g*truc)/sum(fi)
  }
  # calculation of Euclidean distances to centers
  D<-matrix(0,n,f-1)
  for(j in 1:f-1){
    D[,j]<- rowSums((x-matrix(gplus[j,],n,d,byrow = TRUE))^2)
  }
  # Calculation of masses
  if(is.null(m0)){
    m <- matrix(0,n,f-1)
    for(i in 1:n){
      vect0 <- D[i,]
      for(j in 1:(f-1)){
        vect1 <- (rep(D[i,j],f-1)/vect0) ^(1/(beta-1))
        vect2 <-  rep(card[j]^(alpha/(beta-1)),f-1) /(card^(alpha/(beta-1)))
        vect3 <- vect1 * vect2
        m[i,j]<- 1/(sum(vect3) + (card[j]^alpha * D[i,j]/delta2)^(1/(beta-1)))
        if(is.nan(m[i,j])) m[i,j]<-1
      }
    }
  } else m<-m0[,2:f]
  mvide <- 1-rowSums(m)
  Jold <- sum((m^beta)*D*matrix(card^alpha,n,f-1,byrow=TRUE))+ 
    delta2*sum(mvide^beta)
  if(disp) print(c(iter,Jold))
  while(pasfini){
    iter<-iter+1
    dist<-setDistances(x,F,g,m,alpha,distance=distance)
    D<-dist$D
    Smeans<-dist$Smean
    # Calculation of masses
    for(i in 1:n){
      vect0 <- D[i,]
      for(j in 1:(f-1)){
        vect1 <- (rep(D[i,j],f-1)/vect0) ^(1/(beta-1))
        vect2 <-  rep(card[j]^(alpha/(beta-1)),f-1) /(card^(alpha/(beta-1)))
        vect3 <- vect1 * vect2
        m[i,j]<- 1/(  sum(vect3) + (card[j]^alpha * D[i,j]/delta2)^(1/(beta-1)))
        if(is.nan(m[i,j])) m[i,j]<-1
      }
    }
    mvide <- 1-rowSums(m)
    J <- sum((m^beta)*D*matrix(card^alpha,n,f-1,byrow=TRUE))+ delta2*sum(mvide^beta)
    if(disp) print(c(iter,J))
    pasfini <- (abs(J-Jold)>epsi)
    Jold <- J
    
  } # end while loop
  
  m <- cbind(mvide,m)
  clus<-extractMass(m,F,g=g,method="bepc",crit=J,param=list(alpha=alpha,beta=beta,
                   delta=delta,S=Smeans))
  return(clus)
}




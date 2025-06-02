#' Evidential c-means algorithm
#'
#'\code{ecm} computes a credal partition from a matrix of attribute data using the
#' Evidential c-means (ECM) algorithm.
#'
#'ECM is an evidential version algorithm of the Hard c-Means (HCM) and Fuzzy c-Means (FCM)
#'algorithms. As in HCM and FCM, each cluster is represented by a prototype. However, in ECM,
#'some sets of clusters are also represented by a prototype, which is defined as the center of mass
#'of the prototypes in each individual cluster. The algorithm iteratively optimizes a cost
#'function, with respect to the prototypes and to the credal partition. By default, each mass
#'function in the credal partition has \eqn{2^c} focal sets, where c is the supplied number of
#'clusters. We can also limit the number of focal sets to
#'subsets of clusters with cardinalities 0, 1 and c (recommended if c>=10), or to all or some
#'selected pairs of clusters.
#'If  initial prototypes g0 are provided, the number of trials is automatically set to 1.
#'
#'
#' @param x input matrix of size n x d, where n is the number of objects and d the number of
#' attributes.
#' @param c Number of  clusters.
#' @param g0 Initial prototypes, matrix of size c x d. If not supplied, the prototypes are
#' initialized randomly.
#' @param type Type of focal sets ("simple": empty set, singletons and Omega;
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all
#' or selected pairs).
#' @param Omega Logical. If TRUE (default), the whole frame is included (for types 'simple' and
#' 'pairs').
#' @param pairs Set of pairs to be included in the focal sets; if NULL, all pairs are
#' included. Used only if type="pairs".
#' @param ntrials Number of runs of the optimization algorithm (set to 1 if m0 is  supplied).
#' @param alpha Exponent of the cardinality in the cost function.
#' @param beta Exponent of masses in the cost function.
#' @param delta Distance to the empty set.
#' @param epsi Minimum amount of improvement.
#' @param init Initialization: "kmeans" (default) or "rand" (random).
#' @param disp If TRUE (default), intermediate results are displayed.
#'
#' @return The credal partition (an object of class \code{"credpart"}).
#'
#'@references M.-H. Masson and T. Denoeux. ECM: An evidential version of the fuzzy
#'c-means algorithm. Pattern Recognition, Vol. 41, Issue 4, pages 1384--1397, 2008.
#'
#'@author Thierry Denoeux (from a MATLAB code written by Marie-Helene Masson).
#'
#' @export
#' @importFrom stats rnorm kmeans
#'
#' @seealso \code{\link{makeF}}, \code{\link{extractMass}}, \code{\link{recm}}, \code{\link{cecm}},
#' \code{\link{plot.credpart}}
#'
#' @examples ## Clustering of the Four-class dataset
#' \dontrun{
#' data(fourclass)
#' x<-fourclass[,1:2]
#' y<-fourclass[,3]
#' clus<-ecm(x,c=4,type='full',alpha=1,beta=2,delta=sqrt(20),epsi=1e-3,disp=TRUE)
#' plot(clus,X=x,mfrow=c(2,2),ytrue=y,Outliers=TRUE,Approx=2)
#' }
ecm<-function(x,c,g0=NULL,type='full',pairs=NULL,Omega=TRUE,ntrials=1,alpha=1,beta=2,delta=10,
              epsi=1e-3,init="kmeans",disp=TRUE){

  #---------------------- initialisations --------------------------------------

  x<-as.matrix(x)
  n <-nrow(x)
  d <- ncol(x)
  delta2<-delta^2

  if((ntrials>1) & !is.null(g0)){
    print('WARNING: ntrials>1 and g0 provided. Parameter ntrials set to 1.')
    ntrials<-1
  }

  F<-makeF(c,type,pairs,Omega)
  f<-nrow(F)
  card<- rowSums(F[2:f,])

  #------------------------ iterations--------------------------------
  Jbest<-Inf
  for(itrial in 1:ntrials){
    if(is.null(g0)){
      if(init=="kmeans") g<-kmeans(x,c)$centers else g <- x[sample(1:n,c),]+0.1*rnorm(c*d,c,d)
    } else g<- g0
    pasfini<-TRUE
    Jold <- Inf
    gplus<-matrix(0,f-1,d)
    iter<-0
    while(pasfini){
      iter<-iter+1
      for(i in 2:f){
        fi <- F[i,]
        truc <- matrix(fi,c,d)
        gplus[i-1,] <- colSums(g*truc)/sum(fi)
      }

      # calculation of distances to centers
      D<-matrix(0,n,f-1)
      for(j in 1:(f-1)) D[,j]<- rowSums((x-matrix(gplus[j,],n,d,byrow = TRUE))^2)

      # Calculation of masses
      m <- matrix(0,n,f-1)
      for(i in 1:n){
        vect0 <- D[i,]
        for(j in 1:(f-1)){
          vect1 <- (rep(D[i,j],f-1)/vect0) ^(1/(beta-1))
          vect2 <-  rep(card[j]^(alpha/(beta-1)),f-1) /(card^(alpha/(beta-1)))
          vect3 <- vect1 * vect2
          m[i,j]<- 1/(  sum(vect3) + (card[j]^alpha * D[i,j]/delta2)^(1/(beta-1))  )
          if(is.nan(m[i,j])) m[i,j]<-1 # in case the initial prototypes are training vectors
        }
      }

      # Calculation of centers
      A <- matrix(0,c,c)
      for(k in 1:c){
        for(l in 1:c){
          truc <- rep(0,c)
          truc[c(k,l)]<-1
          t <- matrix(truc,f,c,byrow=TRUE)
          indices <- which(rowSums((F-t)-abs(F-t))==0)    # indices of all Aj including wk and wl
          indices <- indices - 1
          if(length(indices)==0) A[l,k]<-0 else{
            for(jj in 1:length(indices)){
              j <- indices[jj]
              mj <- m[,j]^beta
              A[l,k]<-A[l,k]+sum(mj)*card[j]^(alpha-2)
            }
          }
        }
      }

      # Construction of the B matrix
      B<-matrix(0,c,d)
      for(l in 1:c){
        truc <- rep(0,c)
        truc[l]<-1
        t <- matrix(truc,f,c,byrow=TRUE)
        indices <- which(rowSums((F-t)-abs(F-t))==0)   # indices of all Aj including wl
        indices <- indices - 1
        mi <- matrix(card[indices]^(alpha-1),n,length(indices),byrow=TRUE) * m[,indices]^beta
        s <- rowSums(mi)
        mats <- matrix(s,n,d)
        xim <- x*mats
        B[l,]<- colSums(xim)
      }

      g<-solve(A,B)

      mvide <- 1-rowSums(m)
      J <- sum((m^beta)*D*matrix(card^alpha,n,f-1,byrow=TRUE))+ delta2*sum(mvide^beta)
      if(disp) print(c(iter,J))
      pasfini <- (abs(J-Jold)>epsi)
      Jold <- J

    } # end while loop
    if(J<Jbest){
      Jbest<-J
      mbest<-m
      gbest<-g
    }
    res<-c(itrial,J,Jbest)
    names(res)<-NULL
    if (ntrials>1) print(res)
  } #end for loop iter

  m <- cbind(1-rowSums(mbest),mbest)
  clus<-extractMass(m,F,g=gbest,method="ecm",crit=Jbest,
                    param=list(alpha=alpha,beta=beta,delta=delta))
  return(clus)
}




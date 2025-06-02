#' k-CEVCLUS algorithm
#'
#'\code{kcevclus} computes a credal partition from a dissimilarity matrix and pairwise (must-link
#' and cannot-link) constraints using the k-CEVCLUS algorithm.
#'
#' k-CEVCLUS is a version of EVCLUS allowing the user to specify pairwise constraints to guide
#' the clustering process. Pairwise constraints are of two kinds: must-link contraints are
#' pairs of objects that are known to belong to the same class, and cannot-link constraints
#' are pairs of objects that are known to belong to different classes. As \code{kevclus}, 
#' \code{kcevclus} uses the Iterative Row-wise Quadratic Programming (IRQP) algorithm 
#' (see ter Braak et al., 2009). It also makes it possible to use only a random sample of the 
#' dissimilarities, reducing the time and space complexity from quadratic to roughly linear 
#' (Denoeux et al., 2016). 
#'
#' @param x nxp matrix of p attributes observed for n objects (optional).
#' @param k Number of distances to compute for each object (default: n-1).
#' @param D nxn or nxk dissimilarity matrix (used only of x is not supplied).
#' @param J nxk matrix of indices. D[i,j] is the distance between objects i and
#' J[i,j]. (Used only if D is supplied and ncol(D)<n; then k is set to ncol(D).)
#' @param c Number of clusters
#' @param ML Matrix nbML x 2 of must-link constraints. Each row of ML contains the indices
#' of objects that belong to the same class.
#' @param CL Matrix nbCL x 2 of cannot-link constraints. Each row of CL contains the indices
#' of objects that belong to different classes.
#' @param xi Penalization coefficient.
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
#'@references F. Li, S. Li and T. Denoeux. k-CEVCLUS: Constrained evidential clustering of 
#'large dissimilarity data. Knowledge-Based Systems 142:29-44, 2018.
#'
#'T. Denoeux, S. Sriboonchitta and O. Kanjanatarakul. Evidential clustering of large
#'dissimilarity data. Knowledge-Based Systems 106:179-195, 2016.
#'
#'V. Antoine, B. Quost, M.-H. Masson and T. Denoeux. CEVCLUS: Evidential clustering 
#'with instance-level constraints for relational data. Soft Computing 18(7):1321-1335, 2014.
#'
#'C. J. ter Braak, Y. Kourmpetis, H. A. Kiers, and M. C. Bink. Approximating a
#'similarity matrix by a latent class model: A reappraisal of additive fuzzy clustering.
#'Computational Statistics & Data Analysis 53(8):3183--3193, 2009.
#'
#'@author Feng Li and Thierry Denoeux.
#'
#' @export
#' @import limSolve
#' @importFrom stats runif quantile
#'
#' @seealso \code{\link{kevclus}},\code{\link{createD}}, \code{\link{makeF}}, 
#' \code{\link{extractMass}}, \code{\link{create_MLCL}},\code{\link{bananas}},
#' \code{\link{nnevclus}}
#' 
#' @examples 
#'\dontrun{
#' data<-bananas(2000)
#' D<-as.matrix(dist(data$x))
#' link<-create_MLCL(data$y,2000)
#' clus0<-kevclus(D=D,k=200,c=2)
#' clus1<-kcevclus(D=D,k=200,c=2,ML=link2$ML,CL=link2$CL,Xi=0.1,m0=clus0$mass)
#' clus2<-kcevclus(D=D,k=200,c=2,ML=link2$ML,CL=link2$CL,Xi=0.5,m0=clus1$mass)
#' plot(clus2,X=data$x,ytrue=data$y,Outliers=FALSE,Approx=1)
#' }
#'

kcevclus<-function (x,k=n-1,D,J,c,ML,CL,xi=0.5,type="simple",pairs=NULL,m0=NULL, 
          ntrials=1,disp=TRUE,maxit=1000,epsi=1e-05,d0 = quantile(D,0.9),
          tr=FALSE,change.order=FALSE,norm=1) {
  Xi<-xi
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
    if((n==p) & (k==n)){ # D is square and k takes default value
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
  if ((ntrials > 1) & !is.null(m0) & !change.order) {
    print("WARNING: ntrials>1 and m0 provided. Parameter ntrials set to 1.")
    ntrials <- 1
  }
  g = -log(0.05)/d0^2
  D <- 1 - exp(-g * D^2)
  if(norm==1) C<-1/sum(D^2) else C<-1/(n*p) 
  F <- makeF(c = c, type = type, pairs = pairs)
  f <- nrow(F)
  C1 <- matrix(0, f, f)
  for (i in 1:f) {
    for (j in 1:f) {
      C1[i, j] <- 1 - max(pmin(F[i, ], F[j, ]))
    }
  }
  
  #define C1 matrix
  emptysetPos<-rowSums(C1)==f
  B<-matrix(0,f,f)
  if(any(emptysetPos)){
    B[emptysetPos,]<-1
    B[,emptysetPos]<-1
  }
  DD<-diag(rowSums(F)==1)
  C2<-1-B-DD
  #define B D C2 matrix 
  
  I <- (1:n)
  Sbest = Inf
  tol<-sqrt(.Machine$double.eps)#addtional change
  for (N in 1:ntrials) {
    if (missing(m0)) {#random initialization
      mass <- matrix(runif(n * f), n, f)
      mass <- mass/rowSums(mass)
    }
    else {
      mass <- m0
    }
    K <- matrix(0, n, p)
    for (i in 1:n) {
      K[i, ] = mass[i, ] %*% C1 %*% t(mass[J[i, ], ])
    }
    #add constrains to the objective function
 #   ML<-matrix(link$ML,ncol=2)
 #   CL<-matrix(link$CL,ncol=2)
    JM<-diag(mass[ML[,1],]%*%(C1+C2)%*%drop(t(mass[ML[,2],])))#drop is used to avoid ML[,1] only has one value
    JC<-diag(mass[CL[,1],]%*%(1-C1+1-C2)%*%drop(t(mass[CL[,2],])))
   
    if(length(ML)>0|length(CL)>0){
      deta<-2*Xi/(length(ML)+length(CL))
    }else{
      deta<-0
    }
    Spred <- C* sum((K - D)^2)+2*deta*(sum(JM)+sum(JC))#compute stress function
    
    gain <- 1
    Amat <- t(rbind(matrix(1,1,f),diag(f)))
    bvec <- c(1,rep(0,f))
    k <- 0
    if (tr) {
      Trace <- list(temps = matrix(0, maxit + 1, 3), fct = rep(0, 
                                                               maxit + 1))
      Trace$temps[1, ] <- c(0, 0, 0)
      Trace$fct[1] <- Spred
      ptm <- proc.time()[1:3]
    }else Trace <- NULL
    while ((gain > epsi) & (k <= maxit)) {
      S <- 0
      k <- k + 1
      if (change.order) 
        I <- sample(n, n)
      for (i in 1:n) {
        #if(i==108){print(i)}
        #Solve.QP function solves quadratic programming problems of the form min(-t(d) * b + 1/2 t(b)*D*b) 
        #with the constraints t(A)*b >= b_0.
        Dmat <- C*t(C1)%*%t(mass[J[I[i], ], ])%*%mass[J[I[i], ], ]%*% C1#D matrix
        diag(Dmat)<-diag(Dmat)+1e-08 #addtional change
        partone<-C*D[I[i],]%*%mass[J[I[i], ], ]%*%C1#d vector has three part 
        ###################################d = partone -0.5*deta*parttwo-0.5*deta*partthree
        #1/2*g(m_i)=1/2*(C*||M(-i)*C1*m_i-D(-i)||^2+delta*M(m)_i*(C1+C2)*m_i+delta*M(c)_i(1-C1+1-C2)*mi)
        ############deduce the partone#########parttwo################partthree
        posM<-which(ML[,1:2]==I[i],arr.ind = T)
        if(length(posM)>0){
          if(length(posM)==1){#only one ML
            posM<-1
          }else{
            posM<-posM[,1]
          }
          MLpos<-matrix(ML[posM,],nrow=length(posM))
          MLpos<-as.vector(t(MLpos))
          MLpos<-MLpos[MLpos!=I[i]]
          parttwo<-mass[MLpos,]%*%(C1+C2)
          if(nrow(parttwo)>1)
            parttwo<-apply(parttwo,2,sum)
        }
        else{
          parttwo<-rep(0,f)
        }
        
        posC<-which(CL[,1:2]==I[i],arr.ind = T)
        if(length(posC)>0){
          if(length(posC)==1){
            posC<-1
          }else{
            posC<-posC[,1]
          }
          CLpos<-matrix(CL[posC,],nrow=length(posC))
          CLpos<-as.vector(t(CLpos))
          CLpos<-CLpos[CLpos!=I[i]]
          partthree<-mass[CLpos,]%*%(1-C1+1-C2)
          if(nrow(partthree)>1)
            partthree<-apply(partthree,2,sum)
        }else{
          partthree<-rep(0,f)
        }
        
        dvec <- partone-0.5*deta*parttwo-0.5*deta*partthree
        dvec<-as.vector(dvec)
        opt <- solve.QP(Dmat,dvec,Amat,bvec = bvec,meq=1)#obtain the results
        sol<-opt$solution
        #sol<-round(sol,5)
        sol[which(abs(sol)<tol)]<-0 #addtional change
        mass[I[i], ] <- sol
        S<-S+C*sum((mass[J[I[i], ], ]%*%C1%*%sol-D[I[i],])^2)+deta*(parttwo%*%sol+partthree%*%sol)
      }
      #K2 <- matrix(0, n, p)
      #for (i in 1:n) {
      #  K2[i, ] = mass[i, ] %*% C1 %*% t(mass[J[i, ], ])
      #}
      #JM<-diag(mass[ML[,1],]%*%(C1+C2)%*%drop(t(mass[ML[,2],])))
      #JC<-diag(mass[CL[,1],]%*%(1-C1+1-C2)%*%drop(t(mass[CL[,2],])))
      #S0 <- C* sum((K2 - D)^2)+2*deta*(sum(JM)+sum(JC))
      #print(S0)
      if (tr) {
        Trace$temps[k + 1, ] <- proc.time()[1:3] - ptm
        Trace$fct[k + 1] <- S
      }
      if (disp) print(sprintf("% i %i %e %e", N, k, Spred, gain))
      gain <- 0.5 * gain + 0.5 * abs(Spred - S)/(1e-09 + abs(Spred))
      Spred <- S
    }
    if (S < Sbest) {
      mass.best <- mass
      Sbest <- S
      Tracebest <- Trace
    }
    if (disp) print(c(N, S, Sbest))
  }
  for (i in 1:n) {
    K[i, ] = mass.best[i, ] %*% C1 %*% t(mass.best[J[i, ], ])
  }
  clus <- extractMass(mass.best, F, method = "kcevclus", crit = Sbest, 
                      Kmat = K, D = D, trace = Tracebest,J=J)
  return(clus)
}
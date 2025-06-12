

#' Sequential Co-Sparse Factor Regression
#'
#' Sequential factor extraction via co-sparse unit-rank estimation (SeCURE)
#'
#' @param Y response matrix
#' @param X covariate matrix; when X = NULL, the fucntion performs unsupervised learning
#' @param nrank an integer specifying the desired rank/number of factors
#' @param nlambda number of lambda values to be used along each path
#' @param U0 initial value of U
#' @param V0 initial value of V
#' @param D0 initial value of D
#' @param orthXU if TRUE, orthogonality of XU is required
#' @param orthV if TRUE, orthogonality of V is required
#' @param keepPath if TRUE, th solution paths of U, V, D are reported
#' @param control a list of internal parameters controlling the model fitting
#' @param ic character specifying which information criterion to use for selecting the tuning parameter: "GIC"(default), "BICP", and "AIC"
#' @aliases secure
#' @return 
#'   \item{C.est}{estimated coefficient matrix; based on modified BIC}
#'   \item{U}{estimated U matrix (factor weights)}
#'   \item{D}{estimated singular values}
#'   \item{V}{estimated V matrix (factor loadings)}
#'   \item{ortX}{if TRUE, X is treated as an orthogonal matrix in the computation}
#'   \item{lam}{selected lambda values based on the chosen information criterion}
#'   \item{lampath}{sequences of lambda values used in model fitting. In each sequential unit-rank estimation step,
#'   a sequence of length nlambda is first generated between (lamMax*lamMaxFac, lamMax*lamMaxFac*lamMinFac) equally 
#'   spaced on the log scale, in which lamMax is estimated and the other parameters are specified in secure.control.
#'   The model fitting starts from the largest lambda and stops when the maximum proportion of nonzero elements is reached in 
#'   either u or v, as specified by spU and spV in secure.control.}
#'   \item{IC}{values of information criteria}
#'   \item{Upath}{solution path of U}
#'   \item{Dpath}{solution path of D}
#'   \item{Vpath}{solution path of D}
#' @export
#'@importFrom Rcpp evalCpp
#'@useDynLib secure
#' @examples
#' #require(secure)
#' 
#' # Simulate data from a sparse factor regression model
#' p <- 100; q <- 100; n <- 200
#' xrho <- 0.5; nlambda <- 100 
#' nrank <- 3 
#' 
#' U <- matrix(0,ncol=nrank ,nrow=p);  V <- matrix(0,ncol=nrank ,nrow=q)
#' U[,1]<-c(sample(c(1,-1),8,replace=TRUE),rep(0,p-8))
#' U[,2]<-c(rep(0,5),sample(c(1,-1),9,replace=TRUE),rep(0,p-14))
#' U[,3]<-c(rep(0,11),sample(c(1,-1),9,replace=TRUE),rep(0,p-20))
#' V[,1]<-c(sample(c(1,-1),5,replace=TRUE)*runif(5,0.3,1),rep(0,q-5))
#' V[,2]<-c(rep(0,5),sample(c(1,-1),5,replace=TRUE)*runif(5,0.3,1),rep(0,q-10))
#' V[,3]<-c(rep(0,10),sample(c(1,-1),5,replace=TRUE)*runif(5,0.3,1),rep(0,q-15))
#' U[,1:3]<- apply(U[,1:3],2,function(x)x/sqrt(sum(x^2)))
#' V[,1:3]<- apply(V[,1:3],2,function(x)x/sqrt(sum(x^2)))
#' D <- diag(c(20,15,10)) 
#' C <- U%*%D%*%t(V)
#' 
#' Xsigma <- xrho^abs(outer(1:p, 1:p,FUN="-"))
#' sim.sample <- secure.sim(U,D,V,n,snr = 0.25,Xsigma,rho=0.3)
#' Y <- sim.sample$Y; 
#' X <- sim.sample$X
#' 
#' 
#' 
#' # Fitting secure. Set maximum rank to be 4.
#' rank.ini <- 4
#' 
#' # Set largest model to about 25% sparsity
#' # See secure.control for setting other parameters
#' control <- secure.control(spU=0.25, spV=0.25)
#' 
#' # Complete data case. 
#' # Fit secure without orthogonality
#' fit.orthF <- secure.path(Y,X,nrank=rank.ini,nlambda = nlambda,
#'                         control=control)
#' # check orthogonality
#' crossprod(X%*%fit.orthF$U)/n
#' # check solution
#' # fit.orthF$U
#' # fit.orthF$V
#' # fit.orthF$D
#' 
#' # Fit secure with orthogonality if desired. It takes longer time.
#' # fit.orthT <- secure.path(Y,X,nrank=rank.ini,nlambda = nlambda,
#' #                                   orthXU=TRUE,orthV=TRUE,control=control)
#' # check orthogonality
#' # crossprod(X%*%fit.orthT$U)/n
#' 
#'   
#' # 15% missing case
#' miss <- 0.15
#' t.ind <- sample.int(n*q, size = miss*n*q)
#' y <- as.vector(Y); y[t.ind] <- NA;  Ym <- matrix(y,n,q)
#' 
#' fit.orthF.miss <- secure.path(Ym, X, nrank = rank.ini, nlambda = nlambda, 
#'                             control = control) 
#' # fit.orthT.miss <- secure.path(Ym, X, nrank = rank.ini, nlambda = nlambda,
#' #                           orthXU=TRUE,orthV=TRUE, control = control)
#'@references 
#' Mishra, A., Dey, D., Chen, K. (2017) \emph{ Sequential Co-Sparse Factor Regression, To appear in Journal of Computational and Graphical Statistics (JCGS)}
secure.path  = function(Y, X = NULL, nrank = 3, nlambda = 100, 
                       U0 = NULL, V0 = NULL, D0 = NULL, 
                       orthXU = FALSE, orthV = FALSE, 
                       keepPath = TRUE, control = list(),
                       ic = c("GIC","BICP", "AIC")[1]){
  # control <- secure.control()
  # U0 = NULL; V0 = NULL; D0 = NULL
  # nlambda = 100;orthXU=TRUE;orthV=TRUE
  # check for orthogonality 
  control <- do.call("secure.control", control)
  ic <- match.arg(ic, c("GIC","BICP", "AIC"))
  IC <- match(ic, c("GIC","BICP", "AIC"))
  
  cat("Initializing...", "\n")
  
  n <- nrow(Y); isnX <- is.null(X)
  if(isnX)   X <- diag(n) # {X <- sqrt(n)*diag(n);Y <- sqrt(n)*Y;} # 
  p <- ncol(X) ;  q <- ncol(Y)  
  tXX <- crossprod(X)/n;
  if((sum(abs(tXX))-sum(diag(tXX))) < 1e-7 ) {ort <- TRUE} else {ort <- FALSE }
  
  naInd <- !is.na(Y) 
  if(sum(!naInd)==0) {missInd <- FALSE} else {missInd <- TRUE}
  

  if(missInd) {
    ini <- secure.miss.init(Y,X,U0 = U0, V0 = V0, D0 = D0,nrank=nrank,control= control,method=2)
  } else {
      ini <- secure.init(Y,X,U0 = U0, V0 = V0, D0 = D0,nrank=nrank,ort) 
    }
    
  ## Solution path
  ICpath <- array(dim=c(4,nlambda+1,nrank),NA)   ## 3 corresponds to BIC, BICP, GIC, AIC
  lampath <- matrix(nrow=nlambda+1, ncol=nrank,NA)
  ExcutTimepath <- matrix(nrow=nlambda+1, ncol=nrank,NA)
  lamCountpath <- rep(NA, nrank)
  
  Upath <- array(dim=c(p,nlambda+1,nrank),NA)
  Vpath <- array(dim=c(q,nlambda+1,nrank),NA)
  Dpath <- matrix(nrow=nlambda+1, ncol=nrank,NA)
  
  ## Final solution selected by IC
  U <- matrix(0,nrow=p, ncol=nrank)
  V <- matrix(0,nrow=q, ncol=nrank)  
  D <- rep(0, nrank)
  lam <- rep(0, nrank)      

  Yk <- Y;totTime<-0
  if(ort) tXX <- diag(tXX) #else tXX <- crossprod(X)
  for(k in 1:nrank){      # k<-2
    
    if(k==1) cat("Starting...", "\n")
    uk <- ini$U0[,k];vk <- ini$V0[,k];dk <- ini$D0[k]
    if(k ==1){
      Au <- t(rep(0,p));Av <- t(rep(0,q));bu<-0; bv<-0
    }else{
      Yk <- Yk - Y.hat
      if(ort) Au <- t(U[,1:(k-1)] * tXX) else Au <- crossprod(U[,1:(k-1)],tXX)
      Av <- t(V[,1:(k-1)]) 
      if(!orthXU) Au <- 0*Au
      if(!orthV) Av <- 0*Av
      bu<-matrix(0,k-1); bv<-matrix(0,k-1)
    }
    if(missInd) {
      Yk[!naInd] <- 0
      fit.layer <- secure_SURR_miss_Rcpp(X, Yk,naInd+0,Au, Av, bu, bv, uk,vk,dk, nlambda,control)
#       
#       Yk[!naInd] <- NA
#       lay.est <- surr.sea.breg.missing.elpen(FALSE,k,X,Yk,Au,Av,bu,bv,ini$U0,ini$V0,diag(ini$D0),param)
      
    } else {
      if(ort){
        fit.layer <- secure_SURR_Rcpp_ortho(X, Yk, Au, Av, bu, bv, uk, vk, dk, nlambda, control)
      } else {
        fit.layer <- secure_SURR_Rcpp(X, Yk, Au, Av, bu, bv, uk, vk, dk, nlambda, control)
      }
    }

    # lay.est <- surr.sea.breg.enet(FALSE,k,X,Yk,Au,Av,bu,bv,ini$U0,ini$V0,diag(ini$D0),param)
    
    # Storing solution path variable 
    nlamk <- fit.layer$nkpath
    lamCountpath[k] <- nlamk
    ICpath[,1:nlamk,k] <- fit.layer$ICkpath[,1:nlamk]   ## 4 corresponds to BIC, BICP, GIC, AIC
    lampath[1:nlamk,k] <- fit.layer$lamkpath[1:nlamk]
    ExcutTimepath[1:nlamk,k]  <- fit.layer$ExecTimekpath[1:nlamk]
    
    Upath[,1:nlamk,k] <- fit.layer$ukpath[,1:nlamk]
    Vpath[,1:nlamk,k] <- fit.layer$vkpath[,1:nlamk]
    Dpath[1:nlamk,k] <- fit.layer$dkpath[1:nlamk] 
    
    # model selection of solution 
    if(IC == 1){
      if(p<n) ind.select <- which.min(ICpath[1,1:nlamk,k]) else ind.select <- which.min(ICpath[3,1:nlamk,k])
    } else if(IC == 2){
      if(p<n) ind.select <- which.min(ICpath[1,1:nlamk,k]) else ind.select <- which.min(ICpath[2,1:nlamk,k])
    } else { # "AIC"
      ind.select <- which.min(ICpath[4,1:nlamk,k]) 
    }

    
    # if(p<n) ind.select <- which.min(ICpath[1,1:nlamk,k]) else ind.select <- which.min(ICpath[2,1:nlamk,k])
    
    cat("factor", k, 
        ":  #lambdas fitted =", nlamk, 
        "  selected index =", ind.select, 
        " time =", round(sum(fit.layer$ExecTimekpath[1:nlamk]),3),
        "\n")
    
    U[,k] <- Upath[,ind.select,k]
    V[,k] <- Vpath[,ind.select,k]
    D[k] <- Dpath[ind.select,k] 
    lam[k] <- lampath[ind.select,k] 
    totTime <- totTime + ExcutTimepath[ind.select,k]
    
    Ck <- D[k]*tcrossprod(U[,k],V[,k])
    Y.hat <- crossprod(t(X),Ck)
    if(D[k]==0) {
      U <- U[,1:(k-1)] 
      V <- V[,1:(k-1)]
      D <- D[1:(k-1)]
      lam <- lam[1:(k-1)]
      ICpath <- ICpath[,,1:(k-1)]
      Upath <- Upath[,,1:(k-1)]
      Vpath <- Vpath[,,1:(k-1)]
      Dpath <- Dpath[,1:(k-1)]
      totTime <- totTime- ExcutTimepath[ind.select,k]
      break;
    }
  } 
  
  cat("Estimated rank =",sum(D!=0),"\n")
  if(sum(D!=0)==nrank){
    cat("Increase nrank value!")
  }
    
  D = diag(D, nrow=length(D), ncol=length(D))
 
  if(keepPath){
    return(list(C.est = U %*% D %*% t(V), 
              U = U, V = V, D = D, ortX = ort,##tlam = totTime,
              lam = lam, 
              lampath = lampath,
              IC = ICpath, 
              Upath = Upath, 
              Vpath = Vpath, 
              Dpath = Dpath))
  }else{
    return(list(C.est = U %*% D %*% t(V), 
                U = U, V = V, D = D, ortX = ort,##tlam = totTime,
                lam = lam))    
  }
  
  
}



#'Simulation model
#'
#' genertate random samples from a sparse factor regression model
#'
#' @param U specified value of U
#' @param V specified value of V
#' @param D specified value of D
#' @param n sample size
#' @param snr signal to noise ratio
#' @param Xsigma covariance matrix for generating sample of X
#' @param rho parameter defining correlated error
#' @return 
#'   \item{Y}{Generated response matrix}
#'   \item{X}{Generated predictor matrix}
#' @export
#' @importFrom MASS ginv
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
#' @examples
#' #require(secure)
#' 
#' # Simulate data from a sparse factor regression model
#' p <- 100; q <- 50; n <- 300
#' snr <- 0.5; ssigma <- 0.5; nlambda <- 200 
#' nrank <- 3
#' 
#' U <- matrix(0,ncol=nrank ,nrow=p);  V <- matrix(0,ncol=nrank ,nrow=q)
#' U[,1]<-c(sample(c(1,-1),8,replace=TRUE),rep(0,p-8))
#' U[,2]<-c(rep(0,5),sample(c(1,-1),9,replace=TRUE),rep(0,p-14))
#' U[,3]<-c(rep(0,11),sample(c(1,-1),9,replace=TRUE),rep(0,p-20))
#' V[,1]<-c(sample(c(1,-1),5,replace=TRUE)*runif(5,0.3,1),rep(0,q-5))
#' V[,2]<-c(rep(0,5),sample(c(1,-1),5,replace=TRUE)*runif(5,0.3,1),rep(0,q-10))
#' V[,3]<-c(rep(0,10),sample(c(1,-1),5,replace=TRUE)*runif(5,0.3,1),rep(0,q-15))
#' U[,1:3]<- apply(U[,1:3],2,function(x)x/sqrt(sum(x^2)))
#' V[,1:3]<- apply(V[,1:3],2,function(x)x/sqrt(sum(x^2)))
#' D <- diag(c(20,15,10)) 
#' C <- U%*%D%*%t(V)
#' 
#' Xsigma <- ssigma^abs(outer(1:p, 1:p,FUN="-"))
#' sim.sample <- secure.sim(U,D,V,n,snr,Xsigma)
#' Y <- sim.sample$Y
#' X <- sim.sample$X
#' 
secure.sim = function(U,D,V,n,snr,Xsigma,rho=0){
  
  ## finding basis along more number of columns of data vector 
  basis.vec =function(x){
    # require(Matrix)
    if(diff(dim(x))<0) x <- t(x)
    qd <- qr(x)
    k <- qr.Q(qd) %*% qr.R(qd)[,1:qd$rank]
    k[abs(k)<1e-6] <- 0
    b.ind <- vector()
    for(i in 1:qd$rank)
      b.ind[i] <- which(apply(x,2,function(x,y)sum(abs(x-y)),k[,i])<1e-6)[1]
    return(list(ind=b.ind,vec = x[,b.ind]))
  }
  
  p <- nrow(U);q <- nrow(V);nrank <- ncol(U)
  
  U.t <- diag(max(dim(U)))
  U.t <- U.t[,-basis.vec(U)$ind]
  P <- cbind(U,U.t)
  UtXsUt <- t(U.t)%*%Xsigma%*%U.t
  UtXsU <- t(U.t)%*%Xsigma%*%U
  UXsU <- t(U)%*%Xsigma%*%U
  UXsUinv <- solve(UXsU)
  ##sigma.X2 <- t(U.t)%*%Xsigma%*%U.t - t(U.t)%*%Xsigma%*%U%*%solve(t(U)%*%Xsigma%*%U)%*%t(U)%*%Xsigma%*%U.t
  sigma.X2 <- UtXsUt - UtXsU%*%UXsUinv%*%t(UtXsU)
  sigma.X2 <- (sigma.X2+t(sigma.X2))/2
  
  
  X1 <- matrix(nrow=nrank,ncol=n,rnorm(n*nrank))
  ##X1 <- t(mvrnorm(n,rep(0,ncol(U)),diag(ncol(U)) ))
  mean.X2 <- UtXsU%*%UXsUinv%*%X1
  ##mean.X2 <- t(U.t)%*%Xsigma%*%U%*%solve(t(U)%*%Xsigma%*%U)%*%X1
  X2 <- mean.X2 + t(mvrnorm(ncol(mean.X2),rep(0,nrow(mean.X2)),sigma.X2))
  X <- t(solve(t(P))%*%rbind(X1,X2))#/sqrt(n)
  # crossprod(X%*%U)
  
  svdrr <- eigen(rho^abs(outer(1:q, 1:q,FUN="-")))
  svdrrinv <- svdrr$vectors%*%diag(svdrr$values^0.5,nrow=q)%*%t(svdrr$vectors)
  
  UU <- matrix(nrow=n,ncol=q,rnorm(n*q,0,1))%*%svdrrinv
  
  # UU <- matrix(nrow=n,ncol=q,rnorm(n*q,0,1))
  C <- U%*%D%*%t(V)
  Y3 <- X%*%U[,nrank]%*%t(V[,nrank])*D[nrank,nrank]
  sigma <- sqrt(sum(Y3^2)/sum(UU^2))/snr    ## recheck 
  UU <- UU*sigma
  Y <- X%*%C + UU      ## data prepration ends

  return(list(Y=Y,X=X))
}



#' Internal control function for secure
#'
#' list of parameters for controling secure fitting
#' 
#' @param mu penalty parameter used in enforcing orthogonality
#' @param nu penalty parameter used in enforcing orthogonality (incremental rate of mu)
#' @param MMerr tolerence in the majorization maximization(MM) algorithm for computing initial values when missing value occurs
#' @param MMiter maximum number iterations in the MM algorithm
#' @param outTol tolerence of convergence of outer loop in CURE
#' @param outMaxIter maximum number of outer loop iteration in CURE
#' @param inMaxIter maximum number of inner loop iteration in CURE
#' @param inTol tolerence value required for convergence of inner loop in CURE
#' @param lamMaxFac a multiplier of calculated lambda_max
#' @param lamMinFac a multiplier of determing lambda_min as a fraction of lambda_max
#' @param gamma0 power parameter in the adaptive weights
#' @param elnetAlpha elastic net penalty parameter
#' @param spU maximum proportion of nonzero elements in each column of U
#' @param spV maximum proportion of nonzero elements in each column of V
#' 
#' @return a list of controling parameter.
#' @export
secure.control = function(mu=1.0, nu=1.1, 
                         MMerr=1e-3, MMiter=100,
                         outTol=1e-6, outMaxIter=200, 
                         inMaxIter=200, inTol=1e-4, 
                         lamMaxFac=1, lamMinFac=1e-10, 
                         gamma0=2, elnetAlpha=0.95, 
                         spU=0.25, spV=0.25) {
  
  list(mu=mu, nu=nu, MMerr=MMerr, MMiter=MMiter, outTol=outTol, outMaxIter=outMaxIter, 
       inMaxIter=inMaxIter, inTol=inTol, lamMaxFac=lamMaxFac, lamMinFac=lamMinFac, 
       gamma0=gamma0, elnetAlpha=elnetAlpha, spU=spU ,spV=spV)
}





#' Fit reduced rank regression
#'
#' fit multivariate reduced rank regression for a specified rank. 
#' 
#' @param Y a matrix of response (n by q)
#' @param X a matrix of covariate (n by p)
#' @param nrank an integer specifying the desired rank
#' @return 
#'   \item{coef}{reduced rank estimate}
#' @export   
#' @examples
#' #require(secure)
#' Y <- matrix(rnorm(400), 100, 4)
#' X <- matrix(rnorm(800), 100, 8)
#' rrr.fit <- rrr.fit(Y, X, nrank = 3)
rrr.fit <- function(Y,X,nrank=nrank){
  n <- nrow(X);  p <- ncol(X) ;  q <- ncol(Y)  
  naInd <- is.na(Y) 
  Yk <- Y;#Yk[naInd] <- 0   
  f=function(xxx){
    xxx <- as.numeric(as.character(xxx)) #first convert each column into numeric if it is from factor
    xxx[is.na(xxx)] <- mean(xxx, na.rm=TRUE) #convert the item with NA to median value from the column
    xxx #display the column
  }
  Yk <- apply(Yk,2,f) 
  
  
  S_yx <- crossprod(Yk, X)
  S_xx <- crossprod(X)
  S_xx_inv <- tryCatch(ginv(S_xx), error = function(e) solve(S_xx + 
                                                               diag(1e-3,nrow=p,ncol=p)))
  C_ls <- tcrossprod(S_xx_inv, S_yx)
  XC <- X %*% C_ls
  svdXC <- svd(XC, nrank, nrank)
  A <- svdXC$v[, 1:nrank]
  Ad <- (svdXC$d[1:nrank])^2
  AA <- tcrossprod(A)
  Ck <- C_rr <- C_ls %*% AA
  
  if(sum(naInd)>0 ){
    counter <- 0
    repeat{
      counter <- counter+1    
      Yk[naInd] <- crossprod(t(X),Ck)[naInd] 
      
      S_yx <- crossprod(Yk, X)
      S_xx <- crossprod(X)
      S_xx_inv <- tryCatch(ginv(S_xx), error = function(e) solve(S_xx + 
                                                                   diag(1e-3,nrow=p,ncol=p)))
      C_ls <- tcrossprod(S_xx_inv, S_yx)
      XC <- X %*% C_ls
      svdXC <- svd(XC, nrank, nrank)
      A <- svdXC$v[, 1:nrank]
      Ad <- (svdXC$d[1:nrank])^2
      AA <- tcrossprod(A)
      Cc <- C_ls %*% AA
      
      error <- norm(Cc-Ck,'f') / norm(Ck,'f')   
      if ( error < 0.001 | counter>100) {break} else Ck <- Cc        
    }
    C_rr <- Cc
  } 

  # coefSVD <- svd(C_rr, nrank, nrank)
  # D <- diag(coefSVD$d[1:nrank])
  # U <- coefSVD$u[, 1:nrank, drop = FALSE]
  # V <- coefSVD$v[, 1:nrank, drop = FALSE]
  return(list(coef=C_rr))
}







## Initialization (internal)
## 
## initialization for secure, when no missingness in Y
## 
## @param Y response matrix
## @param X covariate matrix
## @param U0 user-supplied initial value of U
## @param V0 user-supplied initial value of V
## @param D0 user-supplied initial value of D
## @param nrank an integer specifying the desired rank
## @param ort if TRUE, X is treated as orthogonal
## @return
##   \item{C_ls}{least square estimate}
##   \item{C_rr}{reduced rank estimate}
##   \item{U0}{initial value of U}
##   \item{D0}{Initial value of D}
##   \item{V0}{Initial value of V}
secure.init <- function(Y, X, U0 = NULL, V0 = NULL, D0 = NULL,nrank = 4,ort) {
  p <- ncol(X);  q <- ncol(Y);  n <- nrow(Y)
  if(ort){
    r.x <- 1/colSums(X^2)
    xy <- crossprod(X,Y)
    C_ls <- xy *r.x
  } else C_ls <- crossprod(ginv(crossprod(X)), crossprod(X,Y))
  
  if (is.null(U0) | is.null(V0) | is.null(D0)){ 
    ## Calculate LS estimator and RR estimator
    XC <- X %*% C_ls/sqrt(n)
    svdxc <- svd(XC)
    V0 <- as.matrix(svdxc$v[,1:nrank]) 
    D0 <- svdxc$d[1:nrank]
    U0 <- C_ls%*%V0%*%diag(1/D0,nrow=nrank,ncol=nrank)
    C_rr <- tcrossprod(U0%*%diag(D0,nrow=nrank,ncol=nrank),V0)
  } else {
    ## D0 <- diag(D0, nrow = nrank)         
    ## C_rr <- U0 %*% D0 %*% t(V0)
    C_rr <- U0 %*% (D0 * t(V0))
    C_ls <- NULL
  }
  
  #   Wuel <- U0^(-gamma0)
  #   Wvel <- V0^(-gamma0)
  #   Wdel <- D0^(-gamma0)
  
  list(C_ls = C_ls, C_rr = C_rr, U0 = U0, V0 = V0, D0 = D0) 
}


## Initialization (internal)
## 
## initialization for secure when Y contains missing value
## 
## 
## @param Y response matrix
## @param X covariate matrix
## @param U0 user-supplied initial value of U.
## @param V0 user-supplied initial value of V. 
## @param D0 user-supplied initial value of D. 
## @param nrank an integer specifying the desired rank
## @param control internal control parameters
## @param method method = 1, least squares; method = 2, reduced rank regression 
## @return 
##   \item{C_ls}{least square estimate}
##   \item{C_rr}{reduced rank estimate}
##   \item{U0}{initial value of U}
##   \item{D0}{Initial value of D}
##   \item{V0}{Initial value of V}
secure.miss.init = function (Y, X, U0 = NULL, V0 = NULL, D0 = NULL, nrank, control = list(), method = 1){  
  ## D0 is assumed not diagonal 
  n <- nrow(X);  p <- ncol(X) ;  q <- ncol(Y)  
  naInd <- is.na(Y) 
  Yk <- Y;#Yk[naInd] <- 0   
  f=function(xxx){
    xxx <- as.numeric(as.character(xxx)) #first convert each column into numeric if it is from factor
    xxx[is.na(xxx)] <- mean(xxx, na.rm=TRUE) #convert the item with NA to median value from the column
    xxx #display the column
  }
  Yk <- apply(Yk,2,f)
  
  # U0 <- V0 <- D0 <- NULL
  U <- V <- D <- NULL
  if(is.null(U0) | is.null(V0) | is.null(D0)){
    Ck <- rrr.fit(Yk,X,nrank=nrank)$coef
    
    counter <- 0
    repeat{
      counter <- counter+1    
      Yk[naInd] <- crossprod(t(X),Ck)[naInd] 
      if(method == 1){    # least square method
        Cc <- tcrossprod(ginv(crossprod(X)),X)%*%Yk
        
      } else {            # RRR method 
        fit.RRR <- rrr.fit(Yk,X,nrank=nrank)
        Cc <- fit.RRR$coef
      }
      error <- norm(Cc-Ck,'f') / norm(Ck,'f')   
      if ( error < control$MMerr | counter>control$MMiter) {break} else Ck <- Cc        
    }
    
    if(method != 1){
      svdxc <- svd(X%*%Cc)
      V0  <- as.matrix(svdxc$v[,1:nrank])
      D0 <- svdxc$d[1:nrank]/sqrt(n)
      U0 <- Cc%*%V0%*%diag(1/D0,nrow=nrank, ncol=nrank)
      
      # coefSVD <- svd(Cc, nrank, nrank)
      # D <- diag(coefSVD$d[1:nrank])
      # U <- coefSVD$u[, 1:nrank, drop = FALSE]
      # V <- coefSVD$v[, 1:nrank, drop = FALSE]
    }
    
  }  else   Cc <- U0 %*% (D0 * t(V0))
  
  
  # print(counter);print(error)
  return(list(C.est = Cc, U0 = U0, V0 = V0, D0 = D0))  
}



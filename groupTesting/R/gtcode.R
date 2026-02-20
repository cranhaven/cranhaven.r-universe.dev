#' Simulating Hierarchical Group Testing Data
#'
#' This function simulates hierarchical group testing data with any number of hierarchical stages.
#'
#' @param N The number of individuals to be tested.
#' @param p A vector of length N consisting of individual disease probabilities.
#' @param S The number of stages used in testing, where \code{S} >= 1.
#' @param psz A vector of pool sizes in stages 1-\code{S}.
#' @param Se A vector of assay sensitivities in stages 1-\code{S}.
#' @param Sp A vector of assay specificities in stages 1-\code{S}.
#' @param assayID A vector of the identification numbers of the assays used in stages 1-\code{S}.
#' @param Yt A vector of individual true disease statuses.
#'
#' @importFrom stats rbinom
#'
#' @details
#' We consider the \eqn{S}-stage hierarchical testing protocol outlined in Kim et al. (2007). Under this protocol, \eqn{N} individual specimens are first assigned to \eqn{m} non-overlapping pools, where each initial pool size is \eqn{c}; i.e., \eqn{N=mc}. The initial pools are tested in stage 1. If a pooled test is negative, all members in the pool are diagnosed as negative. However, if a pooled test is positive, the pool members are split into non-overlapping subpools to be tested in the next stage. This procedure is continued. Note that individual testing is used in the final stage, \eqn{S}, for case identification.
#'
#' \code{S} is a positive integer, \code{S} >= 1. When \code{S}=1, only the non-overlapping initial pools are tested in stage 1.
#'
#' If \code{N} is not divisible by the initial pool size \eqn{c}, we implement the following policy to test the remainder individuals: (1) when \code{S}=1, simply test the remainder pool once as a pooled sample; (2) when \code{S}>1, test the remainder pool based on 2-stage hierarchical testing.
#' 
#' \code{p} is a vector of individual disease probabilities. When all individuals have the same probability of disease, say, 0.10, p can be specified as p=rep(0.10, N) or p=0.10.
#'
#' \code{psz} is a vector of length \code{S}, where the first element is the stage-1 pool size, the second element is the stage-2 pool size, and so on. Pool size at any stage must be divisible by the pool size used at the next stage. For example, \code{psz} can be specified as \code{c(12,3,1)} but not as \code{c(12,5,1)}.
#'
#' When \code{psz} is a vector of length 1, test responses are simulated only from the initial pools.
#'
#' \code{Se} is a vector of length \code{S}, where the first element is the sensitivity of the assay used in stage 1, the second element is sensitivity of the assay in stage 2, and so on.
#'
#' \code{Sp} is a vector of length \code{S}, where the first element is the specificity of the assay used in stage 1, the second element is specificity of the assay in stage 2, and so on.
#'
#' \code{assayID} is a vector of length \code{S}, where the first element is the ID of the assay in stage 1, the second element is the ID of the assay in stage 2, and so on.
#'
#' When available, the individual true disease statuses (1 for positive and 0 for negative) can be used in simulating the group testing data through argument \code{Yt}. When an input is entered for \code{Yt}, argument \code{p} will be ignored.
#'
#' @return A list with components:
#' \item{gtData}{The simulated group testing data.}
#' \item{testsExp}{The number of tests expended.}
#'
#' @export
#'
#' @references
#' Kim HY, Hudgens M, Dreyfuss J, Westreich D, and Pilcher C (2007). Comparison of Group Testing Algorithms for Case Identification in the Presence of Testing Error. \emph{Biometrics}, 63(4), 1152–1163.
#'
#' @seealso
#' \code{\link{array.gt.simulation}} for simulation of the array-based group testing data.
#'
#' @examples
#'
#' library(groupTesting)
#'
#' ## Example 1: Two-stage hierarchical (Dorfman) testing
#' N <- 50              # Sample size
#' psz <- c(5, 1)       # Pool sizes used in stages 1 and 2
#' S <- 2               # The number of stages
#' Se <- c(0.95, 0.95)  # Sensitivities in stages 1-2
#' Sp <- c(0.98, 0.98)  # Specificities in stages 1-2
#' assayID <- c(1, 1)   # The same assay in both stages
#'
#' # (a) Homogeneous population
#' pHom <- 0.10         # Overall prevalence
#' hier.gt.simulation(N=N,p=pHom,S=S,psz=psz,Se=Se,Sp=Sp,assayID=assayID)
#'
#' # Alternatively, the individual true statuses can be used as: 
#' yt <- rbinom( N, size=1, prob=0.1 )
#' hier.gt.simulation(N=N,S=S,psz=psz,Se=Se,Sp=Sp,assayID=assayID,Yt=yt)
#'
#' # (b) Heterogeneous population (regression)
#' param <- c(-3,2,1)
#' x1 <- rnorm(N, mean=0, sd=.75)
#' x2 <- rbinom(N, size=1, prob=0.5)
#' X <- cbind(1, x1, x2)
#' pReg <- exp(X%*%param)/(1+exp(X%*%param)) # Logit
#' hier.gt.simulation(N=N,p=pReg,S=S,psz=psz,Se=Se,Sp=Sp,assayID=assayID)
#'
#' ## Example 2: Initial (1-stage) pooled testing data
#' N <- 50
#' S <- 1
#' Se <- 0.95
#' Sp <- 0.98
#' assayID <- 1
#'
#' # (a) Homogeneous population 
#' pHom <- 0.10   # Overall prevalence
#' 
#' # a(i) Pooled testing
#' psz <- 5       # pool size    
#' hier.gt.simulation(N,pHom,S,psz,Se,Sp,assayID)
#'
#' # a(ii) Inidividual testing
#' psz <- 1       # pool size    
#' hier.gt.simulation(N,pHom,S,psz,Se,Sp,assayID)
#'
#' # (b) Heterogeneous population (regression)
#' param <- c(-3,2,1)
#' x1 <- rnorm(N, mean=0, sd=.75)
#' x2 <- rbinom(N, size=1, prob=0.5)
#' X <- cbind(1, x1, x2)
#' pReg <- exp(X%*%param)/(1+exp(X%*%param))  # Logit
#' 
#' # b(i) Pooled testing
#' psz <- 5
#' hier.gt.simulation(N,pReg,S,psz,Se,Sp,assayID)
#'
#' # b(ii) Individual testing
#' psz <- 1
#' hier.gt.simulation(N,pReg,S,psz,Se,Sp,assayID)
#'
#' ## Example 3: Data with other configurations
#' N <- 48
#' p <- 0.10
#' Se <- c(.90, .95, .92, .90, .99)
#' Sp <- c(.96, .96, .90, .92, .95)
#' Assay <- 1:5
#'
#' # Initial pooled testing, using the first element of Se, Sp & Assay
#' pszH1 <- 4
#' hier.gt.simulation(N=N,p=p,S=1,psz=pszH1,Se=Se,Sp=Sp,assayID=Assay)
#'
#' pszH2 <- c(4,1)       # Two-stage, using first 2 elements of Se, Sp & Assay
#' hier.gt.simulation(N=N,p=p,S=2,psz=pszH2,Se=Se,Sp=Sp,assayID=Assay)
#'
#' pszH4 <- c(16,8,2,1)  # Four-stage, using first 4 elements of Se, Sp & Assay
#' hier.gt.simulation(N=N,p=p,S=4,psz=pszH4,Se=Se,Sp=Sp,assayID=Assay)
#'
#' pszH3 <- c(12,2,1)    # Three-stage, using first 3 elements of Se, Sp & Assay
#' Assay3 <- c(2,1,3)    # Array ID numbers do not need to be in order
#' hier.gt.simulation(N=N,p=p,S=3,psz=pszH3,Se=Se,Sp=Sp,assayID=Assay3)
#'
#' # Works with a remainder pool of 2 individuals
#' N <- 50
#' psz <- c(12,2,1)
#' hier.gt.simulation(N=N,p=p,S=3,psz=psz,Se=Se,Sp=Sp,assayID=Assay)
#'
hier.gt.simulation <- function(N,p=0.10,S,psz,Se,Sp,assayID,Yt=NULL){
  c.s <- psz        ## Pool sizes
  S <- length(c.s)  ## Number of stages
  if(min(c.s)<=0) stop("Pool size cannot be negative or zero")
  if(S > 1){
    quot <- rep(-9,S-1)
    for(s in 1:(S-1)){quot[s] <- c.s[s]%%c.s[s+1]}
    if(max(quot) > 0) stop("Pool size at any stage must be divisible by the next-stage pool size")
  }
  ## Setting up pooling configurations
  M <- floor(N/c.s[1])
  N0 <- M*c.s[1]
  if( !is.null(Yt) ){
    Ytil1 <- Yt 
  }else{
    Ytil1 <- stats::rbinom(N,1,p)
  }
  Rem <- N-N0
  Psz <- n.div <- list()
  Psz[[1]] <- rep(c.s[1],M)
  n.div[[1]] <- rep(1,length(Psz[[1]]))
  n.sm <- matrix(-9,length(Psz[[1]]),S)
  n.sm[ ,1] <- 1
  if(S > 1){
    for( s in 1:(S-1) ){
      store <- tmp <- NULL
      for(i in 1:length(Psz[[s]])){
        temp <- rep( c.s[s+1],floor(Psz[[s]][i]/c.s[s+1]) )
        store <- c(store,temp)
        tmp <- c(tmp,length(temp))
      }
      Psz[[s+1]] <- store
      n.div[[s+1]] <- tmp
    }
    vec <- rep(1,length(Psz[[1]]))
    for(s in 1:(S-1) ){
      id0 <- cumsum(c(0,vec))
      for(i in 1:length(Psz[[1]])){
        vec[i] <- sum(n.div[[s+1]][(id0[i]+1):id0[i+1]])
      }
      n.sm[ ,s+1] <- vec
    }
  }
  Zmat <- NULL
  cc <- cumsum(c(0,colSums(n.sm)))
  id <- cumsum(c(0,Psz[[1]]))
  pl.res <- rep(-9,cc[S+1])
  ## Simulating pooled responses at stage 1
  for(m in 1:M){
    mid <- (id[m]+1):id[m+1]
    prob1 <- ifelse(sum(Ytil1[mid])>0,Se[1],1-Sp[1])
    z1 <- stats::rbinom(1,1,prob1)
    pl.res[m] <- z1
    Zmat <- rbind(Zmat,c(z1,length(mid),Se[1],Sp[1],assayID[1],mid))
  }
  if(S == 1){
    if(Rem > 0){
      rid1 <- (N0+1):N
      zr1 <- rbinom(1,1,ifelse(sum(Ytil1[rid1])>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,c(zr1,Rem,Se[1],Sp[1],assayID[1],rid1,rep(-9,c.s[1]-Rem)))
      warning("N is not divisible by the initial pool size; a smaller remainder pool is used")
    }
  }
  ## Simulating pooled responses from subsequent stages 
  if( S > 1){
    for(s in 2:S){
      Z1 <- pl.res[(cc[s-1]+1):cc[s]]
      cid <- cumsum(c(0,Psz[[s]]))
      cn <- cumsum(c(0,n.div[[s]]))
      tmp1 <- NULL
      for(d in 1:length(Psz[[s-1]])){
        tmp3 <- NULL
        if(Z1[d]==0){
          tmp3 <- rep(0,length((cn[d]+1):cn[d+1]))
        }
        if(Psz[[s-1]][d]==1){
          tmp3 <- Z1[d]
        }
        if(Psz[[s-1]][d]>1){
          if(Z1[d] == 1){
            for(i in (cn[d]+1):cn[d+1]){
              crng <- (cid[i]+1):cid[i+1]
              probs <- ifelse(sum(Ytil1[crng])>0,Se[s],1-Sp[s])
              ztp1 <- stats::rbinom(1,1,probs)
              tmp3 <- c(tmp3,ztp1)
              fill1 <- rep(-9,Psz[[1]][1]-length(crng))
              Zmat <- rbind(Zmat,c(ztp1,length(crng),Se[s],Sp[s],assayID[s],crng,fill1))
            }
          }
        }
        tmp1 <- c(tmp1,tmp3)
      }
      pl.res[(cc[s]+1):cc[s+1]] <- tmp1
    }
    if(Rem == 1){
      yr1 <- rbinom(1,1,ifelse(Ytil1[N]==1,Se[S],1-Sp[S]))
      Zmat <- rbind(Zmat,c(yr1,1,Se[S],Sp[S],assayID[S],N,rep(-9,c.s[1]-1)))
      if( Rem > 0) warning("N is not divisible by the initial pool size; a smaller remainder pool is used")
    }
    if(Rem > 1){
      rid <- (M*c.s[1]+1):N
      ytr1 <- Ytil1[rid]
      zr2 <- rbinom(1,1,ifelse(sum(ytr1)>0,Se[S-1],1-Sp[S-1]))
      Zmat <- rbind(Zmat,c(zr2,Rem,Se[S-1],Sp[S-1],assayID[S-1],rid,rep(-9,c.s[1]-Rem)))
      if(zr2 > 0){
        yrm1 <- rbinom(Rem,1,ifelse(ytr1==1,Se[S],1-Sp[S]))
	    Zmat <- rbind(Zmat,cbind(yrm1,1,Se[S],Sp[S],assayID[S],rid,matrix(-9,Rem,c.s[1]-1)))
      }
      if( Rem > 0) warning("N is not divisible by the initial pool size; a smaller remainder pool is used")
    }
  }
  ## Output
  ivid <- paste( rep("Mem",Psz[[1]][1]),1:Psz[[1]][1],sep="" )
  colnames(Zmat) <- c("Z","psz","Se","Sp","Assay",ivid)
  rownames(Zmat) <- paste("Pool:",1:nrow(Zmat),sep="")
  list("gtData"  = Zmat, 
       "testsExp"= nrow(Zmat))
}

#' Simulating Array-Based Group Testing Data
#'
#' This function simulates two-dimensional array-based group testing data.
#'
#' @param N The number of individuals to be tested.
#' @param p A vector of length N consisting of individual disease probabilities.
#' @param protocol Either "A2" or "A2M", where "A2" ("A2M") refers to the two-dimensional array without (with) testing the members of an array as a single pooled sample.
#' @param n The row (or column) size of the arrays.
#' @param Se A vector of assay sensitivities.
#' @param Sp A vector of assay specificities.
#' @param assayID A vector of assay identification numbers.
#' @param Yt A vector of individual true disease statuses.
#'
#' @importFrom stats rbinom
#'
#' @details
#' We consider the array testing protocol outlined in Kim et al. (2007). Under this protocol, \eqn{N} individuals are assigned to \eqn{m} non-overlapping \eqn{n}-by-\eqn{n} matrices such that \eqn{N=mn^2}. From each matrix, \eqn{n} pools are formed using the row specimens and another \eqn{n} pools are formed using the column specimens. In stage 1, the \eqn{2n} pools are tested. In stage 2, individual testing is used for case identification according to the strategy described in Kim et al. (2007). This is a 2-stage protocol called \emph{Square Array without Master Pool Testing} and denoted by \eqn{A2(n:1)} in Kim et al. (2007). A variant (3-stage protocol) is also presented in Kim et al. (2007) which employs testing the \eqn{n^2} array members together as an initial pooled unit before implementing the 2-stage array. If the initial pooled test is negative, the procedure stops (i.e., the 2-stage array is not needed). However, if the pooled test is positive, the 2-stage protocol is used as before. This 3-stage approach is called \emph{Square Array with Master Pool Testing} and is denoted by \eqn{A2(n^2:n:1)}. See Kim et al. (2007) for more details.
#'
#' \code{N} should be divisible by the array size \eqn{n^2}. When not divisible, the remainder individuals are tested one by one (i.e., individual testing).
#'
#' \code{p} is a vector of individual disease probabilities. When all individuals have the same probability of disease, say, 0.10, \code{p} can be specified as rep(0.10, N) or p=0.10.
#'
#' For "A2" and "A2M", the pool sizes used are \code{c(n, 1)} and \code{c(n^2, n, 1)}, respectively.
#'
#' For "A2", \code{Se} is \code{c(Se1, Se2)}, where \code{Se1} is the sensitivity of the assay used for both row and column pools, and \code{Se2} is the sensitivity of the assay used for individual testing. For "A2M", \code{Se} is \code{c(Se1, Se2, Se3)}, where \code{Se1} is for the initial array pool, \code{Se2} is for the row and column pools, and \code{Se3} is for individual testing. \code{Sp} is specified in the same manner.
#'
#' For "A2", \code{assayID} is \code{c(1, 1)} when the same assay is used for row/column pool testing as well as for individual testing, and assayID is \code{c(1, 2)} when assay 1 is used for row/column pool testing and assay 2 is used for individual testing. In the same manner, \code{assayID} is specified for "A2M" as \code{c(1, 1, 1)}, \code{c(1, 2, 3)}, and in many other ways.
#'
#' When available, the individual true disease statuses (1 for positive and 0 for negative) can be used in simulating the group testing data through argument \code{Yt}. When an input is entered for \code{Yt}, argument \code{p} will be ignored.
#'
#' @return A list with components:
#' \item{gtData}{The simulated group testing data.}
#' \item{testsExp}{The number of tests expended in the simulation.}
#'
#' @export
#'
#' @references
#' Kim HY, Hudgens M, Dreyfuss J, Westreich D, and Pilcher C (2007). Comparison of Group Testing Algorithms for Case Identification in the Presence of Testing Error. \emph{Biometrics}, 63(4), 1152–1163.
#'
#' @seealso
#' \code{\link{hier.gt.simulation}} for simulation of the hierarchical group testing data.
#'
#' @examples
#'
#' library(groupTesting)
#'
#' ## Example 1: Square Array without Master Pool Testing (i.e., 2-Stage Array)
#' N <- 48              # Sample size
#' protocol <- "A2"     # 2-stage array
#' n <- 4               # Row/column size
#' Se <- c(0.95, 0.95)  # Sensitivities in stages 1-2
#' Sp <- c(0.98, 0.98)  # Specificities in stages 1-2
#' assayID <- c(1, 1)   # The same assay in both stages
#'
#' # (a) Homogeneous population
#' pHom <- 0.10         # Overall prevalence
#' array.gt.simulation(N=N,p=pHom,protocol=protocol,n=n,Se=Se,Sp=Sp,assayID=assayID)
#'
#' # Alternatively, the individual true statuses can be used as: 
#' yt <- rbinom( N, size=1, prob=0.1 )
#' array.gt.simulation(N=N,protocol=protocol,n=n,Se=Se,Sp=Sp,assayID=assayID,Yt=yt)
#'
#' # (b) Heterogeneous population (regression)
#' param <- c(-3,2,1)
#' x1 <- rnorm(N, mean=0, sd=.75)
#' x2 <- rbinom(N, size=1, prob=0.5)
#' X <- cbind(1, x1, x2)
#' pReg <- exp(X%*%param)/(1+exp(X%*%param)) # Logit
#' array.gt.simulation(N=N,p=pReg,protocol=protocol,n=n,Se=Se,Sp=Sp,assayID=assayID)
#'
#' # The above examples with different assays
#' Se <- c(0.95, 0.98)
#' Sp <- c(0.97, 0.99)
#' assayID <- c(1, 2)
#' array.gt.simulation(N,pHom,protocol,n,Se,Sp,assayID)
#' array.gt.simulation(N,pReg,protocol,n,Se,Sp,assayID)
#'
#' ## Example 2: Square Array with Master Pool Testing (i.e., 3-Stage Array)
#' N <- 48
#' protocol <- "A2M"
#' n <- 4
#' Se <- c(0.95, 0.95, 0.95)
#' Sp <- c(0.98, 0.98, 0.98)
#' assayID <- c(1, 1, 1)    # The same assay in 3 stages
#'
#' # (a) Homogeneous population
#' pHom <- 0.10
#' array.gt.simulation(N,pHom,protocol,n,Se,Sp,assayID)
#'
#' # (b) Heterogeneous population (regression)
#' param <- c(-3,2,1)
#' x1 <- rnorm(N, mean=0, sd=.75)
#' x2 <- rbinom(N, size=1, prob=0.5)
#' X <- cbind(1, x1, x2)
#' pReg <- exp(X%*%param)/(1+exp(X%*%param)) # Logit
#' array.gt.simulation(N,pReg,protocol,n,Se,Sp,assayID)
#'
#' # The above examples with different assays:
#' Se <- c(0.95, 0.98, 0.98)
#' Sp <- c(0.97, 0.98, 0.92)
#' assayID <- 1:3
#' array.gt.simulation(N,pHom,protocol,n,Se,Sp,assayID)
#' array.gt.simulation(N,pReg,protocol,n,Se,Sp,assayID)
#'
array.gt.simulation <- function(N,p=0.10,protocol=c("A2","A2M"),n,Se,Sp,assayID,Yt=NULL){
  ind.simulation <- function(Nr,prob=0.1,Se.ind,Sp.ind,yt=NULL){
    Yt.ind <- yt
    if( is.null(yt) ){
      Yt.ind <- stats::rbinom(Nr,1,prob)
    }
    for(k in 1:Nr){
      prb <- ifelse(Yt.ind>0, Se.ind, 1-Sp.ind)
      y.test <- stats::rbinom(Nr,1,prb)
    }
    return(y.test)
  }
  protocol <- match.arg(protocol)
  if(n <= 1) stop("Row size and column size must be larger than 1")
  if(n^2 > N) stop("The array size n*n is too large")
  L <- floor(N/n^2)
  N0 <- L*n^2
  if( !is.null(Yt) ){
    Ytil1 <- Yt 
  }else{
    Ytil1 <- stats::rbinom(N,1,p)
  }
  id1 <- cumsum( c(0,rep(n^2,L)) )
  Zmat <- Ymat <- NULL
  ## Simulating for two-stage array 
  if(protocol=="A2"){
    for(l in 1:L){
      Z_id <- matrix((id1[l]+1):id1[l+1],n,n)
      Ymat1 <- matrix(Ytil1[(id1[l]+1):id1[l+1]],n,n)
      R1 <- stats::rbinom(n,1,ifelse(rowSums(Ymat1)>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,cbind(R1,n,Se[1],Sp[1],assayID[1],Z_id))
      C1 <- stats::rbinom(n,1,ifelse(colSums(Ymat1)>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,cbind(C1,n,Se[1],Sp[1],assayID[1],t(Z_id)))
      for(i in 1:n){
        for(j in 1:n){
          T1 <- 0
          if(R1[i]==1 & C1[j]==1) T1 <- T1 + 1
          if(R1[i]==1 & sum(C1)==0) T1 <- T1 + 1
          if(sum(R1)==0 & C1[j]==1) T1 <- T1 + 1
          if(T1>=1){
            y1 <- stats::rbinom(1,1,ifelse(Ymat1[i,j]==1,Se[2],1-Sp[2]))
            Ymat <- rbind(Ymat,c(y1,1,Se[2],Sp[2],assayID[2],Z_id[i,j]))
          }
        }
      }
    }
    if(!is.null(Ymat)){
      Zmat <- rbind(Zmat,cbind(Ymat,matrix(-9,nrow(Ymat),n-1)))
    }
    idv.id <- paste( rep("Mem",n), 1:n, sep="" )
  }
  ## Simulating for three-stage array
  if(protocol=="A2M"){
    zfil <- matrix(-9,n,n^2-n)
    for(l in 1:L){
      aryid <- (id1[l]+1):id1[l+1]
      ytmp <- Ytil1[aryid]
      zary <- stats::rbinom(1,1,ifelse(sum(ytmp)>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,c(zary,n*n,Se[1],Sp[1],assayID[1],aryid))
      if(zary==1){
        Z_id <- matrix((id1[l]+1):id1[l+1],n,n)
        Ymat1 <- matrix(Ytil1[(id1[l]+1):id1[l+1]],n,n)
        R1 <- stats::rbinom(n,1,ifelse(rowSums(Ymat1)>0,Se[2],1-Sp[2]))
        Zmat <- rbind(Zmat,cbind(R1,n,Se[2],Sp[2],assayID[2],Z_id,zfil))
        C1 <- stats::rbinom(n,1,ifelse(colSums(Ymat1)>0,Se[2],1-Sp[2]))
        Zmat <- rbind(Zmat,cbind(C1,n,Se[2],Sp[2],assayID[2],t(Z_id),zfil))
        for(i in 1:n){
          for(j in 1:n){
            T1 <- 0
            if(R1[i]==1 & C1[j]==1) T1 <- T1 + 1
            if(R1[i]==1 & sum(C1)==0) T1 <- T1 + 1
            if(sum(R1)==0 & C1[j]==1) T1 <- T1 + 1
            if(T1>=1){
              y1 <- stats::rbinom(1,1,ifelse(Ymat1[i,j]==1,Se[3],1-Sp[3]))
              Ymat <- rbind(Ymat,c(y1,1,Se[3],Sp[3],assayID[3],Z_id[i,j]))
            }
          }
        }
      }
    }
    if(!is.null(Ymat)){
      Zmat <- rbind(Zmat,cbind(Ymat,matrix(-9,nrow(Ymat),n*n-1)))
    }
    idv.id <- paste( rep("Mem",n), 1:n^2, sep="" )
  }
  ## Individual testing for the remainder sample
  Rem <- N-N0
  if( Rem > 0) warning("N is not divisible by the array size; the remainder individuals are tested one by one")
  if( Rem > 0 ){
    S <- length(Se)
    ytest <- ind.simulation(Nr=Rem,Se.ind=Se[S],Sp.ind=Sp[S],yt=Ytil1[(N0+1):N])
	rmat <- matrix(-9,length(ytest),ncol(Zmat)-6)
    zfil <- cbind( ytest,1,Se[S],Sp[S],assayID[S],(N0+1):N,rmat )
    Zmat <- rbind( Zmat, zfil )
  }
  ## Output
  rownames(Zmat) <- NULL
  colnames(Zmat) <- c("Z","psz","Se","Sp","Assay",idv.id)
  rownames(Zmat) <- paste("Pool:",1:nrow(Zmat),sep="")
  return( list("gtData"   = Zmat,
               "testsExp" = nrow(Zmat)) )
}

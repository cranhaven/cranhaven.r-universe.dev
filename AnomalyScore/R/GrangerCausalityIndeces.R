
##### Conditional Granger Causality index  ######
#      Copyright (C) 2015 Dimitris Kugiumtzis
#  
#      This program is free software: you can redistribute it and/or modify
#      it under the terms of the GNU General Public License as published by
#      the Free Software Foundation, either version 3 of the License, or
#      (at your option) any later version.
#  
#      This program is distributed in the hope that it will be useful,
#      but WITHOUT ANY WARRANTY; without even the implied warranty of
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#      GNU General Public License for more details.
#  
#      You should have received a copy of the GNU General Public License
#      along with this program.  If not, see <http://www.gnu.org/licenses/>.
#  
# =========================================================================
#  Reference : D. Kugiumtzis, "Direct coupling information measure from 
#              non-uniform embedding", Physical Review E, Vol 87, 062918, 
#              2013
#              I. Vlachos, D. Kugiumtzis, "Non-uniform state space 
#              reconstruction and coupling detection", Physical Review E, 
#              Vol 82, 016207, 2010

#  Link      : http://users.auth.gr/dkugiu/
# ========================================================================= 
#
# R Version adapted by Guillermo Cuauhtemoctzin Granados Garcia
# Department of mathematics and statistics Lancaster University


#' multilagmatrix
#' 
#' multilagmatrix builds the set of explanatory variables for the dynamic
#' regression model.

#' @param xM  the matrix of K time series (variables in columns)
#' @param responseindex the index of the response variable in \eqn{\{1,\ldots,K\}}
#' @param ordersV vector of size 1xK of the maximum order for each of the K
#'                 variables.
#' @param indexV  the vector of size 1 x K*pmax of zeros and ones
#'                 e.g. if the component in position 2*pmax+3 is one, the
#'                 third variable, lag 3, X3(t-3), is selected.
#' @return the matrix of all explanatory lagged variables in the
#'                 DR model. The sequence of the lagged variables in 'lagM'
#                 is determined by indexV.
#' @seealso Kugiumtzis, D. "Direct-Coupling Information Measure from Nonuniform
#' Embedding." Physical Review E 87, no. 6 (June 25, 2013): 062918. 
#' \doi{10.1103/PhysRevE.87.062918}
#'  
#' @export

multilagmatrix <- function(xM, responseindex, ordersV, indexV) {
  n <- nrow(xM)
  K <- ncol(xM)
  pmax <- length(indexV) / K
  xtempM <- matrix(NA, n, K * pmax)
  for (iK in 1:K) {
    lag_indices <- ((iK - 1) * pmax + 1) : (ordersV[iK] + (iK - 1) * pmax)
    for (lag in 1:ordersV[iK]) {
      xtempM[ (lag + 1):n, lag_indices[lag] ] <- xM[1:(n - lag), iK]
    }
  }
  
  xlagM <- cbind(xM[(max(ordersV) + 1):n, responseindex, drop = FALSE], xtempM[(max(ordersV) + 1):n, indexV == 1])
  return(xlagM)
}

# estimates the var parameters and returns the MSE error
DRfitmse <- function(xM, responseindex, ordersV, indexV){
  n <- nrow(xM)
  xlagM=multilagmatrix(xM, responseindex, ordersV, indexV)
  
  if(dim(xlagM)[2]!=1 ){
    An <- MASS::ginv(t(xlagM[, -1]) %*% xlagM[, -1]) %*% t(xlagM[, -1]) %*% xlagM[, 1]
    preV <- xlagM[, -1] %*% An
    resV <- xM[(max(ordersV) + 1):n, responseindex] - preV
    MSE <- sum(resV^2) / length(resV)
  }else{
    MSE=var(xlagM)
    An=rep(0, ncol(xlagM)-1 )
  }
  return(list( MSE=MSE, coeff=An )   )
}



#' modified Back-in-time Selection for vector AR parameters estimation
#' 
#' @param xM  the matrix of K time series (variables in columns)
#' @param responseindex the index of the response variable in \eqn{\{1,\ldots,K\}}
#' @param pmax maximum order(lag) of the VAR model to be considered
#' @return the matrix of all explanatory lagged variables in the
#'                 DR model. The sequence of the lagged variables in 'lagM'
#                 is determined by indexV.
#' @seealso I. Vlachos and D. Kugiumtzis, "Backward-in-time selection of the
#'  order of dynamic regression prediction model," J. Forecast., 
#'  vol. 32, pp. 685-701, 2013.
#'  
#' @export

mBTS <- function(xM, responseindex, pmax) {
  n <- nrow(xM)
  K <- ncol(xM)
  
  for (d in 1:K) {
    xM[, d] <- xM[, d] - mean(xM[, d])
  }
  
  indexV <- rep(0, K * pmax)
  ordersinV <- rep(0, K)
  
  modelvar=DRfitmse(xM, responseindex, ordersinV, indexV)
  MSEval  <- modelvar$MSE
  coeff=modelvar$coeff
  BICold <- (n - max(ordersinV)) * log(MSEval) + sum(ordersinV) * log(n - max(ordersinV))
  
  ingameV <- 1:K
  ningame <- K
  terminateflag <- FALSE
  incrisor <- 1
  
  while (!terminateflag && ningame != 0) {
    pmaxreach <- which(ordersinV >= pmax)
    ingameV <- setdiff(ingameV, pmaxreach)
    ningame <- length(ingameV)
    
    if (ningame != 0) {
      BICnowV <- rep(NA, ningame)
      coeffnow=list()
      MSEnowV <- rep(NA, ningame)
      
      for (iK in 1:ningame) {
        
        ordtempV <- ordersinV
        ordtempV[ingameV[iK]] <- ordtempV[ingameV[iK]] + incrisor
        overpmaxV <- which(ordtempV > pmax)
        
        if (length(overpmaxV) > 0) {
          ordtempV[overpmaxV] <- pmax
        }
        
        if (length(overpmaxV) == K) {
          terminateflag <- TRUE
        }
        
        tempindexV <- indexV
        tempindexV[(ingameV[iK] - 1) * pmax + ordtempV[ingameV[iK]]] <- 1
        modelnow=DRfitmse(xM, responseindex, ordtempV, tempindexV)
        
        MSEnowV[iK] <- modelnow$MSE
        coeffnow[[iK]]=modelnow$coeff
        BICnowV[iK] <- (n - max(ordtempV)) * log(MSEnowV[iK]) + sum(tempindexV) * log(n - max(ordtempV))
      }
      
      BICnew <- min(BICnowV, na.rm = T)
      iBICnew <- which(BICnowV == BICnew)
      invarindex <- ingameV[ BICnowV == BICnew  ]
      
      if (BICold <= BICnew) {
        incrisor <- incrisor + 1
        
        if (incrisor > pmax - min(ordersinV)) {
          terminateflag <- TRUE
        }
      } else {
        indexV[(invarindex - 1) * pmax + ordersinV[invarindex] + incrisor] <- 1
        ordersinV[invarindex] <- ordersinV[invarindex] + incrisor
        BICold <- BICnew
        incrisor <- 1
        MSEval <- MSEnowV[iBICnew]
        coeff=coeffnow[[iBICnew]]
      }
    } else {
      terminateflag <- TRUE
    }
  }
  maxorder <- max(ordersinV)
  return(list(indexV = indexV, maxorder = maxorder, MSEval = MSEval, coeff=coeff ))
}


#' computation of the conditional Granger causality index
#' 
#' @param xM  the matrix of K time series (variables in columns)
#' @param responseindex the index of the response variable in \eqn{\{1,\ldots,K\}}
#' @param pmax maximum order(lag) of the VAR model to be considered
#' @return the matrix of all the conditional Granger causality index across 
#' the series of a multivariate set.
#' @seealso Siggiridou, Elsa, and Dimitris Kugiumtzis. "Granger Causality
#'  in Multivariate Time Series Using a Time-Ordered Restricted Vector 
#'  Autoregressive Model." IEEE Transactions on Signal Processing 64, 
#'  no. 7 (April 2016): 1759-73. \doi{10.1109/TSP.2015.2500893}
#'  
#' @export

mBTSCGCI <- function(xM, responseindex, pmax) {
  n <- nrow(xM)
  K <- ncol(xM)
  RCGCIV <- rep(0, K)
  pRCGCIV <- rep(NA, K)
  list_mBTSCGCI <- mBTS(xM, responseindex, pmax)
  indexV <- list_mBTSCGCI$indexV
  maxorder <- list_mBTSCGCI$maxorder
  
  if (maxorder > 0) {
    RCGCIV[responseindex] <- 0
    xM <- sweep(xM, 2, colMeans(xM))
    unrestrictedindexV=c()
    for(k in 1:K){
      unrestrictedindexV=c(unrestrictedindexV, indexV[((k-1)*pmax+1 ):((k-1)*pmax+maxorder)])
    }
    
    MSEunrestricted <- DRfitmse(xM, responseindex, rep(maxorder, K), unrestrictedindexV)$MSE
    allactive <- sum(unrestrictedindexV)
    drivingV <- setdiff(seq_len(K), responseindex)
    
    for (iK in drivingV) {
      
      restrictedindexV <- unrestrictedindexV
      restrictedindexV[((iK - 1) * maxorder + 1):(iK * maxorder)] <- rep(0, maxorder) # exclude X_{iK}
      MSErestricted <- DRfitmse(xM, responseindex, rep(maxorder, K), restrictedindexV)$MSE
      RCGCIV[iK] <- MSEunrestricted/MSErestricted 
    }
  }
  return(list(RCGCIV = RCGCIV))
}






#' mBTS Vector Autoregressive coefficients fourier transform
#' 
#' DFT Vector Autoregressive coefficients matrix using the mBTS algorithm.
#' This matrix is the base to compute the generalized partial directed coherence 
#' 
#' @param xM  the matrix of K time series (variables in columns)
#' @param responseindex the index of the response variable in \eqn{\{1,\ldots,K\}}
#' @param pmax maximum order(lag) of the VAR model to be considered
#' @param freqs frequencies at which the spectral density is estimated
#' @return the matrix of all the Restricted Generalized Partial Directed 
#' Coherence index across the series of a multivariate set.
#'  
#' @export

mBTS_Af_mat<- function(xM, responseindex, pmax, freqs) {
  n <- nrow(xM)
  K <- ncol(xM)
  Af <- matrix(0, ncol=K, nrow = length(freqs) )
  
  list_mBTSCGCI <- mBTS(xM, responseindex, pmax)
  indexV <- list_mBTSCGCI$indexV
  maxorder <- list_mBTSCGCI$maxorder
  varcoeff=     list_mBTSCGCI$coeff
  
  if (maxorder > 0) {
    DFtmat=matrix(0, ncol=length(freqs), nrow = maxorder)
    for(i in 1:maxorder ){
      DFtmat[i, ]=complex( real = cos(2*pi*freqs), imaginary = sin(2*pi*freqs) )^i
    }
    
    xM <- sweep(xM, 2, colMeans(xM))
    unrestrictedindexV=c()
    for(k in 1:K){
      unrestrictedindexV=c(unrestrictedindexV, indexV[((k-1)*pmax+1 ):((k-1)*pmax+maxorder)])
    }
    varcoeffall=rep(0, length(unrestrictedindexV))
    varcoeffall[     which(unrestrictedindexV==1)]= varcoeff
    
    for (iK in 1:K) {
      indexmbts=((iK - 1) * maxorder + 1):(iK * maxorder)
      
      if( sum(unrestrictedindexV[indexmbts])==0 ){
        Af[,iK ] <- 0
      }else{
        
        coeffnow=varcoeffall[indexmbts]
        orders= which( coeffnow!=0 )
        coeffforpdc=coeffnow[orders]
        Af[,iK]=  as.numeric(iK==responseindex)  - t(coeffforpdc)%*%DFtmat[orders,]
      }
    }
    
  }
  
  return(Af)
}

#' Restricted Generalized Partial Directed Coherence
#' 
#' partial directed coherence matrix values based on the mBTS algorithm
#' for estimation of the VAR parameters
#' 
#' @param xM  the matrix of K time series (variables in columns)
#' @param pmax maximum order(lag) of the VAR model to be considered
#' @param freqs frequencies at which the spectral density is estimated
#' @return the matrix of all the Restricted Generalized Partial Directed 
#' Coherence index across the series of a multivariate set.
#' #'  
#' @export

mBTSRGPDC<- function(xM, pmax, freqs){
  n <- nrow(xM)
  K <- ncol(xM)
  sigmaii=as.vector( apply(xM, 2, var))
  
  Afall= array(0, dim=c(K,K, length(freqs) ) )
  for(k in 1:K){
    Aftemp <-   t( t( mBTS_Af_mat(xM, responseindex=k, pmax, freqs)  )/sqrt(sigmaii) ) 
    denompdc=apply( Aftemp,1, function(x)  sqrt(sum(abs(x)^2 ))  )
    if(sum(denompdc)>0 ){ 
      for(jj in 1:K){
        Aftemp[,jj]=abs( Aftemp[,jj])/denompdc
      }
    }
    Afall[k,,]= t(abs(Aftemp))
  }
  return(Afall)
}


##### PMIME Partial mutual information from mixed embedding ######

#' PMIME Partial mutual information from mixed embedding
#' 
#'  computes the measure \eqn{R_{X->Y|Z}} for all combinations of \eqn{X} and \eqn{Y} time
#'  series from the multivariate time series given in matrix 'allM', of size
#'  \eqn{N x K}, where \eqn{Z} contains the rest \eqn{K-2} time series. 
#'  The components of X,Y, and Z, are found from a mixed embedding aiming at
#'  explaining \eqn{Y}. The mixed embedding is formed by using the progressive 
#'  embedding algorithm based on conditional mutual information (CMI). 
#'  CMI is estimated by the method of nearest neighbors (Kraskov's method). 
#'  The function is the same as PMIMEsig.m but defines the stopping criterion
#'  differently, using a fixed rather than adjusted threshold. Specifically,
#'  the algorithm terminates if the contribution of the selected lagged
#'  variable in explaining the future response state is small enough, as
#'  compared to a threshold 'A'. Concretely, the algorithm terminates if 
#'         \eqn{I(x^F; w| wemb) / I(x^F; w,wemb) <= A}
#'  where \eqn{I(x^F; w| wemb)} is the CMI of the selected lagged variable w and 
#'  the future response state x^F given the current mixed embedding vector, 
#'  and \eqn{I(x^F; w,wemb)} is the MI between \eqn{x^F} and the augmented mixed
#'  embedding vector \eqn{wemb, w}.
#'  We experienced that in rare cases the termination condition is not 
#'  satisfied and the algorithm does not terminate. Therefore we included a 
#'  second condition for termination of the algorithm when the ratio 
#'  \eqn{I(x^F; w| wemb) / I(x^F; w,wemb)} increases in the last two embedding
#'  cycles. 
#'  The derived \eqn{R} measure indicates the information flow of time series \eqn{X} to
#'  time series \eqn{Y} conditioned on the rest time series in \eqn{Z}. The measure
#'  values are stored in a \eqn{K x K} matrix 'RM' and given to the output, where
#'  the value at position \eqn{(i,j)} indicates the effect from \eqn{i} to \eqn{j} (row to
#'  col), and the \eqn{(i,i)} components are zero. The library RANN was used 
#'  for the nearest neighbor estimation of the mutual information
#' 
#' @param allM the N x K matrix of the K time series of length N.
#' @param Lmax the maximum delay to search for X and Y components for the 
#' mixed embedding vector ,default is 5.
#' @param Tl Tl steps ahead that the mixed embedding vector has to explain.
#' Note that if T>1 the future vector is of length T and contains the samples
#' at times t+1,..,t+T ,dafault is 1. 
#' @param nnei number of nearest neighbors for density estimation ,default is 5
#' @param A the threshold for the ratio of CMI over MI of the lagged variables
#' for the termination criterion.
#' @param showtxt : if 0 or negative do not print out anything, if 1 print out
#' the response variable index at each run, if 2 or larger print also info for 
#' each embedding cycle ,default is 1.
#' @return *RM*: A K x K matrix containing the R values computed by PMIME using
#'           the surrogates for setting the stopping criterion. 
#'           *ecC*: cell array of K components, where each component is a matrix of 
#'           size E x 5, and E is the number of embedding cycles. For each 
#'           embedding cycle the following 5 results are stored:
#'           1. variable index, 2. lag index, 3. CMI of the selected lagged
#'           variable w and the future response state x^F given the current 
#'           mixed embedding vector, I(x^F; w| wemb). 4. MI between x^F and 
#'           the augmented mixed embedding vector wemb w, I(x^F; w,wemb).
#'           5. The ration of 3. and 4.: I(x^F; w| wemb)/I(x^F; w,wemb)
#'           
#' @seealso Kugiumtzis, D. "Direct-Coupling Information Measure from Nonuniform
#' Embedding." Physical Review E 87, no. 6 (June 25, 2013): 062918. 
#' \doi{10.1103/PhysRevE.87.062918}
#'  
#' @export
#' 
PMIME <- function(allM, Lmax = 5, Tl = 1, nnei = 5, A = 0.03, showtxt = 1) {
  maxcomps <- 20
  if (missing(showtxt)) {
    showtxt <- 1
  } else if (missing(A)) {
    showtxt <- 1
    A <- 0.03
  } else if (missing(nnei)) {
    showtxt <- 1
    A <- 0.03
    nnei <- 5
  } else if (missing(Tl)) {
    showtxt <- 1
    A <- 0.03
    nnei <- 5
    Tl <- 1
  } else if (missing(Lmax)) {
    showtxt <- 1
    A <- 0.03
    nnei <- 5
    Tl <- 1
    Lmax <- 5
  }
  if (is.null(A)) {
    A <- 0.03
  }
  if (is.null(nnei)) {
    nnei <- 5
  }
  if (is.null(Tl)) {
    Tl <- 1
  }
  if (is.null(Lmax)) {
    Lmax <- 5
  }
  N <- nrow(allM)
  K <- ncol(allM)
  wV <- rep(Lmax, K)
  # Standardization of the input matrix columnwise in [0,1].
  minallV <- as.vector( apply(allM, 2, min) )
  rang <- sweep(allM, 2, minallV, "-")
  rang <- sweep(rang, 2,as.vector( apply(rang, 2, range)[2, ]), "/")
  allM <- rang
  # Build up the lag matrix from all variables
  alllagM <- matrix(NA, N, sum(wV)) #lag matrix of all variables
  indlagM <- matrix(NA, K, 2) #Start and end of columns of each variable in lag matrix
  count <- 0
  for (iK in 1:K) {
    indlagM[iK, ] <- c(count + 1, count + wV[iK])
    alllagM[, indlagM[iK, 1]] <- allM[, iK]
    for (ilag in 1:(wV[iK] - 1)) {
      alllagM[(ilag + 1):N, (indlagM[iK, 1] + ilag) ] <- allM[1:(N - ilag), iK]
    }
    count <- count + wV[iK]
  }
  alllagM <- alllagM[Lmax:(N - Tl), ]
  N1 <- nrow(alllagM)
  alllags <- ncol(alllagM)
  # Find mixed embedding and R measure for purpose: from (X,Y,Z) -> Y 
  RM <- matrix(0, K, K)
  ecC <- vector("list", K)
  psinnei <- digamma(nnei) #Computed once here, to be called in several times
  psiN1 <- digamma(N1) # Computed once here, to be called in several times
  for (iK in 1:K) {
    if (showtxt == 1) {
      cat(iK, "..")
    } else if (showtxt >= 2) {
      cat("Response variable index=", iK, "..\n")
      cat("EmbeddingCycle  Variable  Lag  I(x^F;w|wemb)  I(x^F;w,wemb)  I(x^F;w|wemb)/I(x^F;w,wemb) \n")
    }
    Xtemp <- matrix(NA, N, Tl)
    for (iT in 1:Tl) {
      Xtemp[1:(N - iT), iT] <- allM[(1 + iT):N, iK]
    }
    xFM <- Xtemp[Lmax:(N - Tl), ] #The future vector of response
    # First embedding cycle: max I(y^T, w), over all candidates w
    miV <- rep(NA, alllags)
    for (i1 in 1:alllags) {
      
      xnowM <- cbind(xFM, alllagM[, i1])
      distsM <- RANN::nn2(xnowM, xnowM, nnei + 1)$nn.dists #annMaxquery(t(xnowM), t(xnowM), nnei + 1)
      maxdistV <- distsM[,(nnei + 1) ]
      nyFV= RANN::nn2(xFM, xFM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #annMaxquery(t(xnowM), t(xnowM), nnei + 1)
      nyFV  = apply(nyFV,1,function(x) sum(x>0 ) )
      nwcandV=RANN::nn2(alllagM[, i1], alllagM[, i1],k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx
      nwcandV  = apply(nwcandV,1,function(x) sum(x>0 ) )
      psibothM <- digamma(nyFV)+digamma(nwcandV)
      miV[i1] <- psinnei + psiN1 - mean(psibothM)
    }
    iembV <- which.max(miV)
    xembM <- alllagM[, iembV]
    varind <- ceiling(iembV / Lmax)
    lagind <- iembV %% Lmax
    if (lagind == 0) {
      lagind <- Lmax
    }
    ecC[[iK]] <- matrix(c(varind, lagind, miV[iembV], NA, NA), ncol = 5)
    if (showtxt >= 2) {
      cat(nrow(ecC[[iK]]), "\t", ecC[[iK]][nrow(ecC[[iK]]), 1], "\t", ecC[[iK]][nrow(ecC[[iK]]), 2], "\t", ecC[[iK]][nrow(ecC[[iK]]), 3], "\t", ecC[[iK]][nrow(ecC[[iK]]), 4], "\t", ecC[[iK]][nrow(ecC[[iK]]), 5], "\n")
    }
    terminator <- 0
    maxcomps <- min(ncol(alllagM), maxcomps)
    if(is.vector(xembM) ){ ncol_xembM=1 }else{ncol_xembM=ncol(xembM) }
    while ((terminator == 0) & (ncol_xembM < maxcomps) ) {
      activeV <- setdiff(1:alllags, iembV)
      cmiV <- rep(NA, alllags)
      miwV <- rep(NA, alllags)
      for (i1 in activeV) {
        xallnowM <- cbind(xFM, alllagM[, i1], xembM)
        
        distsM <- RANN::nn2(xallnowM, xallnowM, nnei + 1)$nn.dists  #annMaxquery(t(xallnowM), t(xallnowM), nnei + 1)
        maxdistV <- distsM[,(nnei + 1) ]
        #nyFV= RANN::nn2(xFM, xFM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #annMaxquery(t(xnowM), t(xnowM), nnei + 1)
        #nyFV  = apply(nyFV,1,function(x) sum(x>0 ) )
        nwV <- RANN::nn2(xembM, xembM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(xembM, maxdistV - rep(10^(-10), N1))
        nwV <-apply(nwV,1,function(x) sum(x>0 ) )
        
        nwcandV <-RANN::nn2(cbind(alllagM[, i1], xembM), cbind(alllagM[, i1], xembM),k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(cbind(alllagM[, i1], xembM), maxdistV - rep(10^(-10), N1))
        nwcandV=apply(nwcandV,1,function(x) sum(x>0 ) )
        
        nyFwV <-RANN::nn2(cbind(xFM, xembM), cbind(xFM, xembM),k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(cbind(xFM, xembM), maxdistV - rep(10^(-10), N1))
        nyFwV <-apply(nyFwV,1,function(x) sum(x>0 ) )
        
        digamma_nwcandV= digamma(nwcandV)
        psinowM <- digamma(nyFwV) +digamma_nwcandV -digamma(nwV)
        cmiV[i1] <- psinnei - mean(psinowM)
        
        nyFV <-RANN::nn2(xFM, xFM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(xFM, maxdistV - rep(10^(-10), N1))
        nyFV  = apply(nyFV,1,function(x) sum(x>0 ) )
        
        psinowM <- digamma(nyFV)+ digamma_nwcandV
        miwV[i1] <- psinnei + psiN1 - mean(psinowM)
      }
      ind <- which.max(cmiV)
      xVnext <- alllagM[, ind]
      varind <- ceiling(ind / Lmax)
      lagind <- ind %% Lmax
      if (lagind == 0) {
        lagind <- Lmax
      }
      ecC[[iK]] <- rbind(ecC[[iK]], c(varind, lagind, cmiV[ind], miwV[ind], cmiV[ind] / miwV[ind]))
      if (length(iembV) == 1) {
        if (showtxt >= 2) {
          cat(nrow(ecC[[iK]]), "\t", ecC[[iK]][nrow(ecC[[iK]]), 1], "\t", ecC[[iK]][nrow(ecC[[iK]]), 2], "\t", ecC[[iK]][nrow(ecC[[iK]]), 3], "\t", ecC[[iK]][nrow(ecC[[iK]]), 4], "\t", ecC[[iK]][nrow(ecC[[iK]]), 5], "\n")
        }
        if (ecC[[iK]][nrow(ecC[[iK]]), 5] > A) {
          xembM <- cbind(xembM, xVnext)
          iembV <- c(iembV, ind)
        } else {
          terminator <- 1
        }
      } else {
        if (length(iembV) == 2) {
          if (showtxt >= 2) {
            cat(nrow(ecC[[iK]]), "\t", ecC[[iK]][nrow(ecC[[iK]]), 1], "\t", ecC[[iK]][nrow(ecC[[iK]]), 2], "\t", ecC[[iK]][nrow(ecC[[iK]]), 3], "\t", ecC[[iK]][nrow(ecC[[iK]]), 4], "\t", ecC[[iK]][nrow(ecC[[iK]]), 5], "\n")
          }
          if (ecC[[iK]][nrow(ecC[[iK]]), 5] > A) {
            xembM <- cbind(xembM, xVnext)
            iembV <- c(iembV, ind)
          } else {
            terminator <- 1
          }
        } else {
          if (showtxt >= 2) {
            cat(nrow(ecC[[iK]]), "\t", ecC[[iK]][nrow(ecC[[iK]]), 1], "\t", ecC[[iK]][nrow(ecC[[iK]]), 2], "\t", ecC[[iK]][nrow(ecC[[iK]]), 3], "\t", ecC[[iK]][nrow(ecC[[iK]]), 4], "\t", ecC[[iK]][nrow(ecC[[iK]]), 5], "\n")
          }
          if (ecC[[iK]][nrow(ecC[[iK]]), 5] > A & (ecC[[iK]][nrow(ecC[[iK]]), 5] < ecC[[iK]][nrow(ecC[[iK]]) - 1, 5] | ecC[[iK]][nrow(ecC[[iK]]) - 1, 5] < ecC[[iK]][nrow(ecC[[iK]]) - 2, 5])) {
            xembM <- cbind(xembM, xVnext)
            iembV <- c(iembV, ind)
          } else {
            terminator <- 1
          }
        }
      }
    }
    if ( !is.null(iembV) & !is.null(which(iembV < indlagM[iK, 1] | iembV > indlagM[iK, 2])[1])) {
      xformM <- matrix(NA, length(iembV), 2)
      xformM[, 1] <- ceiling(iembV / Lmax)
      xformM[, 2] <- iembV %% Lmax
      xformM[xformM[, 2] == 0, 2] <- Lmax
      activeV <- unique(xformM[, 1])
      if (  iK %in% activeV) {
        inowV <- which(xformM[, 1] == iK)
        if(is.vector(xembM) ){ 
          xrespM <- xembM 
          activeV <- activeV
        }else{
          xrespM <- xembM[, inowV]
          activeV <- setdiff(activeV, iK)  
        }
        
      } else {
        xrespM <- NULL
      }
      KK <- length(activeV)
      indKKM <- matrix(NA, KK, 2)
      iordembV <- matrix(NA, length(iembV), 1)
      count <- 0
      for (iKK in 1:KK) {
        inowV <- which(xformM[, 1] == activeV[iKK])
        
        indKKM[iKK, ] <- c(count + 1, count + length(inowV))
        iordembV[indKKM[iKK, 1]:indKKM[iKK, 2]] <- inowV
        count <- count + length(inowV)
      }
      iordembV <- iordembV[1:indKKM[KK, 2]]
      if(is.vector(xembM) ){ xembM=xembM;ncol_xembM=1 }else{
        if( !(max(iordembV, na.rm = T)<=ncol(xembM)) ){
          xembM <- xembM[, iordembV];ncol_xembM=ncol(xembM)}else{
            xembM = xembM;ncol_xembM=ncol(xembM)
          } 
      }
      if (is.null(xrespM)) {
        xpastM <- xembM
      } else {
        xpastM <- cbind(xrespM, xembM)
      }
      dists <- RANN::nn2(cbind(xFM, xpastM), cbind(xFM, xpastM), nnei + 1)$nn.dists #annMaxquery(rbind(xFM, xpastM), rbind(xFM, xpastM), nnei + 1)
      maxdistV <- dists[,(nnei + 1) ]
      nyFV <-RANN::nn2(xFM, xFM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(xFM, maxdistV - rep(10^(-10), N1))
      nyFV  = apply(nyFV,1,function(x) sum(x>0 ) )
      
      nwV <-RANN::nn2(xpastM, xpastM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(xpastM, maxdistV - rep(10^(-10), N1))
      nwV  = apply(nwV,1,function(x) sum(x>0 ) )
      
      psi0V <- digamma(nyFV)+ digamma(nwV)
      psinnei <- digamma(nnei)
      IyFw <- psinnei + psiN1 - mean(psi0V)
      for (iKK in 1:KK) {
        indnowV <- indKKM[iKK, 1]:indKKM[iKK, 2]
        irestV <- setdiff(1:ncol_xembM, indnowV)
        
        if(is.vector(xembM) ){ xembMsub=xembM;ncol_xembM=1 }else{
          if( !(max(iordembV, na.rm = T)<=ncol(xembM)) ){
            xembM <- xembM[, iordembV];ncol_xembM=ncol(xembM)}else{
              xembM = xembM;ncol_xembM=ncol(xembM)
            } 
          
        }
        if (length(irestV) == 0 & length(xrespM) == 0) {
          xcondM <- NULL
        } else if (length(irestV) == 0 & length(xrespM) > 0) {
          xcondM <- xrespM
        } else if (length(irestV) > 0 & length(xrespM) == 0) {
          
          if(is.vector(xembM) ){  xcondM <- xembM} else{xcondM <- xembM[, irestV] }
          
        } else {
          xcondM <- if(is.vector(xembM) ){  cbind(xrespM, xembM)} else{cbind(xrespM, xembM[, irestV]) } 
        }
        
        if (is.null(xcondM)) {
          IyFwXcond <- IyFw
        } else {
          
          nxFcondV <-RANN::nn2(cbind(xFM, xcondM), cbind(xFM, xcondM),k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(cbind(xFM, xcondM), maxdistV - rep(10^(-10), N1))
          nxFcondV= apply(nxFcondV,1,function(x) sum(x>0 ) )
          
          ncondV <-RANN::nn2(xcondM, xcondM,k=nnei + 1, searchtype = c( "radius"), radius = maxdistV  )$nn.idx #nneighforgivenr(xcondM, maxdistV - rep(10^(-10), N1))
          ncondV= apply(ncondV,1,function(x) sum(x>0 ) )
          
          psinowV <- digamma(nxFcondV)+ digamma(nwV) -digamma(ncondV)
          IyFwXcond <- psinnei - mean(psinowV)
          
        }
        RM[activeV[iKK], iK] <- IyFwXcond / IyFw
      }
    }
    if (!is.null(ecC[[iK]])) {
      ecC[[iK]] <- ecC[[iK]][-nrow(ecC[[iK]]), ]
    }
  } # end for all K variables 
  if (showtxt > 0) {
    cat("\n")
  }
  diag(RM)=0
  return(list(RM = RM, ecC = ecC))
}

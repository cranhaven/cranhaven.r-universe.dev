#' Efficiency of the Proportion Estimator Calculated from Group Testing Data
#' 
#' This function provides relative efficiency results for the maximum likelihood estimator of disease prevalence (proportion), \eqn{p}. The estimator, \eqn{\hat{p}}, is calculated using test responses from commonly used group testing protocols. The relative efficiency values measure how effective group testing is for estimating the prevalence when compared to individual testing. The function also calculates expected testing and estimation costs. Please refer to Warasi and Das (2024), where the statistical methods were introduced.
#' 
#' @param p Proportion of individuals truly positive for a disease (i.e., disease prevalence).
#' @param Se Assay sensitivity, a scalar value.
#' @param Sp Assay specificity, a scalar value. 
#' @param initial.psz A vector of initial pool sizes.
#' @param protocol One of the six group testing protocols, where 'MPT' is the default.
#' @param criterion One of the three optimization criteria, where 'RTE' is the default.
#' @param N Number of individuals to be tested using group testing (i.e., sample size).
#' @param ngit Number of Gibbs iterates used in the EM algorithm.
#' @param maxit Maximum number of iterates in the EM algorithm.
#' @param tol Convergence tolerance for the EM algorithm.
#' @param nrep Number of repetitions used in the proposed computation algorithm.
#' @param seed A single seed value, an integer. 
#' @param ncore Number of CPU cores to be used in computing, where ncore => 1.
#'
#' @importFrom stats rbinom 
#' @importFrom binGroup2 opChar1
#' @importFrom binGroup2 ExpTests
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterSetRNGStream
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#'
#'
#' @details
#' The function \code{mle.prop.eff} computes three measures of efficiency: relative testing efficiency (RTE), relative estimation efficiency (REE), and relative cost efficiency (RCE). These measures can be calculated for six common group testing protocols: master pool testing (MPT), hierarchical testing with two, three, and four stages (H2, H3, and H4), and array testing without and with master pool testing (A2 and A2M). For more information on these protocols, refer to Kim et al. (2007). We use the term 'relative efficiency' because these measures compare group testing (numerator) with the usual one-at-a-time, i.e., individual testing (denominator).
#' 
#' In the paper, we defined 'RTE' and discussed how it can be calculated for both common and more complex group testing protocols. For the five multistage protocols (H2, H3, H4, A2, and A2M), our function provides RTE values based on the analytic expressions of Kim et al. (2007). These expressions have been coded by Hitt et al. (2023) in their R package 'binGroup2'. We developed R code to restructure the output obtained from binGroup2, so it is consistent with our proposed method. 
#' 
#' Based on the expressions in our article, we analytically calculate REE for MPT and H2 and also compute RCE for MPT. For other scenarios, we determine REE and RCE based on our proposed computation algorithm.
#' 
#' The expected costs, \eqn{E[T]}, \eqn{E[(\hat{p} - p)^2]}, and \eqn{E[T(\hat{p} - p)^2]}, are calculated for a given \eqn{N}, the number of individuals to be tested. The 'MPT' and 'H2' protocols require that \eqn{N} is completely divisible by the initial pool size \eqn{k}. If this is not the case, the integer that is closest to \eqn{N} and divisible by \eqn{k} will be used. It is worth noting that \eqn{N} is also used in the computation algorithm, where a large-sample assumption is made. We found that \eqn{N = 800} may be sufficient in most scenarios for the validity of this assumption, although we used \eqn{N = 1200} in the article; for more information, refer to Warasi and Das (2024).
#' 
#' Arguments 'ngit', 'maxit', and 'tol' are used in the EM algorithm. We found that 'ngit = 3000' Gibbs iterates are generally sufficient, but using a bigger 'ngit' may be more reliable. The default choices for 'maxit' and 'tol' are also reasonable. 
#'
#' Argument 'nrep' is necessary for the validity of the law of large numbers in the computation algorithm. While a larger choice of 'nrep' is generally preferable, our paper suggests that 5000 repetitions may be sufficient in most scenarios. Note that 'nrep = 3000' or so may also provide reasonable approximations. 
#'
#' Execution of 'RCE' for multistage protocols (H2, H3, H4, A2, and A2M) is quite slow because it uses Gibbs samplers in the computation algorithm. For the same reason, execution of 'REE' for H3, H4, A2, and A2M is also slow. To overcome this limitation, we included the `ncore` argument, which enables users to perform the computing tasks using parallel CPUs through the `parallel` package. The program works with `ncore = 1` or with any larger choice of `ncore`.
#'
#' Specifying a 'seed' ensures reproducibility of the results produced by our function, whether one uses a single core or multiple cores. Note that the seed value is used only when the computation algorithm is implemented.
#'
#'
#' @return A list with three matrix objects labeled as "efficiency", "expected_cost", "iterations":
#'   \itemize{
#'     \item \strong{"efficiency"}: A matrix that reports the relative efficiency results: RTE, REE, and RCE.
#'     \item \strong{"expected_cost"}: A matrix reporting the expected costs: \eqn{E[T]}, \eqn{E[(\hat{p} - p)^2]}, and \eqn{E[T(\hat{p} - p)^2]}, corresponding to the criteria RTE, REE, and RCE, respectively.
#'     \item \strong{"iterations"}: A matrix that provides the cumulative running average of the estimates (sample means) of \eqn{E[(\hat{p} - p)^2]} and \eqn{E[T(\hat{p} - p)^2]} when the computation algorithm is used. These running estimates can be useful for verification of the 'law of large numbers', as demonstrated in the Web Appendix of the article Warasi and Das (2024). In each row of the matrix, the first 1, 2, or 3 columns are for pool size(s) and the remaining 'nrep' columns provide the running averages; see the examples.
#'   }
#'   These results are provided for all possible pooling configurations within the specified range of initial pool sizes. The stage-specific pool sizes are also included in the results.
#'
#' @export
#'
#' @references
#' \itemize{
#' \item Kim HY, Hudgens M, Dreyfuss J, Westreich D, and Pilcher C. (2007). Comparison of Group Testing Algorithms for Case Identification in the Presence of Testing Error. \emph{Biometrics}, 63:1152-1163.
#' \item Zhang W, Liu A, Li Q, Albert P. (2020). Incorporating Retesting Outcomes for Estimation of Disease Prevalence. \emph{Statistics in Medicine}, 39:687-697.
#' \item Warasi and Das (2024). Optimizing Disease Surveillance Through Pooled Testing with Application to Infectious Diseases. \emph{Journal of Agricultural, Biological and Environmental Statistics}. In press.
#' }
#' 
#' @examples
#' 
#' library(groupTesting)
#' 
#' ## Gonorrhea data information:
#' p0 <- 0.041       # True prevalence  
#' Se <- 0.913       # Assay sensitivity
#' Sp <- 0.993       # Assay specificity
#' psz <- 2:6        # A range of initial pool sizes
#'  
#'  
#' ## Example 1: Two-stage hierarchical testing (H2)  
#' 
#' ## REE (using closed-form expressions)
#' res <- mle.prop.eff(p=p0, Se=Se, Sp=Sp, initial.psz=psz, protocol="H2", criterion="REE")
#' 
#' ## Output
#' 
#' # > res
#' # $efficiency
#' #      PSZ.S1 PSZ.S2    REE
#' # [1,]      2      1 0.8920
#' # [2,]      3      1 0.8932
#' # [3,]      4      1 0.8975
#' # [4,]      5      1 0.9034
#' # [5,]      6      1 0.9104
#'
#' # $expected_cost
#' # PSZ.S1 PSZ.S2 E[(phat-p)^2]
#' # [1,]      2      1  5.731668e-05
#' # [2,]      3      1  5.732655e-05
#' # [3,]      4      1  5.767260e-05
#' # [4,]      5      1  5.805423e-05
#' # [5,]      6      1  5.864758e-05
#'
#' # $iterations
#' # NULL
#' 
#' 
#' ## RTE (using closed-form expressions)
#' mle.prop.eff(p=p0, Se=Se, Sp=Sp, initial.psz=psz, protocol="H2", criterion="RTE")
#' 
#' ## RCE (using the computation algorithm)
#' # res <- mle.prop.eff(p=p0, Se=Se, Sp=Sp, initial.psz=psz, protocol="H2", seed=123, 
#' #   criterion="RCE", N=800, ngit=3000, maxit=200, tol=0.001, nrep=3000, ncore=4)
#' 
#' # Note: For 'H2' protocol, get cumulative running averages as:
#' # res2 <- res$iterations[ ,-(1:2)] 
#' # Now, plot each row; for example: plot(res2[1, ], type='l')
#' #
#' # Execution of 'RCE' for H2 is slow, as discussed in the 'details' section.
#' # The computing challenge can be overcome using multiple CPU cores as shown above. 
#' 
#' 
#' ## Example 2: Three-stage hierarchical testing (H3)
#' 
#' ## REE (using the computation algorithm)
#' # res <- mle.prop.eff(p=p0, Se=Se, Sp=Sp, initial.psz=psz, protocol="H3", seed=123, 
#' #   criterion="REE", N=800, ngit=3000, maxit=200, tol=0.001, nrep=3000, ncore=4)
#' #
#' # Note: For 'H3' protocol, get cumulative running averages as:
#' # res2 <- res$iterations[ ,-(1:3)]  # Now, plot each row: plot(res2[1, ], type='l')
#' 
#' ## RTE (using closed-form expressions)
#' # res <- mle.prop.eff(p=p0, Se=Se, Sp=Sp, initial.psz=psz, protocol="H3",  
#' #   criterion="RTE", N=800, ngit=3000, maxit=200, tol=0.001, nrep=3000, ncore=4)
#' 
#' ## RCE (using the computation algorithm)
#' # res <- mle.prop.eff(p=p0, Se=Se, Sp=Sp, initial.psz=psz, protocol="H3", seed=123, 
#' #   criterion="RCE", N=800, ngit=3000, maxit=200, tol=0.001, nrep=3000, ncore=4)
#' 
#' 
#' 
mle.prop.eff <- function(p, Se, Sp, initial.psz, protocol=c("MPT","H2","H3","H4","A2","A2M"), criterion=c("RTE", "REE", "RCE"), N=800, ngit=3000, maxit=200, tol=1e-03, nrep=3000, seed=NULL, ncore=1){
  M.psz <- sort(unique(initial.psz))
  protocol <- match.arg(protocol)
  criterion <- match.arg(criterion)
  if(criterion=="RTE"){
    res <- prop.RTE(p=p, Se=Se, Sp=Sp, initial.psz=initial.psz, protocol=protocol)
    lcol <- ncol(res)
    exp.value <- cbind(matrix(res[ ,-lcol], ncol=(lcol-1)), as.numeric(res[ ,lcol]*N))
    colnames(exp.value) <- c( paste("PSZ.S", 1:(lcol-1), sep=""), "E[T]" )
    iter <- NULL
  }
  if(criterion=="REE"){
    if(protocol=="MPT"){
      S <- 1
      gsz <- M.psz
      RE <- REE.mpt(p=p, Se=Se, Sp=Sp, kvec=M.psz)
      res <- cbind(gsz, round(RE, 4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "REE" )
      J <- N/M.psz
      J0 <- round(J)
      N0 <- J0*M.psz
      exp.value <- rep(-9, length(M.psz))
      for(s in 1:length(M.psz)){
        exp.value[s] <- prop.var.mpt(p=p, k=M.psz[s], J=J0[s], Se=Se, Sp=Sp)
      }
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[(phat-p)^2]" )
      iter <- NULL
    }
    if(protocol=="H2"){
      S <- 2
      gsz <- cbind(M.psz, 1)
      RE <- REE.H2(p=p, Se=Se, Sp=Sp, kvec=M.psz)
      res <- cbind(gsz, round(RE,4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "REE" )
      J <- N/M.psz
      J0 <- round(J)
      N0 <- J0*M.psz
      exp.value <- rep(-9, length(M.psz))
      for(s in 1:length(M.psz)){
        exp.value[s] <- prop.var.H2(p=p, K=M.psz[s], J=J0[s], Se=Se, Sp=Sp)
      }
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[(phat-p)^2]" )
      iter <- NULL
    }
    if(protocol=="H3" || protocol=="H4"){
      if(protocol=="H3"){
        S <- 3
        pszH <- S3.configs(psz = M.psz)
      }
      if(protocol=="H4"){
        S <- 4
        pszH <- S4.configs(psz = M.psz)
      }
      ind.var <- prop.var.ind(p=p, N=N, Se=Se[1], Sp=Sp[1])
      if(ncore == 1){
        out <- prop.var.hier(N=N, p=p, Se=rep(Se, S), Sp=rep(Sp, S), assayID=rep(1,S), kmat=pszH, nstg=S, ngit=ngit, nrep=nrep, seed=seed)
        exp.value <- out$sample_mean
        RE <- exp.value/ind.var
        iterations <- out$iterations
      }
      if(ncore > 1){
        cl <- parallel::makeCluster(ncore)
        parallel::clusterEvalQ(cl, {library(groupTesting)})
        # Setting seed:
        RNGkind(kind = "L'Ecuyer-CMRG")
        set.seed(seed)
        parallel::clusterSetRNGStream(cl = cl, iseed = seed)
        npsz <- nrow(pszH)
        iterations <- matrix(-9, npsz, nrep)
        exp.value <- rep(-9, npsz)
        for(i in 1:npsz){
          list.Z <- lapply(X=rep(N,nrep), FUN=hier.gt.sim, p=p, S=S, psz=pszH[i, ], Se=rep(Se,S), Sp=rep(Sp,S), assayID=rep(1,S))
          ginfo <- parallel::parLapply(cl, list.Z, prop.info.louis, p=p, ngit=ngit)
          g.info <- unlist(ginfo)
          exp.value[i] <- 1/mean(g.info)
          iterations[i, ] <- (1:nrep)/cumsum(g.info)
        }
        parallel::stopCluster( cl )
        RE <- exp.value/ind.var
      }
      gsz <- pszH
      res <- cbind(gsz, round(RE,4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "REE" )
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[(phat-p)^2]" )
      iter <- cbind(gsz, iterations)
      colnames(iter) <- NULL
    }
    if(protocol=="A2" || protocol=="A2M"){
      if(protocol=="A2"){
        S <- 2
        gsz <- cbind(M.psz, 1)
      }
      if(protocol=="A2M"){
        S <- 3
        gsz <- cbind(M.psz^2, M.psz, 1)
      }
      ind.var <- prop.var.ind(p=p, N=N, Se=Se[1], Sp=Sp[1])
      if(ncore == 1){
        out <- prop.var.array(N=N, p=p, protocol=protocol, nvec=M.psz, Se=rep(Se,S), Sp=rep(Sp,S), assayID=rep(1,S), ngit=ngit, nrep=nrep, seed=seed)
        exp.value <- out$sample_mean	
        RE <- exp.value/ind.var
        iterations <- out$iterations
      }
      if(ncore > 1){
        cl <- parallel::makeCluster(ncore)
        parallel::clusterEvalQ(cl, {library(groupTesting)})
        # Setting seed:
        RNGkind(kind = "L'Ecuyer-CMRG")
        set.seed(seed)
        parallel::clusterSetRNGStream(cl = cl, iseed = seed)
        npsz <- length(M.psz)
        iterations <- matrix(-9, npsz, nrep)
        exp.value <- rep(-9, npsz)
        for(i in 1:npsz){
          list.Z <- lapply(X=rep(N,nrep), FUN=array.gt.sim, p=p, protocol=protocol, n=M.psz[i], Se=rep(Se, S), Sp=rep(Sp, S), assayID=rep(1, S))
          ginfo <- parallel::parLapply(cl, list.Z, prop.info.louis, p=p, ngit=ngit)
          g.info <- unlist(ginfo)
          exp.value[i] <- 1/mean(g.info)
          iterations[i, ] <- (1:nrep)/cumsum(g.info)
        }
        parallel::stopCluster( cl )
        RE <- exp.value/ind.var
      }
      res <- cbind(gsz, round(RE,4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "REE" )
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[(phat-p)^2]" )
      iter <- cbind(gsz, iterations)
      colnames(iter) <- NULL
    }
  }
  if(criterion=="RCE"){
    if(protocol=="MPT"){
      S <- 1
      gsz <- M.psz
      RC <- RCE.mpt(p=p, Se=Se, Sp=Sp, kvec=M.psz)
      res <- cbind(gsz, round(RC,4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "RCE" )
      
      J <- N/M.psz
      J0 <- round(J)
      N0 <- J0*M.psz
      T0 <- N0/J0
      exp.value <- rep(-9, length(M.psz))
      for(s in 1:length(M.psz)){
        exp.value[s] <- T0[s]*prop.var.mpt(p=p, k=M.psz[s], J=J0[s], Se=Se, Sp=Sp)
      }
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[T*(phat-p)^2]" )
      iter <- NULL
    }
    if(protocol=="H2" || protocol=="H3" || protocol=="H4"){
      if(protocol=="H2"){
        S <- 2
        pszH <- cbind(M.psz, 1)
      }
      if(protocol=="H3"){
        S <- 3
        pszH <- S3.configs(psz = M.psz)
      }
      if(protocol=="H4"){
        S <- 4
        pszH <- S4.configs(psz = M.psz)
      }
      ind.var <- prop.var.ind(p=p, N=N, Se=Se[1], Sp=Sp[1])
      if(ncore == 1){
        out <- prop.CPUI.hier(N=N, p=p, Se=rep(Se,S), Sp=rep(Sp,S), assayID=rep(1,S), kmat=pszH, nstg=S, ngit=ngit, nrep=nrep, maxit=maxit, tol=tol, seed=seed)
        exp.value <- out$sample_mean
        RC <- exp.value/(N*ind.var)
        iterations <- out$iterations
      }
      if(ncore > 1){
        cl <- parallel::makeCluster(ncore)
        parallel::clusterEvalQ(cl, {library(groupTesting)})
        # Setting seed:
        RNGkind(kind = "L'Ecuyer-CMRG")
        set.seed(seed)
        parallel::clusterSetRNGStream(cl = cl, iseed = seed)
        npsz <- nrow(pszH)
        iterations <- matrix(-9 ,npsz, nrep)
        exp.value <- rep(-9, npsz)
        for(i in 1:npsz){
          hdata <- lapply(X=rep(N,nrep), FUN=hier.gt.sim, p=p, S=S, psz=pszH[i, ], Se=rep(Se,S), Sp=rep(Sp,S), assayID=rep(1,S))
          T <- sapply(X=hdata, FUN=nrow)
          phat <- parallel::parLapply(cl, hdata, propMLE.gt, p0=p, ngit=ngit, maxit=maxit, tol=tol)
          tmp <- T*(unlist(phat) - p)^2
          exp.value[i] <- mean( tmp )
          iterations[i, ] <- cumsum(tmp)/(1:nrep)
        }
        parallel::stopCluster( cl )
        RC <- exp.value/(N*ind.var)
      }
      gsz <- pszH
      res <- cbind(gsz, round(RC,4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "RCE" )
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[T*(phat-p)^2]" )
      iter <- cbind(gsz, iterations)
      colnames(iter) <- NULL
    }
    if(protocol=="A2" || protocol=="A2M"){
      if(protocol=="A2"){
        S <- 2
        gsz <- cbind(M.psz, 1)
      }
      if(protocol=="A2M"){
        S <- 3
        gsz <- cbind(M.psz^2, M.psz, 1)
      }
      ind.var <- prop.var.ind(p=p, N=N, Se=Se[1], Sp=Sp[1])
      if(ncore == 1){
        out <- prop.CPUI.array(N=N, p=p, protocol=protocol, nvec=M.psz, Se=rep(Se,S), Sp=rep(Sp,S), assayID=rep(1,S), ngit=ngit, nrep=nrep, maxit=maxit, tol=tol, seed=seed)
        exp.value <- out$sample_mean
        RC <- exp.value/(N*ind.var)
        iterations <- out$iterations
      }
      if(ncore > 1){
        cl <- parallel::makeCluster(ncore)
        parallel::clusterEvalQ(cl, {library(groupTesting)})
        # Setting seed:
        RNGkind(kind = "L'Ecuyer-CMRG")
        set.seed(seed)
        parallel::clusterSetRNGStream(cl = cl, iseed = seed)
        npsz <- length(M.psz)
        iterations <- matrix(-9, npsz, nrep)
        exp.value <- rep(-9, npsz)
        for(i in 1:npsz){
          adata <- lapply(X=rep(N,nrep), FUN=array.gt.sim, p=p, protocol=protocol, n=M.psz[i], Se=rep(Se, S), Sp=rep(Sp, S), assayID=rep(1, S))
          T <- sapply(X=adata, FUN=nrow)
          phat <- parallel::parLapply(cl, adata, propMLE.gt, p0=p, ngit=ngit, maxit=maxit, tol=tol)
          tmp <- T*(unlist(phat) - p)^2
          exp.value[i] <- mean( tmp )
          iterations[i, ] <- cumsum(tmp)/(1:nrep)
        }
        parallel::stopCluster( cl )
        RC <- exp.value/(N*ind.var)
      }
      res <- cbind(gsz, round(RC,4))
      colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "RCE" )
      exp.value <- cbind( gsz, exp.value )
      colnames(exp.value) <- c( paste("PSZ.S", 1:S, sep=""), "E[T*(phat-p)^2]" )
      iter <- cbind(gsz, iterations)
      colnames(iter) <- NULL  
    }
  }
  return(list("efficiency" = res, "expected_cost" = exp.value, "iterations" = iter))
}


#' @import utils
## quiets concerns of R CMD check re: the .'s that appear in pipelines
#if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",">"))
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



#' FastKerFdr
#'
#' @description Computes H1 posteriors of the Z-scores.
#' @param Z A vector containing Zscores
#' @param p0 A double between 0 and 1. A priori proportion of H0 hypotheses
#' @param plotting A boolean saying to plot or not (FALSE by default)
#' @param NbKnot The (maximum) number of knot for the kde procedure.(1e5 by default)
#' @param tol a tolerance value for convergence (1e-5 by default)
#' @param max_iter the maximum number of iterations allowed for the algorithm to converge or complete its process.(Default is 1e4.)
#' @return  tau is the vector of H1 posteriors
#' @import graphics
#' @importFrom ks kde
#' @importFrom stats dnorm


FastKerFdr <- function(Z,p0,plotting=FALSE,NbKnot=1e5,tol = 1e-5,max_iter = 1e4){

  ## Get the data and remove the NAs
  n.all <- length(Z)
  ind.notNA <- !is.na(Z)
  Z <- Z[ind.notNA] %>% as.matrix %>% as.numeric
  n <- length(Z)
  p1 <- 1 - p0

  ## Knots and counts
  if(length(Z)>NbKnot){
    Hist = graphics::hist(Z, breaks=NbKnot, plot=FALSE)
    Knots = Hist$mids; ActualNbKnot = length(Knots); Counts = Hist$counts;
  } else {
    Knots = Z; ActualNbKnot = length(Z); Counts = rep(1,ActualNbKnot);
  }
  if (plotting){
    Order <- order(Knots)
    Knots <- Knots[Order]
    Counts <- Counts[Order]
  }
  
  h <- ks::hpi(Z)
  
  ## Initialize the taus
  phi <-  stats::dnorm(Knots)
  MaxKnot <- range(Knots) %>% abs %>% max
  tau <- sign(Knots)*0.8*Knots/MaxKnot+0.1

  ## Get the weighted kernel density estimate
  diff <- 2 * tol
  iter <- 0
  while(diff > tol & iter <= max_iter){
    iter <- iter + 1
    
    weights <- tau*Counts
    weights <- ActualNbKnot * weights / sum(weights)
    
    f1 <- ks::kde(x=Knots, w=weights, eval.points=Knots, h=h)$estimate
    f1[f1 < 0] <- 0
    
    tauNew <- p1*f1/(p0*phi + p1*f1)

    diff <- max(abs(tau - tauNew))
    tau <- tauNew
  }
  
  if (iter > max_iter & diff > tol) {
    message(paste0("Warning: The algorithm did not converge within max_iter=", max_iter, "."))
  }
  
  if(plotting){
    Hist.fig <- hist(Z, freq=TRUE, breaks=sqrt(n), main='', border=8,
                     xlab="Q-transformed pvalues", ylab="Densities")
    bin.width <- mean(diff(Hist.fig$breaks))
    lines(Knots, n*bin.width*p0*phi, type='l', col=4, lwd=2);
    lines(Knots, n*bin.width*p1*f1, col=2,lwd=2);
    lines(Knots, n*bin.width*(p0*phi+p1*f1), lwd=2)
    legend("topright", legend=c("H0 dist", "H1 dist","Mixture Dist"),
           col=c("blue","red", "black"), lty=c(1,1,2), cex=0.8)
  }

  ## Now get the f1 estimate
  KDE <- ks::kde(x=Knots, w=weights, eval.points=Z)
  f1 <- KDE$estimate
  
  ## Dirty job 2: get rid of numeric problems
  f1[f1<0] <- 1e-30
  tau <- p1*f1 / (p1*f1 + p0*dnorm(Z))

  ## Add the NA
  tau.all <- rep(NA,n.all)
  tau.all[ind.notNA] <- tau

  return(tau.all)
}


#' GetH0Items
#'
#' @description This function give the index of the markers which seems not significant (under H0)
#' @param Zmat A matrix containing the Zscore (in rows) for each environment (in columns)
#' @param Threshold Threshold on posteriors (to be H1) to filter markers for correlation computation (0.6 by default)
#' @param plotting A boolean saying to plot or not (FALSE by default)
#' @param Cores The number of cores to used, optional. By default, availableCores()-1 cores is used.
#' @return  A vector of index of markers which seems not significant (under H0)
#' @import future
#' @importFrom stats qnorm


GetH0Items <- function(Zmat, Threshold=0.8, plotting=FALSE,Cores=NULL){

  Zmat <- Zmat %>% as.matrix
  n <- nrow(Zmat)
  Q <- ncol(Zmat)

  #### Step 1: Marginal density estimation

  ## Get p0 estimates
  p0 <- rep(0, Q)
  for (q in 1:Q){
    p0[q] <- min(sum(abs(Zmat[,q]) < stats::qnorm(p = 0.975),na.rm=TRUE)/(0.95*sum(!is.na(Zmat[,q]))),1-1/sum(!is.na(Zmat[,q])))
  }

  ## Fit a 2-component mixture to each test serie using kerFdr
  if(is.null(Cores)){
    if(future::availableCores()>2){
      future::plan(multicore, workers= future::availableCores()-1)
    }
  }else if(Cores>1 & Cores <= future::availableCores() ){
    future::plan(multicore,workers= Cores)
  }
  GetTheTaus <- furrr::future_map(1:Q, ~ FastKerFdr(Zmat[, .x], p0=p0[.x], plotting=FALSE))
  plan("default")

  #### Step 2: get the null guys

  ##INITIAL FILTER
  H0Filter <- function(thres){
    GetTheTaus %>% purrr::map(~ which(.x<thres | is.na(.x))) %>% reduce(intersect)
  }
  H0Items <- purrr::map(Threshold, H0Filter)
  ##ALTERNATIVE FILTER
  #Tmp <- GetTheTaus %>% map(~ which(.x>Threshold)) %>% reduce(c) %>% table
  #H0Items <- names(Tmp)[which(Tmp>floor(0.15*length(GetTheTaus)))] %>%
  #  as.numeric %>%
  #  setdiff(1:n,.)

  return(H0Items)
}


#' Infer inter-environment correlation matrix
#'
#' @description This function infer the inter-environment correlation matrix from the z-scores after filtering markers with high probability of being under H1.
#' @param Data A dataset containing the effects and pvalues of each marker (in rows) in each environment (in columns) as obtained by [metaGE.collect()].
#' @param Threshold Threshold on posteriors (to be H1) to filter markers before computing correlation (\code{0.6} by default).
#' @param NA.omit A boolean: should the NA be removed for the inter-environment correlation matrix computation (\code{TRUE} by default).
#' @param Cores The number of cores to used, optional. By default, \code{availableCores()-1} cores is used.
#' @return  The inter-environment correlation matrix
#' @import dplyr tidyr corrplot
#' @importFrom stats qnorm cor
#' @export
#' @examples
#' require(corrplot)
#' data("metaData")
#' Threshold <- 0.8
#' matCorr <- metaGE.cor(metaData, Threshold = Threshold)
#' corrplot(matCorr,order = "hclust")

metaGE.cor<- function(Data,
                         Threshold = 0.6,
                         NA.omit = TRUE,
                         Cores=NULL) {
  ## Get the data
  Data <- dplyr::select(Data, contains('EFFECT.'), contains('PVAL.'))
  if (NA.omit) {
    Data <- tidyr::drop_na(Data)
  }
  if (sum(is.na(Data)) == 0) {
    NA.omit  <- TRUE
  }

  ## Compute the Zscore per environment
  SignDF <- Data %>%
    select(contains('EFFECT.')) %>%
    map_df(sign)
  qnorm_function <- function(x) {
    stats::qnorm(x / 2) * (-1)
  }
  TransPvalDF <- Data %>%
    select(contains('PVAL.')) %>%
    map_df(qnorm_function)
  Zmat <- SignDF * TransPvalDF

  ## Get correlation matrix according to the required method

  ### Filter the H1 guys if required
  if (!is.null(Threshold) & Threshold != 0) {
    H0List <- GetH0Items(Zmat = Zmat,Threshold =  Threshold, Cores = Cores)
  } else{
    H0List <- list(1:nrow(Zmat))
  }

  if (NA.omit) {
    MatCorr <- purrr::map(H0List, ~ {
      MC <- cor(Zmat[.x, ])
      colnames(MC) <- row.names(MC) <- colnames(MC) %>%
        str_remove('EFFECT.')
      return(MC)
    })
  } else {
    K <- ncol(Zmat)
    MatCorr.NA <- function(H0Items) {
      MC <- matrix(NA, K, K)
      for (i in 1:K) {
        res <- map_dbl(i:K, ~ {
          indList <-
            list(which(!is.na(Zmat[, .x])), which(!is.na(Zmat[, i])), H0Items)
          ind <- reduce(indList, intersect)
          corZ1Z2 <- cor(Zmat[ind, c(i, .x)])
          return(corZ1Z2[1, 2])
        })
        MC[i, i:K] <- res
        MC[i:K, i] <- res
      }
      colnames(MC) <- row.names(MC) <- names(Zmat) %>%
        str_remove('EFFECT.')
      return(MC)
    }

    MatCorr <- purrr::map(H0List, MatCorr.NA)
  }

  if (length(Threshold) == 1) {
    MatCorr <- MatCorr[[1]]
  }
  return(MatCorr)
}

#' Data Generation
#'
#' @usage generate.data(N,Mu0.list,Theta0.list,Sigma0.list)
#' @param N K0 * 1 vector, the sample sizes of subgroups.
#' @param Mu0.list A list including K0 mean vectors (p * 1).
#' @param Theta0.list A list including K0 precision matrices (p * p).
#' @param Sigma0.list A list including K0 correlation matrices (p * p).
#'
#' @return The simulated data and the true parameters.
#'
#' @importFrom MASS mvrnorm
#'
#' @export
#'
#' @examples
#' n <- 200              # The sample size of each subgroup
#' p <- 20               # The dimension of the precision matrix
#' K0 <- 3               # The true number of subgroups
#' N <- rep(n,K0)        # The sample sizes of K0 subgroups
#'
#' ################ The true parameters ################
#' mue <- 1.5
#' nonnum <- 4
#' mu01 <- c(rep(mue,nonnum),rep(-mue,nonnum),rep(0,p-2*nonnum))
#' mu02 <- c(rep(mue,2*nonnum),rep(0,p-2*nonnum))
#' mu03 <- c(rep(-mue,2*nonnum),rep(0,p-2*nonnum))
#'
#' # Power law network
#' set.seed(2)
#' A.list <- Power.law.network(p,s=5,I2=c(1),I3=c(2))
#' Theta01 <- A.list$A1
#' Theta02 <- A.list$A2
#' Theta03 <- A.list$A3
#' sigma01 <- solve(Theta01)
#' sigma02 <- solve(Theta02)
#' sigma03 <- solve(Theta03)
#' Mu0.list <- list(mu01,mu02,mu03)
#' Sigma0.list <- list(sigma01,sigma02,sigma03)
#' Theta0.list <- list(Theta01,Theta02,Theta03)
#'
#' ################ Generating simulated data ################
#' whole.data <- generate.data(N,Mu0.list,Theta0.list,Sigma0.list)
#'
generate.data = function(N,Mu0.list,Theta0.list,Sigma0.list){

  ## ---------------------------------------------------------------------------------------------------------------
  ## The name of the function: generate.data
  ## ---------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Generating the simulated data.
  ## ---------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R packages: mvtnorm
  ## ---------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ N: K0 * 1 vector, the sample sizes of subgroups.
  ## @ Mu0.list: a list including K0 mean vectors (p * 1).
  ## @ Theta0.list: a list including K0 precision matrices (p * p).
  ## @ Sigma0.list: a list including K0 correlation matrices (p * p).
  ## ---------------------------------------------------------------------------------------------------------------
  ## Output:
  ## A list "whole.data" including:
  ## @ L0: n * 1 vector, the subgroup labels to which each sample belongs.
  ## @ Mu0: K0 * p matrix, K0 mean vectors.
  ## @ Theta0: K0 * p * p array, K0 precision matrices.
  ## @ data: n * p matrix, the design matrix.
  ## @ n_all: int, the total sample size.
  ## @ K0: int, the true number of subgroups.
  ## ---------------------------------------------------------------------------------------------------------------

  K0 = length(Mu0.list)
  p = length(Mu0.list[[1]])
  Mu0=matrix(0,K0,p);L0=NULL;Theta0=array(0, dim = c(p, p, K0));data=NULL
  for (k in 1:K0) {
    Mu0[k,] <- Mu0.list[[k]]
    L0 <- c(L0,rep(k,N[k]))
  }
  for (k in 1:K0) {
    Theta0[,,k] <- as.matrix(Theta0.list[[k]])
  }
  for (k in 1:K0) {
    data <- rbind(data,mvrnorm(N[k],Mu0[k,],Sigma0.list[[k]]))
  }
  n_all = dim(data)[1]
  whole.data=list()
  whole.data$L0=L0
  whole.data$Mu0=Mu0
  whole.data$Theta0=Theta0
  whole.data$data=data
  whole.data$n_all=n_all
  whole.data$K0=K0
  return(whole.data)
}

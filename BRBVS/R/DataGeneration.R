#' Data Generation with Relevant and Noisy Covariates
#'
#' This function generates a design matrix with both relevant and noisy covariates.
#' It is designed to simulate data for survival analysis with potential censoring and covariates.
#'
#' @param nn Integer. The number of rows for the design matrix.
#' @param Sigma Matrix. A variance-covariance matrix used for the informative covariates in the data generation process.
#' @param pp Integer. Total number of covariates (relevant + noisy).
#' @param corr Numeric. Specifies the correlation matrix of the noisy covariates. If set to 0, an identity matrix is used.
#' @param pp.rel Integer. Number of relevant covariates.
#' @param Beta.list List. A list containing coefficients for the linear predictors used in the data generation process.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{dataSimFull}: A data frame with all the generated variables including relevant and noisy covariates.
#'     \item \code{Rel.Sim}: A subset of the generated data containing only the relevant covariates.
#'     \item \code{XXNoisy}: A matrix of the generated noisy covariates.
#'     \item \code{XX.Full}: A full design matrix combining both relevant and noisy covariates.
#'   }
#' @noRd
Data.generation <- function(nn, Sigma, pp, corr, pp.rel, Beta.list ){

  # Check for Sigma
  if (!is.matrix(Sigma) || nrow(Sigma) != pp.rel || ncol(Sigma) != pp.rel) {
    stop("Error: Sigma must be a square matrix of dimension pp.rel x pp.rel.")
  }
  eig <- eigen(Sigma)$values
  if (any(eig < 0)) {
    stop("Error: Sigma must be a positive semi-definite variance-covariance matrix.")
  }

  # Check for corr
  if (corr < 0 || corr > 1) {
    stop("Error: corr must be a value between [0,1].")
  }
  #################################################
  ## Input
  #################################################
  # pp.rel = are the number of relevant covariates defined
  # pp = is the number of relevant covariates + noisy covariates
  # nn = number of row of  the design matrix
  # corr = specify the correlation matrix of the noisy covariates
  # sigma = specify the variance-covariance matrix of the informative covariates (covariates used in the DGP)

  #################################################
  ## Output
  #################################################

  # This functions generates a design matrix of dimension nn \times pp

  output <- datagenCopulaMixCens(n=nn, Sigma ,Beta.coef = Beta.list)
  dataSim = output$dataSim

  #sequence that generates noisy covariates.
  # if corr=0

  if(corr==0){
    m.cov=diag(1, pp-pp.rel)
    XX.matrix <- matrix(stats::rnorm(nn*(pp-pp.rel), 0, 1), nrow=nn, ncol=pp-pp.rel)
    colnames(XX.matrix) <- c(paste0('z', (pp.rel+1):pp ))

  }else{

    m.cov <- matrix(rep(corr, (pp-pp.rel)^2), ncol=(pp-pp.rel))
    diag(m.cov) <- rep(1, pp-pp.rel)
    m.mean <- rep(0, pp-pp.rel)
    XX.matrix <- mvtnorm::rmvnorm(nn, m.mean, m.cov)
    colnames(XX.matrix) <- c(paste0('z', (pp.rel+1):pp ))
  }

  Simulated.Data<- cbind(dataSim,XX.matrix)
  XX.Full <- cbind(dataSim[,c(paste0('z', 1:pp.rel ))],XX.matrix )


return( list(dataSimFull=Simulated.Data, Rel.Sim=dataSim, XXNoisy=XX.matrix, XX.Full= XX.Full))



}








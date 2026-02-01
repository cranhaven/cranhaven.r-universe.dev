#' @title NetworkGuided
#'
#' @description A main function to obtain network-guided penalized
#' regression coefficient estimates.
#'
#' @param Y A continuous outcome variable.
#' @param X A data matrix of dimension n x p representing samples (rows) by
#'      features (columns).
#' @param hubs A vector of hubs idenfitied through identifyHubs function from
#'        our package.
#' @param Z A matrix of clinical or demographic covariates.
#' @param nfolds A user-specified numeric value for k-fold cross-validation.
#'
#' @return A vector of network-guided penalized regression coefficients.
#'
#' @examples
#' library(plsgenomics)
#' data(Colon) ## Data from plsgenomics R package
#' X = data.frame(Colon$X[,1:100]) ## The first 100 genes
#' Z = data.frame(Colon$X[,101:102]) ## Two clinical covariates
#' colnames(Z) = c("Z1", "Z2")
#' Y = as.vector(Colon$X[,1000])  ## Continuous outcome variable
#'
#' ## Apply identifyHubs():
#' preNG = identifyHubs(X=X, delta=0.05, tau=5, ebic.gamma = 0.1)
#'
#' ## Explore preNG results:
#' hubs = preNG$hubs ## Returns the names of the identified hub nodes.
#'
#' ## Use our main NetworkGuided function, to obtain network-guided
#' ## penalized regression coefficient estimates.
#' NG = NetworkGuided(Y=Y, X=X, hubs=preNG$hubs, Z=Z, nfolds=5)
#' NG$coef
#' @export
#' @import huge
#' @import glmnet
#' @import dplyr
#' @import plsgenomics
#' @importFrom stats cov2cor coef

NetworkGuided = function(Y, X, hubs, Z, nfolds=5){

  H = data.frame(X) %>% select(all_of(hubs))
  W = cbind(Z, H)
  N = data.frame(X) %>% select(-all_of(hubs))
  newX = data.matrix(cbind(W, N))

  # weights construction
  enet.ini = cv.glmnet(newX, Y, nfolds=nfolds, alpha=0.5)
  abs.enet = 1/(abs(coef(enet.ini)) + 1/nrow(newX))
  weights = abs.enet
  weights = weights[-1] # except intercept
  weights[1:length(W)] = 0 # for partial penalization

  # network-guided penalization
  ng.alass = cv.glmnet(newX, Y, nfolds=nfolds, penalty.factor=weights)
  coefficients = coef(ng.alass)
  return(list(coef=coefficients))

}

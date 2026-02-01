#' @title identifyHubs
#'
#' @description A function to identify hub nodes (i.e., genes or proteins)
#'    from high-dimensional data using network-based criteria.
#'
#' @param X A data matrix of dimension n x p representing samples (rows) by
#'      features (columns).
#' @param delta A numeric value indicating the proportion of nodes to
#'      considered as hubs in a network.
#' @param tau A user-specified cutoff for the number of hubs.
#' @param ebic.gamma A numeric value specifying the tuning parameter for the
#'      extended Bayesian information criterion (eBIC)
#'      used in network estimation.
#'
#' @return A list containing (1) the selected sparse graph structure and
#'       model selection results; (2) a data frame of feature names with
#'       their associated network characteristics (e.g., degree centrality);
#'       and (3) a character vector of top-ranked hub features
#'       (e.g., hub genes or proteins).
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
#' ## To display the degree centrality for each node,
#' ## sorted from strongest to weakest.
#' preNG$assoResults
#' preNG$hubs ## Returns the names of the identified hub nodes.
#'
#' @export
#' @import huge
#' @import glmnet
#' @import dplyr
#' @import plsgenomics
#' @importFrom stats cov2cor coef

identifyHubs = function(X, delta, tau, ebic.gamma = 0.1){

  # to disable in-function printed message for huge()
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  hugeResults = quiet(huge(as.matrix(X), method = "glasso"))
  hugeSelectResults = quiet(huge.select(hugeResults, criterion = "ebic", ebic.gamma = ebic.gamma))

  precMat = hugeSelectResults$opt.icov
  assomat = cov2cor(precMat)

  results = vector()
  for(k in 1:ncol(assomat)) {
    results[k] = sum(abs(assomat[k, -k]))
  }

  index = order(results, decreasing=T)
  phi = sort(results, decreasing = T)
  assoResults = data.frame(index=colnames(X)[index], phi=phi)

  top = min(floor(ncol(assomat)*delta), tau)
  hubs = colnames(X)[index[1:top]]

  return(list(hugeSelectResults=hugeSelectResults,
              assoResults=assoResults,
              hubs=hubs
  ))
}

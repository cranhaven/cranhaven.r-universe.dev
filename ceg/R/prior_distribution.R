

#' Prior.distribution
#'
#'
setClass("Prior.distribution"
         #    representation(score = "numeric", cluster = "list"),
          # contains = "Model.search.algorithm"
)





#' PriorDistribution
#'
#' \code{PriorDistribution} initialises the prior distributions under the
#' conservative and uniform assumptions for the hyperparameter 'alpha' over
#' the event tree.
#'
#' @param stratified.event.tree  "Stratified.event.tree" a S4 object that represents an event tree.
#' @param alpha  numeric It plays a role of a phantom sample to construct 
#'                        the prior probability distribution and reprssents 
#'                        the prior knowledge about the process.
#'
#' @return  prior is a list of matrices. Each matrix is a collection of
#' vectors that correspond to a prior for each situation associated with
#' a particular variable.
#' @seealso \code{\link{PriorVariable}}
#'
PriorDistribution <- function(stratified.event.tree, alpha) {
  alpha.edge <- lapply(1:(stratified.event.tree@num.variable), function(x)
    AlphaEdgeInternal(x, stratified.event.tree, alpha))
  prior <- lapply(1:(stratified.event.tree@num.variable),
                  function(x) PriorVariable(stratified.event.tree@num.situation[x],
                                             alpha.edge[[x]]))
  return(prior)
}



#'   PriorVariable
#'
#'   The function \code{PriorVariable}  yields the prior distributions for
#'   all situations associated with a particular variable in the event tree.
#'
#' @param ref         numeric  - It indicates the variable.
#' @param alpha.edge  vector   - Dirichlet hyperparameter vector of a situation
#'                                 associated with a particular variable.
#'
#' @return   a matrix. Each row represents the Dirichlet hyperparameter vector of a 
#'                     situation associated with a particular variable in the event tree.
#'
#' @seealso  \code{Prior.distribution} and
#'          \code{AlphaEdgeInternal}
#'
PriorVariable <- function(ref, alpha.edge) {
  if (ref < 1)
    return(c())
  return(rbind(PriorVariable(ref - 1, alpha.edge),
               alpha.edge,
               deparse.level = 0))
}




#' AlphaEdgeInternal
#'
#' \code{AlphaEdgeInternal} yields a possible objective prior distribution for each situation
#' associated with a particular variable in the event tree.
#'
#' @param level numeric - It indicates the level in the event tree.
#' @param stratified.event.tree  Stratified.event.tree - S4 object that represents an event tree.
#' @param alpha numeric - It plays a role of a phantom sample to construct 
#'               the prior probability distribution of a situation associated
#'               with a particular variable in the event tree.
#'
#' @return "vector"  - Dirichlet hyperparameter vector of a situation
#'                     associated with a particular variable.
#'
AlphaEdgeInternal <- function(level, stratified.event.tree, alpha) {
  if (level <= stratified.event.tree@num.variable) {
    variable <- level
    result <- rep(alpha / (stratified.event.tree@num.category[variable] * stratified.event.tree@num.situation[level]),
                  stratified.event.tree@num.category[variable])
  } else {
    variable <- level - stratified.event.tree@num.variable
    result <- rep(alpha / (stratified.event.tree@num.category[variable] * stratified.event.tree@num.situation[level]),
                  stratified.event.tree@num.category[variable]) * (stratified.event.tree@num.slice - 1)
  }
  return(result)
}


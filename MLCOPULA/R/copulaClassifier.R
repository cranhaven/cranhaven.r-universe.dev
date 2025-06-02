#' @title Train a classification model using the Grid copula.
#' @param X Matrix with predictor variables.
#' @param y Numerical vector with the classes 
#' to predict, y = {0,1,...,nclass}.
#' @param distribution Distribution to be used: normal or kernels, 
#' by default normal.  
#' @param cop Character with the name of the copula to be used:
#'  "frank", "normal", "clayton", "joe", "gumbel", "amh" or "grid", by default frank.
#' @param weights Character with the weight construction method:
#'  "likelihood" or "mutual_information", by default likelihood.
#' @param k positive integer indicating the 
#' number of subintervals for the U2 variable. (Only for grid copula).
#' @param m positive integer indicating the number
#'  of subintervals for the U1 variable. (Only for grid copula).
#' @param method_grid Method that uses, least squares "ls" or
#'  maximum likelihood "ml", by default "ml". (Only for grid copula).
#' @description It trains a classification model based on copulas, 
#' the joint density of copulas is built with the minimum
#'  expansion tree with weights equal to the negative of: 
#'  the log likelihood or the mutual information 
#'  of bivariate copulas.
#' @return Returns a list with the trained model.
#' @examples 
#' X <- iris[,1:4]
#' Y <- iris$Species
#' y_cod <- ifelse(Y == "setosa",0,ifelse(Y == "versicolor",1,2))
#' model <- classifier.cop(X = X, y = y_cod, cop = "frank", distribution = "kernel")
#' y_pred <- predict_cop(X = X, model = model)
#' table(y_cod,y_pred)
#' @export

copulaClassifier = function(X, y, distribution = 'kernel', copula = 'frank',
                          weights = "likelihood", graph_model = "tree",
                          k = 7, m = 7, method_grid = "ml"){
  cop <- copula
  X <- as.data.frame(X)
  y_cod <- as.factor(y)
  labels <- levels(y_cod)
  y <- as.numeric(y_cod) - 1
  if(length(cop) > 1){
    cop2 <- cop
    cop <- "all"
  }
  train <- switch(cop,
                  'frank' = train.frank(X = X, y = y, 
                                        distribution = distribution,
                                        weights = weights,graph_model = graph_model),
                  'gaussian' = train.normal(X = X, y = y, 
                                          distribution = distribution,
                                          weights = weights, graph_model = graph_model),
                  'clayton' = train.clayton(X = X, y = y, 
                                            distribution = distribution,
                                            weights = weights, graph_model = graph_model),
                  'joe' = train.joe(X = X, y = y, 
                                            distribution = distribution,
                                            weights = weights, graph_model = graph_model),
                  'gumbel' = train.gumbel(X = X, y = y, 
                                            distribution = distribution,
                                            weights = weights, graph_model = graph_model),
                  'amh' = train.amh(X = X, y = y, 
                                            distribution = distribution,
                                            weights = weights, graph_model = graph_model),
                  'grid' = train.grid(X = X, y = y, 
                                      distribution = distribution,
                                      method = method_grid, k = k,
                                      m = m, weights = weights, graph_model = graph_model),
                   'all' = train.mixture(X = X, y = y, distribution = distribution,
                       weights = weights, cop = cop2, graph_model = graph_model),
                  'independent' = train.ind(X = X, y = y, distribution = distribution)
  )
  train$levels <- labels
  return(train)
}

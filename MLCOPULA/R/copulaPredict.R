#' @title Get predictions from a classification model.
#' @param X Matrix with predictor variables.
#' @param model Classification model.
#' @description Use the models trained with copula functions
#'  to generate new predictions.
#' @return Returns a vector with the predicted class.
#' @examples 
#' X <- iris[,1:4]
#' Y <- iris$Species
#' y_cod <- ifelse(Y == "setosa",0,ifelse(Y == "versicolor",1,2))
#' model <- classifier.cop(X = X, y = y_cod, cop = "frank", distribution = "kernel")
#' y_pred <- predict_cop(X = X, model = model)
#' table(y_cod,y_pred)


copulaPredict <- function(X,model){
  X <- as.data.frame(X)
  nclass <- model[[1]]$nclass
  prob <- matrix(nrow = dim(X), ncol = nclass)
  colnames(prob) <- 0:(nclass -1 )
  
  if(is.list(model[[1]]$cop)){
    cop = "all"
  }else{
    cop = model[[1]]$cop
  }
  
  for (i in 1:nclass) {
    den.marg <- switch (model[[i]]$distribution,
                        'normal' =   join.normal(X,
                                                 mu = model[[i]]$den$mu,
                                                 sd = model[[i]]$den$sd),
                        'kernel' = join.kernel(X, 
                                               den = model[[i]]$den$kernel)
    )
    
    join.cop <- switch (cop,
                        'frank' = join.frank(theta = model[[i]]$copula$theta,
                                             arbol = model[[i]]$arbol,
                                             U = den.marg$U),
                        'normal' = join.gaussian(theta = model[[i]]$copula$theta,
                                                 arbol = model[[i]]$arbol,
                                                 U = den.marg$U),
                        'clayton' = join.clayton(theta = model[[i]]$copula$theta,
                                                 arbol = model[[i]]$arbol,
                                                 U = den.marg$U),
                        'joe' = join.joe(theta = model[[i]]$copula$theta,
                                         arbol = model[[i]]$arbol,
                                         U = den.marg$U),
                        'gumbel' = join.gumbel(theta = model[[i]]$copula$theta,
                                               arbol = model[[i]]$arbol,
                                               U = den.marg$U),
                        'amh' = join.amh(theta = model[[i]]$copula$theta,
                                         arbol = model[[i]]$arbol,
                                         U = den.marg$U),
                        'grid' = join.grid(theta = model[[i]]$copula$theta,
                                           arbol = model[[i]]$arbol,
                                           U = den.marg$U),
                        'all' = join.all(theta = model[[i]]$copula$theta,
                                         arbol = model[[i]]$arbol,
                                         U = den.marg$U, cop = model[[i]]$copula$cop),
                        'independent' = 0
    )
    
    
    prob[,i] <- den.marg$den + join.cop
  }

  predict <- apply(prob, 1, which.max) - 1
  predict <- as.factor(predict)
  levels(predict) <- model$levels
  prob <- t(apply(prob, 1, sfmx))
  value <- list(prob = prob, class = predict)
  return(value)
}

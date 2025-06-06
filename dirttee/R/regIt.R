#' @keywords internal

regIt <- function(formula, data, delta, h, maxit, tol, residuals, pred, response, lambda, ns, p){

  n <- length(response)
  
  counter <- 1
  repeat{
    
    ww <- data$KMweights * dnorm(residuals, sd = h)
    if(sum(ww != 0) < p + 1){
      return(list(reg = NA, bw = h, score = NA, score_raw = NA, df = NA, converged = FALSE, iterations = NA))
    }
    data$w1 <- ww / sum(ww) / 2 / h^2
    pred0 <- pred
  
    reg <- gam(formula, data = data, weights = eval(parse(text ="w1")), sp = lambda)
    #reg <- gam(formula, data = data, weights = w1, sp = lambda)
    
    pred <- predict(reg)
    residuals <- response - pred
    
    diff <- max(abs(pred[delta == 1] - pred0[delta == 1]))
    if(counter >= maxit || diff < tol){
      if(diff < tol){
        converged <- TRUE
      } else {
        converged <- FALSE
      }
      break
    }
  
    counter <- counter + 1
  }

 # calculate penalty term
 penalty <- 0
 if(ns >=1){
   pnames <- names(summary(reg)$p.coeff)
   cof <- reg$coefficients
   cofnp <- cof[!(names(cof) %in% pnames)] # parameters of all splines
  
   for(i in 1:ns){
     S <- reg$smooth[[i]]$S[[1]] # penalty matrix. Careful: Is it still scaled?
     #S <- reg$smooth[[i]]$S[[1]] / (reg$smooth[[i]]$S.scale / lambda) 
     cofm <- matrix(cofnp[1:ncol(S)], ncol = 1)
     
     penalty <- penalty + (lambda[i] * t(cofm) %*% S %*% cofm)
     
     cofnp <- cofnp[-(1:ncol(S))]
     
   }
 }
 
 score <- log(sum(data$KMweights * dnorm(residuals / h))) - penalty
 score_raw <- sum(data$KMweights * dnorm(residuals / h))
 #df <- sum(influence(reg))
 
 
  list(reg = reg, bw = h, score = score, score_raw = score_raw, converged = converged, iterations = counter)
}


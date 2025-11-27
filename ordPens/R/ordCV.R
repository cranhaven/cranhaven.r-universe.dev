ordCV <- function(x, y, u = NULL, z = NULL, k=5, lambda, offset = rep(0,length(y)), 
                   model = c("linear", "logit", "poisson", "cumulative"), type=c("selection", "fusion"), ...){
  
  type <- match.arg(type)
  model <- match.arg(model)
  model <- switch(model, linear="linear", logit="logit", poisson="poisson", cumulative="cumulative")
  
  if (model != "cumulative")
  {
    control <- grpl.control()
    control@trace <- 0
  }else{ 
    control <- ordglasso_control()
  }  
  
  nh <- length(y)
  os <- sample(nh,nh)
  y10 <- cbind(y==1, coding(as.matrix(y),constant=F,splitcod=F))
  
  Train <- matrix(NA, k, length(lambda))
  Test <- matrix(NA, k, length(lambda))
  
  nk <- floor(nh/k)
  result.wk <- list()
  for (wk in 1:k) {
    
    if (wk < k) 
      inds <- os[(wk - 1) * nk + (1:nk)]
    else inds <- os[-(1:((wk - 1) * nk))]
    
    for (i in 1:length(lambda)){
      
      if(type=="fusion"){
        res <- ordFusion(x=x[-inds,], y=(y[-inds]), u=u[-inds,], z=z[-inds], offset=offset[-inds], model=model, lambda = lambda[i], ...)
      }else{
        res <- suppressWarnings(ordSelect(x=x[-inds,], y=(y[-inds]), u=u[-inds,], z=z[-inds], offset=offset[-inds], model=model, lambda = lambda[i], control=control, ...))
      }
      
      # training data
      pred <- res$fitted
      if(model=="cumulative"){
        Train[wk, i] <- mean(apply((y10[-inds,] - pred[[1]])^2, 1, sum))   
        
      }else{
        Train[wk, i] <- mean((y[-inds] - pred)^2)  
      }
      
      # test data
      pred2 <- predict(res, newx=x[inds,], type="response")
      if(model=="cumulative"){
        Test[wk, i] <- mean(apply((y10[inds,] - pred2[[1]])^2, 1, sum))  
      }else{
        Test[wk, i] <- mean((y[inds] - pred2)^2) 
      }
    }
  }
  out <- list( Train = Train, 
               Test = Test)
  return(out)
}


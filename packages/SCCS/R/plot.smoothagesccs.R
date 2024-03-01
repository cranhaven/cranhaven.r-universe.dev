plot.smoothagesccs <- function(x, type="l", ...) {
  
  fit <- x
   
  graphics::plot(fit$ageaxis, fit$age, type=type, ylab = "relative incidence", xlab = "age (days)") 
}


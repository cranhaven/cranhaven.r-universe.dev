check_negy <- function(object, trafo) {

  # Log
  if (trafo == "log" && any(object$model[, paste0(formula(object)[2])] <= 0)) {
    trafo <- "logshift"
    shift <- with_shift(y = object$model[, paste0(formula(object)[2])], 
                        shift = 0)
    
    cat(paste0(formula(object)[2]), " contains zero or negative values. Thus, 
         a shift equal to ",shift," is included such that y + shift > 0.")
  } else if (trafo == "log" && all(object$model[, paste0(formula(object)[2])] > 0)) {
    trafo <- "log"
  }
  
  # Box-Cox
  if (trafo == "boxcox" && any(object$model[, paste0(formula(object)[2])] <= 0)) {
    trafo <- "boxcoxshift"
    shift <- with_shift(y = object$model[, paste0(formula(object)[2])], 
                        shift = 0)
    cat(paste0(formula(object)[2]), " contains zero or negative values. Thus, 
         a shift equal to ",shift," is included such that y + shift > 0.")
  } else if (trafo == "boxcox" && all(object$model[, paste0(formula(object)[2])] > 0)) {
    trafo <- "boxcox"
  }
  
  # Dual 
  if (trafo == "dual" && any(object$model[, paste0(formula(object)[2])] <= 0)) {
    stop(paste0(paste0(formula(object)[2]), "contains zero or negative values. 
         However, the dual transformation is only defined for positive response 
         values."))
  }
  
  return(trafo)
}
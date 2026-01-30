#' Function that fits model with transformed dependent variable
#' 
#' @param object a model of type lm or lme
#' @param trans_mod the return of one of the transformation functions
#' @param std logical. If TRUE, the standardised transformations are used.
#' @return A lm or lme model that uses the transformed variable as dependent 
#' variable.
#' @keywords internal

get_modelt <- function(object, trans_mod, std) {
  
  
  if (inherits(object, "lm")) {
    if (std == FALSE) {
      data <- object$model 
      transformed_dependent <- paste0(as.character(formula(object$terms)[2]), "t")
      formula <- as.formula(paste(transformed_dependent, "~", as.character(formula(object$terms)[3])))
      data[, transformed_dependent] <- trans_mod$yt
      suppressWarnings(modelt <- lm(formula, data = data))
      modelt$formula <- paste(transformed_dependent, "~", as.character(formula(object$terms)[3]))

    } else if (std == TRUE) {
      data <- object$model 
      transformed_dependent <- paste0(as.character(formula(object$terms)[2]), "sdtt")
      formula <- as.formula(paste(transformed_dependent, "~", as.character(formula(object$terms)[3])))
      data[, transformed_dependent] <- trans_mod$zt
      suppressWarnings(modelt <- lm(formula, data = data))
      modelt$formula <- paste(transformed_dependent, "~", as.character(formula(object$terms)[3]))
      
      
      #model_frame <- object$model 
      #x <- model.matrix(attr(model_frame, "terms"), data = model_frame)
      #k <- ncol(x)
      #zt <- trans_mod$zt
      #suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt = zt, x[, 2:k])))
    }
    
    
  } else if (inherits(object, "lme")) {
    
    lme <- NULL
  
    if (std == FALSE) {
      formula <- formula(object)
      tdata <- object$data
      tdata[paste(formula[2])] <- trans_mod$yt
      rand_eff <- names(object$coefficients$random)
      suppressWarnings(modelt <- lme(fixed = formula, data = tdata,
                                     random = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                                     method = "REML",
                                     keep.data = TRUE,
                                     na.action = na.omit))
      

    } else if (std == TRUE) {
      formula <- formula(object)
      tdata <- object$data
      tdata[paste(formula[2])] <- trans_mod$zt
      rand_eff <- names(object$coefficients$random)
      suppressWarnings(modelt <- lme(fixed = formula, data = tdata,
                                     random = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                                     method = "REML",
                                     keep.data = FALSE,
                                     na.action = na.omit))
      
    }
    
    
  }
  
  return(modelt = modelt)
  
  }
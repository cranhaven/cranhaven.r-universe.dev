check_trafomod_lm <- function(object, trafo, std, custom_trafo) {
  
  
  
  if (!(trafo %in% c("bickeldoksum", "boxcoxshift", "boxcox", "dual", "gpower", 
                     "manly", "modulus", "logshiftopt", "sqrtshift", 
                     "yeojohnson", "custom_one", "log",  "logshift", "neglog", 
                     "reciprocal", 
                     "glog", "custom_wo"))) {
    stop(paste0(trafo, " is not a supported transformation. 
                Please provide valid variable name for trafo."))
  } 
  if (!inherits(object, "lm")) { 
    stop(paste0(object, "is of class", class(object), " but it needs to be of 
                class lm." ))
  }
  if (!is.logical(std) || length(std) != 1) {
    stop("std must be a logical value. Set std to TRUE or FALSE.")
  }
  if (!is.null(custom_trafo) && (!(length(custom_trafo) == 1 || length(custom_trafo) == 2))) {
    stop("custom_trafo needs to be a list with two elements.")
    
    }
}


check_trafomod_lme <- function(object, std, custom_trafo) {
  
  if (!inherits(object, "lme")) { 
    stop(paste0(object, "is of class", class(object), " but it needs to be of 
                class lme." ))
  }
  if (!is.logical(std) || length(std) != 1) {
    stop("std must be a logical value. Set std to TRUE or FALSE.")
  }
  if (!is.null(custom_trafo) && (length(custom_trafo) != 1 || length(custom_trafo) != 2)) {
    stop("custom_trafo needs to be a list with one or two elements depending on 
         the transformation.")
    
  }
}
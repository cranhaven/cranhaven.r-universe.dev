check_assumptions <- function(object, method, std) {
  

  if (!inherits(object, "lm")) { 
    stop(paste0(object, "is of class", class(object), " but it needs to be of 
                class lm." ))
  }
  if (!is.logical(std) || length(std) != 1) {
    stop("std must be a logical value. Set std to TRUE or FALSE.")
  }
  if (!(method %in% c("ml", "skew", "kurt", "div.ks", "div.cvm", "div.kl" 
                      #,"reml", "pskew"
  ))) {
    stop(paste0(method, " is not a supported estimation method. 
                Please provide valid variable name for method."))
  } 

}
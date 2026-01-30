check_woparam <- function(trafo, custom_trafo) {
  
  if (!(trafo %in% c("log", "logshift", "neglog", "reciprocal", "glog", "custom"))) {
    stop(paste0(trafo, " is not a supported transformation. 
                Please provide valid variable name for trafo."))
  } 
  if (!is.null(custom_trafo)) {
    
    if (!inherits(custom_trafo, "list")) {
      stop("An additional transformation needs to be added in argument 
            custom_trafo as a list with the transformation functions.")
    }
    
    N_custom <- length(custom_trafo)
    for (i in 1:N_custom) {
      if (!inherits(custom_trafo[[i]], "function")) {
        stop("The elements of the list need to be named functions. These functions 
             for custom transformations and standardized custom transformations 
             need to have one argument y and only one return y.")
      }
      else if (inherits(custom_trafo[[i]], "function") 
               && !all(names(formals(custom_trafo[[i]])) == c("y"))) {
        stop("The functions for custom transformations and standardized custom 
              transformations need to have one argument y and only 
              one return y.")
      }
    }
  }
  
  
    }
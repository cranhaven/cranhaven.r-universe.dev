check_oneparam <- function(trafo, lambda, method, 
                           lambdarange, plotit, 
                           custom_trafo) {
  
  if (!(trafo %in% c("bickeldoksum", "boxcoxshift", "boxcox", "dual", "gpower", 
                     "manly", "modulus", "logshiftopt", "sqrtshift", 
                     "yeojohnson", "custom"))) {
    stop(paste0(trafo, " is not a supported transformation. 
                Please provide valid variable name for trafo."))
  } 

  if (lambda != "estim" && !is.numeric(lambda)) {
    
    stop(paste0(lambda, " needs to be the character 'estim' or a numeric value. 
         Please provide valid value for lambda."))
  }
  if (is.numeric(lambda) && length(lambda) != 1) {
    
    stop("If lambda is a numeric value it needs to be a numeric vector of 
         length 1.")
  }
  if (is.numeric(lambda) && trafo == "bickeldoksum" && lambda <= 0) {
    stop("The Bickel-Doksum transformation is only defined for positive 
         values of lambda.")
  }
  # if (is.numeric(lambda) && (trafo == "logshiftopt" || trafo == "sqrtshift")
  #     && lambda < min(y) + 1) {
  #   stop("The Bickel-Doksum transformation is only defined for positive 
  #        values of lambda.")
  # }
  
  if (!(method %in% c("ml", "skew", "kurt", "div.ks", "div.cvm", "div.kl" 
                     #,"reml", "pskew"
                     ))) {
    stop(paste0(method, " is not a supported estimation method. 
                Please provide valid variable name for method."))
  } 
  if (length(lambdarange) != 2 || !is.vector(lambdarange, mode = "numeric") ||
        !(lambdarange[1] < lambdarange[2])) {
    stop("lambdarange needs to be a numeric vector of length 2 
         defining a lower and upper limit for the estimation of the optimal 
         transformation parameter. The value of the lower limit needs to be 
         smaller than the upper limit.")
  } 
  if (trafo == "bickeldoksum" && lambdarange[1] <= 0) {
    stop("The Bickel-Doksum transformation is only defined for positive 
         values of lambda.")
  }
  # if ((trafo == "logshiftopt" || trafo == "sqrtshift") && 
  #     lambdarange[1] < min(y) + 1) {
  #   stop("The lambda must be at least the minimum of the dependent variable
  #        plus 1 in order to shift the values of the dependent variable to 
  #        the positive scale.")
  # }
  if (!is.logical(plotit) || length(plotit) != 1) {
    stop("plotit must be a logical value. Set plotit to TRUE or FALSE.")
  }

  if (!is.null(custom_trafo)) {
    
    if (!inherits(custom_trafo, "list")) {
      stop("An additional transformation needs to be added in argument 
            custom_trafo as a list with the transformation function.")
    }
    
    N_custom <- length(custom_trafo)
    for (i in 1:N_custom) {
      if (!inherits(custom_trafo[[i]], "function")) {
        stop("The elements of the list need to be named functions. These functions 
             for custom transformations and standardized custom transformations 
             need to have two arguments y and lambda and only one return y.")
      }
      else if (inherits(custom_trafo[[i]], "function") 
               && !all(names(formals(custom_trafo[[i]])) == c("y", "lambda"))) {
        stop("The functions for custom transformations and standardized custom 
              transformations need to have two arguments y and lambda and only 
              one return y.")
      }
    }
  }
  
  
  }
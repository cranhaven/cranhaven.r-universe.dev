#' First check of assumptions to find suitable transformations
#'
#' Gives a first overview if a transformation is useful and which transformation
#' is promising to fulfill the model assumptions normality, homoscedasticity and 
#' linearity.
#' 
#' @param object an object of type \code{lm}.
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),
#' (iii) Kurtosis optimization ("kurt"), (iv) Divergence minimization by 
#' Kolmogorov-Smirnov ("div.ks"), by Cramer-von-Mises ("div.cvm") or by 
#' Kullback-Leibler ("div.kl"). Defaults to "ml".
#' @param std logical. If \code{TRUE}, the transformed model is returned based 
#' on the standardized/scaled transformation. Defaults to \code{FALSE}.
#' @param ... other parameters that can be passed to the function, e.g. other 
#' lambdaranges. Self-defined lambdaranges are given to the function as an 
#' argument that is the combination of the name of the transformation and lr and 
#' the range needs to be a numeric vector of length 2. For instance, changing the 
#' lambdarange for the Manly transformation would mean to add an argument 
#' \code{manly_lr = manly_lr = c(0.000005,0.00005)}. For the default values that 
#' are used for the lambdaranges see the documentation for the provided 
#' transformations. 
#' @return A table with tests for normality and homoscedasticity. Furthermore, 
#' scatterplots are returned to check the linearity assumption.
#' @seealso \code{\link{bickeldoksum}}, \code{\link{boxcox}}, \code{\link{dual}}, 
#' \code{\link{glog}}, \code{\link{gpower}}, \code{\link{log}}, 
#' \code{\link{logshiftopt}}, \code{\link{manly}}, \code{\link{modulus}}, 
#' \code{\link{neglog}}, \code{\link{sqrtshift}}, \code{\link{yeojohnson}} 
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' assumptions(lm_cars)
#' assumptions(lm_cars, method = "skew", manly_lr = c(0.000005,0.00005))
#' @importFrom moments skewness kurtosis
#' @importFrom lmtest bptest
#' @export

assumptions <- function(object, method = "ml", std = FALSE, ...){
  

  check_assumptions(object = object, method = method, std = std)

  lambdaranges <- list(...)

  trafos <- list()
  
  if (any(object$model[, paste0(formula(object)[2])] <= 0)) {
    
    # Box-Cox shift
    if ("boxcox_lr" %in% names(lambdaranges)) {
      try(trafos[["boxcoxshift"]] <- boxcoxshift(object = object, method = method,
                                                 plotit = FALSE, 
                                                 lambdarange = lambdaranges[["boxcox_lr"]]), 
          silent = TRUE)
    } else if (!("boxcox_lr" %in% names(lambdaranges))) {
      try(trafos[["boxcoxshift"]] <- boxcoxshift(object = object, method = method,
                                                 plotit = FALSE), 
          silent = TRUE)
    }
    if (is.null(trafos[["boxcoxshift"]])) {
      warning("The default lambarange is not suitable for the shifted Box-Cox 
              transformation and this data. If the Box-Cox transformation is 
            supposed to be considered in the fast check, please add an argument 
               boxcox_lr which is a vector of two elements that define the lower and 
               the upper bound for the lambdarange, e.g. boxcox_lr = c(-2,2).")
    } else {
      trafos[["boxcoxshift"]] <- trafos[["boxcoxshift"]]
    }
    
    
    # Log shift
    trafos[["logshift"]] <- logshift(object = object)
    
    # Calculate shift
    shift <- with_shift(y = object$model[, paste0(formula(object)[2])], 
                        shift = 0)
    cat(paste0(formula(object)[2]), " contains zero or negative values. Thus, 
        a shift equal to ",shift," is included to the Log and the Box-Cox 
        transformation such that y + shift > 0. The Dual transformation is 
        only suitable for positive response values.")
   } else if (all(object$model[, paste0(formula(object)[2])] > 0)) {
     
     # Box-Cox
     if ("boxcox_lr" %in% names(lambdaranges)) {
       try(trafos[["boxcox"]] <- boxcox(object = object, method = method,
                                        plotit = FALSE, 
                                        lambdarange = lambdaranges[["boxcox_lr"]]), 
           silent = TRUE)
     } else if (!("boxcox_lr" %in% names(lambdaranges))) {
       try(trafos[["boxcox"]] <- boxcox(object = object, method = method,
                                        plotit = FALSE), 
           silent = TRUE)
     }
     if (is.null(trafos[["boxcox"]])) {
       warning("The lambarange is not suitable for the Box-Cox 
              transformation and this data. If the Box-Cox transformation is 
            supposed to be considered in the fast check, please add an argument 
               boxcox_lr which is a vector of two elements that define the lower and 
               the upper bound for the lambdarange, e.g. boxcox_lr = c(-2,2).")
     } else {
       trafos[["boxcox"]] <- trafos[["boxcox"]]
     }
     
     # Dual 
     if ("dual_lr" %in% names(lambdaranges)) {
       try(trafos[["dual"]] <- dual(object = object, method = method,
                                    plotit = FALSE, 
                                    lambdarange = lambdaranges[["dual_lr"]]), 
           silent = TRUE)
     } else if (!("dual_lr" %in% names(lambdaranges))) {
       try(trafos[["dual"]] <- dual(object = object, method = method,
                                    plotit = FALSE), silent = TRUE)
     }
     if (is.null(trafos[["dual"]])) {
       warning("The lambarange is not suitable for the Dual 
              transformation and this data. If the Dual transformation is 
            supposed to be considered in the fast check, please add an argument 
               dual_lr which is a vector of two elements that define the lower and 
               the upper bound for the lambdarange, e.g. dual_lr = c(0,2).")
     } else {
       trafos[["dual"]] <- trafos[["dual"]]
     }
     
     trafos[["log"]] <- logtrafo(object = object)
   }
  

  
  # Bickel-Doksum
  if ("bickeldoksum_lr" %in% names(lambdaranges)) {
    try(trafos[["bickeldoksum"]] <- bickeldoksum(object = object, method = method,
                                                 plotit = FALSE, 
                                                 lambdarange = lambdaranges[["bickeldoksum_lr"]]), 
        silent = TRUE)
  } else if (!("bickeldoksum_lr" %in% names(lambdaranges))) {
    try(trafos[["bickeldoksum"]] <- bickeldoksum(object = object, method = method,
                                                 plotit = FALSE), silent = TRUE)
  }
  if (is.null(trafos[["bickeldoksum"]])) {
    warning("The lambarange is not suitable for the Bickel-Doksum 
            transformation and this data. If the Bickel-Doksum transformation is 
            supposed to be considered in the fast check, please add an argument 
            bickeldoksum_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. bickeldoksum_lr = c(-1e-11,2).")
  } else {
    trafos[["bickeldoksum"]] <- trafos[["bickeldoksum"]]
  }
  
  # Gpower
  if ("gpower_lr" %in% names(lambdaranges)) {
    try(trafos[["gpower"]] <- gpower(object = object, method = method,
                                     plotit = FALSE, 
                                     lambdarange = lambdaranges[["gpower_lr"]]), 
        silent = TRUE)
    
  } else if (!("gpower_lr" %in% names(lambdaranges))) {
    try(trafos[["gpower"]] <- gpower(object = object, method = method,
                                     plotit = FALSE), silent = TRUE)
  }
  if (is.null(trafos[["gpower"]])) {
    warning("The lambarange is not suitable for the Gpower 
            transformation and this data. If the Gpower transformation is 
            supposed to be considered in the fast check, please add an argument 
            gpower_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. gpower_lr = c(-2,2).")
  } else {
    trafos[["gpower"]] <- trafos[["gpower"]]
  }
  
  # Manly 
  if ("manly_lr" %in% names(lambdaranges)) {
    try(trafos[["manly"]] <- manly(object = object, method = method,
                                   plotit = FALSE, 
                                   lambdarange = lambdaranges[["manly_lr"]]), 
        silent = TRUE)
    
  } else if (!("manly_lr" %in% names(lambdaranges))) {
    try(trafos[["manly"]] <- manly(object = object, method = method,
                                   plotit = FALSE), silent = TRUE)
  }
  if (is.null(trafos[["manly"]])) {
    warning("The lambarange is not suitable for the Manly transformation
            and this data. If the Manly transformation is 
            supposed to be considered in the fast check, please add an argument 
            manly_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. manly_lr = c(-2,2).")
  } else {
    trafos[["manly"]] <- trafos[["manly"]]
  }
  
  # Modulus
  if ("modulus_lr" %in% names(lambdaranges)) {
    try(trafos[["modulus"]] <- modulus(object = object, method = method,
                                       plotit = FALSE, 
                                       lambdarange = lambdaranges[["modulus_lr"]]), 
        silent = TRUE)
    
  } else if (!("modulus_lr" %in% names(lambdaranges))) {
    try(trafos[["modulus"]] <- modulus(object = object, method = method,
                                       plotit = FALSE), silent = TRUE)
  }
  if (is.null(trafos[["modulus"]])) {
    warning("The lambarange is not suitable for the Modulus 
            transformation and this data. If the Modulus transformation is 
            supposed to be considered in the fast check, please add an argument 
            modulus_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. modulus_lr = c(-2,2).")
  } else {
    trafos[["modulus"]] <- trafos[["modulus"]]
  }
  
  # Log shift opt
  if ("logshiftopt_lr" %in% names(lambdaranges)) {
    try(trafos[["logshiftopt"]] <- logshiftopt(object = object, method = method,
                                               plotit = FALSE, 
                                               lambdarange = lambdaranges[["logshiftopt_lr"]]), 
        silent = TRUE)
  } else if (!("logshiftopt_lr" %in% names(lambdaranges))) {
    try(trafos[["logshiftopt"]] <- suppressWarnings(logshiftopt(object = object, method = method,
                                               plotit = FALSE)), silent = TRUE)
  }
  if (is.null(trafos[["logshiftopt"]])) {
    warning("The lambarange is not suitable for the Log shift opt 
            transformation and this data. If the Log shift opt transformation is 
            supposed to be considered in the fast check, please add an argument 
            logshiftopt_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. logshiftopt_lr = c(-10,10).")
  } else {
    trafos[["logshiftopt"]] <- trafos[["logshiftopt"]]
  }
  
  
  # Square-root shift
  if ("sqrtshift_lr" %in% names(lambdaranges)) {
    try(trafos[["sqrtshift"]] <- sqrtshift(object = object, method = method,
                                           plotit = FALSE, 
                                           lambdarange = lambdaranges[["sqrtshift_lr"]]), 
        silent = TRUE)
    
  } else if (!("sqrtshift_lr" %in% names(lambdaranges))) {
    try(trafos[["sqrtshift"]] <- sqrtshift(object = object, method = method,
                                           plotit = FALSE), silent = TRUE)
  }
  if (is.null(trafos[["sqrtshift"]])) {
    warning("The lambarange is not suitable for the Square-root shift 
            transformation and this data. If the Square-root shift transformation is 
            supposed to be considered in the fast check, please add an argument 
            sqrtshift_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. sqrtshift_lr = c(-10,10).")
  } else {
    trafos[["sqrtshift"]] <- trafos[["sqrtshift"]]
  }
  
  
  # Yeo-Johnson
  if ("yeojohnson_lr" %in% names(lambdaranges)) {
    try(trafos[["yeojohnson"]] <- yeojohnson(object = object, method = method,
                                             plotit = FALSE, 
                                             lambdarange = lambdaranges[["yeojohnson_lr"]]), 
        silent = TRUE)
    
  } else if (!("yeojohnson_lr" %in% names(lambdaranges))) {
    try(trafos[["yeojohnson"]] <- yeojohnson(object = object, method = method,
                                             plotit = FALSE), silent = TRUE)
  }
  if (is.null(trafos[["yeojohnson"]])) {
    warning("The lambarange is not suitable for the Yeo-Johnson 
            transformation and this data. If the Yeo-Johnson transformation is 
            supposed to be considered in the fast check, please add an argument 
            yeojohnson_lr which is a vector of two elements that define the lower and 
            the upper bound for the lambdarange, e.g. yeojohnson_lr = c(-2,2).")
  } else {
    trafos[["yeojohnson"]] <- trafos[["yeojohnson"]]
  }

  trafos[["log"]] <- logtrafo(object = object)
  trafos[["reciprocal"]] <- reciprocal(object = object)
  trafos[["neglog"]] <- neglog(object = object)
  trafos[["glog"]] <- glog(object = object)
  
  # Get residuals
  resid_orig <- residuals(object, level = 0, type = "pearson")
  
  trafo_mod <- list()
  resid <- list()
  shapiroEst <- list()
  shapiroP <- list()
  skewness_trafo <- list()
  kurtosis_trafo <- list()
  breusch_pagan <- list()
  scatter_trafos <- list()
  
  for (transform in names(trafos)) {
    trafo_mod[[transform]] <- get_modelt(object = object, 
                                         trans_mod = trafos[[transform]], 
                                         std = std)
    resid[[transform]] <- residuals(trafo_mod[[transform]], level = 0, 
                                    type = "pearson")
    
    if (length(resid[[transform]]) > 3 & length(resid[[transform]]) < 5000) {
      
      # Original model or first transformed
      shapiroEst_orig <- shapiro.test(resid_orig)$statistic[[1]]
      shapiroP_orig <- shapiro.test(resid_orig)$p.value[[1]]
      
      
      shapiroEst[[transform]] <- shapiro.test(resid[[transform]])$statistic[[1]]
      shapiroP[[transform]] <- shapiro.test(resid[[transform]])$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_orig <- NA
      shapiroP_orig <- NA
      
      shapiroEst[[transform]] <- NA
      shapiroP[[transform]] <- NA
    }
    
    skewness_orig <- skewness(resid_orig)
    kurtosis_orig <- kurtosis(resid_orig)
    
    skewness_trafo[[transform]] <- skewness(resid[[transform]])
    kurtosis_trafo[[transform]] <- kurtosis(resid[[transform]])
    
    breusch_pagan_orig <- bptest(formula(object$terms), 
                                 data = object$model)
    breusch_pagan[[transform]] <- bptest(formula(trafo_mod[[transform]]$terms), 
                                         data = trafo_mod[[transform]]$model)
    
  }
  
  norm_resid <- data.frame(Skewness  = round(skewness_orig, 4),
                           Kurtosis  = round(kurtosis_orig, 4),
                           Shapiro_W = round(shapiroEst_orig,4),
                           Shapiro_p = round(shapiroP_orig,4))
  
  hetero <- data.frame(BreuschPagan_V = round(breusch_pagan_orig$statistic, 4),
                       BreuschPagan_p = round(breusch_pagan_orig$p.value, 4))
  
  for (transform in names(trafos)) {
    
    norm_resid <- rbind(norm_resid, 
                        data.frame(Skewness  = round(skewness_trafo[[transform]], 4),
                                   Kurtosis  = round(kurtosis_trafo[[transform]], 4),
                                   Shapiro_W = round(shapiroEst[[transform]],4),
                                   Shapiro_p = round(shapiroP[[transform]], 4)))
    
    hetero <- rbind(hetero, 
                    data.frame(BreuschPagan_V = round(breusch_pagan[[transform]]$statistic, 4),
                               BreuschPagan_p = round(breusch_pagan[[transform]]$p.value, 4)))
    
  }
  rownames(norm_resid) <- c("untransformed", names(trafos))
  rownames(hetero) <- c("untransformed", names(trafos))
  
  norm_resid <- norm_resid[order(norm_resid$Shapiro_p, decreasing = TRUE),]
  hetero <- hetero[order(hetero$BreuschPagan_p, decreasing = TRUE),]
  
  cat("Test normality assumption \n")
  print(norm_resid)
  cat("\n")
  cat("Test homoscedasticity assumption \n")
  print(hetero)
  cat("\n")
  cat("Test linearity assumption \n")
  cat("Press [enter] to continue")
  line <- readline()
  pairs(formula(object$terms), data = object$model, main = "Untransformed model", 
        lower.panel = panel.smooth, upper.panel = panel.cor)
  for (transform in names(trafos)) {
    cat("Press [enter] to continue")
    line <- readline()
    pairs(formula(trafo_mod[[transform]]$terms), 
          data = trafo_mod[[transform]]$model,
          main = transform, 
          lower.panel = panel.smooth, upper.panel = panel.cor) 
  }
 
}

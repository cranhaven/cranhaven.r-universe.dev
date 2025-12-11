# @importFrom utils lsf.str
# @importFrom measures multiclass.Brier listAllMeasures

univarImp = function(object, vname, mincriterion = 0, conditional = FALSE, threshold = 0.2, 
                  nperm = 1, OOB = TRUE, pre1.0_0 = conditional, measure = "multiclass.Brier", ...) { 

  # Some tests
  measureList = listAllMeasures()
  if (!(measure %in% measureList[, 1]))
    stop("measure should be a measure of the measures package")
  
  # Test the Class
  response = object@responses
  CLASS = all(response@is_nominal | response@is_ordinal)
  PROB = measureList$probabilities[measureList[,1] == measure]
  MEASURECLASS = measureList$task[measureList[,1] == measure]
  if (CLASS & (MEASURECLASS %in% c("regression", "multilabel")))
    stop("Measure is not suitable for classification")
  if (!CLASS & !(MEASURECLASS %in% "regression"))
    stop("Measure is not suitable for regression")
  MEASUREMINIMIZE = measureList$minimize[measureList[,1] == measure]
  
  input = object@data@get("input")
  xnames = colnames(input)
  idv <- which(xnames==vname)
  inp = initVariableFrame(input, trafo = NULL)
  y = object@responses@variables[[1]]
  if (length(response@variables) != 1) 
    stop("cannot compute variable importance measure for multivariate response")
  if (conditional || pre1.0_0) {
    if (!all(complete.cases(inp@variables)))
      stop("cannot compute variable importance measure with missing values")
  }
  
  if (CLASS) {
    if (PROB) {
      error = function(x, oob, ...) {
        xoob = t(sapply(x, function(x) x))[oob,]
        colnames(xoob) = levels(y)
        yoob = y[oob]
        return(do.call(measure, list(xoob, yoob, ...)))
      } 
    }else {
      error = function(x, oob, ...) {
        xoob = t(sapply(x, function(x) x))[oob,]
        colnames(xoob) = levels(y)
        xoob = colnames(xoob)[max.col(xoob,ties.method="first")]
        yoob = y[oob]
        return(do.call(measure, list(yoob, xoob, ...)))
      } 
    }
  } else {
    error = function(x, oob, ...) {
      xoob = unlist(x)[oob]
      yoob = y[oob]
      return(do.call(measure, list(xoob, yoob, ...)))
    }
  }
  
  w = object@initweights
  if (max(abs(w - 1)) > sqrt(.Machine$double.eps)) 
    warning(sQuote("varImp"), " with non-unity weights might give misleading results")
  perror = matrix(0, nrow = nperm * length(object@ensemble), ncol = length(xnames))
  colnames(perror) = xnames
  for (b in 1:length(object@ensemble)) {
    tree <- object@ensemble[[b]]
    if (OOB) {
      oob = object@weights[[b]] == 0
    } else {
      oob = rep(TRUE, length(xnames))
    }
    p = party_intern(tree, inp, mincriterion, -1L, fun = "R_predict") 
    eoob = error(p, oob, ...)
    for (j in unique(varIDs(tree))) {
      if(j==idv) {
          for (per in 1:nperm) {
            if (conditional || pre1.0_0) {
              tmp = inp
              ccl = create_cond_list(conditional, threshold, 
                                     xnames[j], input)
              if (is.null(ccl)) {
                perm = sample(which(oob))
              }
              else {
                perm = conditional_perm(ccl, xnames, input, 
                                        tree, oob)
              }
              tmp@variables[[j]][which(oob)] = tmp@variables[[j]][perm]
              p = party_intern(tree, tmp, mincriterion, -1L, fun = "R_predict") 
            } else {
              p = party_intern(tree, inp, mincriterion, as.integer(j), fun = "R_predict") 
            }
            minSign = ifelse(MEASUREMINIMIZE, 1, -1)
            perror[(per + (b - 1) * nperm), j] = minSign * (error(p,oob, ...) - eoob)
          }
      }
    }
  }
  perror = as.data.frame(perror)
  MeanDecrease = colMeans(perror, na.rm = TRUE)
  MeanDecrease = MeanDecrease[names(MeanDecrease)==vname]
  return(MeanDecrease)
}

#' @import party varImp

#' @importFrom stats complete.cases

#' @export

fastvarImpAUC = function (object, mincriterion = 0, conditional = FALSE, threshold = 0.2,
                          nperm = 1, OOB = TRUE, pre1.0_0 = conditional,
                          parallel = TRUE) {
  # vgl. Janitza
  response = object@responses
  input = object@data@get("input")
  xnames = colnames(input)
  inp = initVariableFrame(input, trafo = NULL)
  y = object@responses@variables[[1]]
  if (length(response@variables) != 1)
    stop("cannot compute variable importance measure for multivariate response")
  if (conditional || pre1.0_0) {
    if (!all(complete.cases(inp@variables)))
      stop("cannot compute variable importance measure with missing values")
  }
  CLASS = all(response@is_nominal)
  ORDERED = all(response@is_ordinal)
  if (!CLASS & !ORDERED)
    stop("only calculable for classification")
  if (CLASS) {
    if (nlevels(y) > 2) {
      stop("varImpAUC() is only usable for binary classification. For multiclass classification please use the standard varImp() function.")
    } else {
      error <- function(x, oob) {
        xoob <- sapply(x, function(x) x[1])[oob]
        yoob <- y[oob]
        which1 <- which(yoob==levels(y)[1])
        noob1 <- length(which1)
        noob <- length(yoob)
        if (noob1==0|noob1==noob) { return(NA) }       # AUC cannot be computed if all OOB-observations are from one class
        return(1-sum(kronecker(xoob[which1] , xoob[-which1],">"))/(noob1*(length(yoob)-noob1)))       # calculate AUC
      }
    }
  } else {
    if (ORDERED) {
      error = function(x, oob) mean((sapply(x, which.max) != y)[oob])
    }
    else {
      error = function(x, oob) mean((unlist(x) - y)[oob]^2)
    }
  }
  w = object@initweights
  if (max(abs(w - 1)) > sqrt(.Machine$double.eps))
    warning(sQuote("varimp"), " with non-unity weights might give misleading results")
  foo <- function(b) {
    perror = matrix(0, nrow = nperm * length(object@ensemble), ncol = length(xnames))
    colnames(perror) = xnames
    tree = object@ensemble[[b]]
    if (OOB) {
      oob = object@weights[[b]] == 0
    } else {
      oob = rep(TRUE, length(xnames))
    }
    p = party_intern(tree, inp, mincriterion, -1L, fun = "R_predict")
    eoob = error(p, oob)
    for (j in unique(varIDs(tree))) {
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
        perror[(per + (b - 1) * nperm), j] = - (error(p,oob) - eoob)
      }
    }
    return(-perror)
  }
  #for (b in 1:length(object@ensemble))
  #liste_perror <- list()
  liste_perror <- plyr::alply(1:length(object@ensemble), 1, .fun=foo, .parallel=parallel, .paropts=list(.packages="party"))
  all_perror <- Reduce('+',liste_perror)
  all_perror = as.data.frame(all_perror)
  return(MeanDecrease = colMeans(all_perror, na.rm = TRUE))
}

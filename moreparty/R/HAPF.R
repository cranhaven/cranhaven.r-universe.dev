# @importFrom MASS fitdistr

# @import MASS

#' @importFrom stats pnorm

HAPF <- function(Y, X, nperm = 30, ntree = 50, distrib = 'approx', alpha = 0.05, measure=NULL, parallel=FALSE, ...) {

  # RETURNS: selected variables, a corresponding forest and the OOB-error
  #          with and without Bonferroni-Adjustment
  
  if(is.factor(Y)) mtry <- ceiling(sqrt(ncol(X))) # automatically set mtry to sqrt(p)
  if(class(Y) %in% c('numeric','integer')) mtry <- ceiling(ncol(X)/3) # automatically set mtry to p/3
  
  if(is.factor(Y) & is.null(measure)) measure = 'AUC'
  if(class(Y) %in% c('numeric','integer') & is.null(measure)) measure = 'RMSE'
  
  measureList = measures::listAllMeasures()
  if(!(measure %in% measureList[,1])) stop("measure should be a measure of the measures package")
  CLASS = is.factor(Y)
  PROB = measureList$probabilities[measureList[,1] == measure]
  MEASURECLASS = measureList$task[measureList[,1] == measure]
  if(CLASS & (MEASURECLASS %in% c("regression", "multilabel"))) stop("Measure is not suitable for classification")
  if(!CLASS & !(MEASURECLASS %in% "regression")) stop("Measure is not suitable for regression")

  MEASUREMINIMIZE = measureList$minimize[measureList[,1] == measure]
  minSign = ifelse(MEASUREMINIMIZE, 1, -1)

  if (CLASS) {
    if (PROB) {
      error = function(cf, ...) {
        pred <- do.call('rbind',predict(cf,OOB=TRUE,type='prob'))[,2]
        return(do.call(measure, list(pred, Y, ...)))
      } 
    }else {
      error = function(cf, ...) {
        pred <- predict(cf,OOB=TRUE)
        return(do.call(measure, list(Y, pred, ...)))
      } 
    }
  } else {
    error = function(cf, ...) {
      pred <- as.numeric(predict(cf,OOB=TRUE))
      return(do.call(measure, list(pred, Y, ...)))
    }
  }
  
  dat <- cbind(Y, X) # create the data
  names(dat)[1] <- 'response'
  
  if(measure=="AUC") {
    vi_fct <- function(x,v) { univarImpAUC(x,vname=v) }
  } else {
    vi_fct <- function(x,v) { univarImp(x, vname=v, measure=measure, ...) }
  }
  
  forest <- fastcforest(response ~ ., data = dat, # fit a forest
                        controls = cforest_unbiased(mtry = mtry, ntree = ntree),
                        parallel = parallel)
  if(measure=='AUC') {
    obs.varimp <- fastvarImpAUC(forest,parallel=parallel)
  } else {
    obs.varimp <- fastvarImp(forest,measure=measure,parallel=parallel, ...) # compute initial variable importances
  }
  selection <- names(obs.varimp)
  
  # create a matrix that contains the variable importances after permutation
  # perm.mat <- matrix(NA, ncol = length(selection), nrow = nperm, 
  #                    dimnames = list(1:nperm, selection))
  
  # for (i in 1:nperm) { # do nperm permutations
  foo <- function(x) {
    res <- numeric()
    for (j in 1:length(selection)) { # repeat the computational steps for each variable
      perm.dat <- dat # perm.dat will be the data after permutation
      perm.dat[, j+1] <- sample(perm.dat[, j+1]) # permute each variable
      perm.forest <- party::cforest(response ~ ., data = perm.dat, # recompute the forests
                                    controls = party::cforest_unbiased(mtry = mtry, ntree = ntree))
      res[j] <- vi_fct(perm.forest,selection[j])
      #univarImp(perm.forest, selection[j], measure=measure)
      }
    return(res) 
    } # recompute the importances
  
  liste.res <- plyr::alply(1:nperm, 1, .fun=foo, .parallel=parallel, .paropts=list(.packages="party"))
  perm.mat <- do.call('rbind',liste.res)
  colnames(perm.mat) <- selection
  
  if(distrib=='approx') {
    p.vals <- sapply(selection, function(x) sum(perm.mat[, x]>=obs.varimp[x]) / nperm) # compute p-values
  } else {
    p.vals <- sapply(selection, function(x) {fit <- MASS::fitdistr(perm.mat[, x], "normal")$estimate; round(1-stats::pnorm(obs.varimp[x],fit[1],fit[2]),5) })
    names(p.vals) <- names(obs.varimp)
  }
  
  p.vals.bonf <- p.vals * length(p.vals) # p-values with Bonferroni-Adjustment
  
  if (any(p.vals < alpha)) { # keep significant variables
    selection <- names(p.vals)[which(p.vals < alpha)]
    exclusion <- base::setdiff(names(X),selection)
    if(is.factor(Y)) mtry <- ceiling(sqrt(length(selection))) # automatically set mtry to sqrt(p)
    if(class(Y) %in% c('numeric','integer')) mtry <- ceiling(length(selection)/3) # automatically set mtry to p/3
    forest <- fastcforest(as.formula(paste("response", paste(selection,collapse="+"),sep="~")), data=dat, 
                          controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                          parallel=parallel)
  }
  
  if (any(p.vals.bonf < alpha)) { # keep significant variables (Bonferroni)
    selection.bonf <- names(p.vals.bonf)[which(p.vals.bonf<alpha)]
    exclusion.bonf <- base::setdiff(names(X),selection.bonf)
    if(is.factor(Y)) mtry <- ceiling(sqrt(length(selection.bonf))) # automatically set mtry to sqrt(p)
    if(class(Y) %in% c('numeric','integer')) mtry <- ceiling(length(selection.bonf)/3) # automatically set mtry to p/3
    forest.bonf <- fastcforest(as.formula(paste("response", paste(selection.bonf,collapse = " + "), sep = " ~ ")), data = dat, 
                               controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                               parallel=parallel)
  }
  
  if (!any(p.vals < alpha)) { # if there are not significant variables
    selection <- c(); exclusion <- names(X); forest <- c()
  }
  
  if (!any(p.vals.bonf < alpha)) { # if there are not significant variables
    selection.bonf <- c(); exclusion.bonf <- names(X); forest.bonf <- c()
  }
  
  oob.error <- error(forest, ...)
  oob.error.bonf <- error(forest.bonf, ...)

  return(list("p.vals" = p.vals, "selection" = selection, "exclusion" = exclusion, "forest" = forest, "oob.perf" = oob.error,
              "p.vals.bonf" = p.vals.bonf, "selection.bonf" = selection.bonf, "exclusion.bonf" = exclusion.bonf, "forest.bonf" = forest.bonf, "oob.perf.bonf" = oob.error.bonf))
}

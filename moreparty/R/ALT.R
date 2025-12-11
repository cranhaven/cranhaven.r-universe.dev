# @importFrom MASS fitdistr

# @import MASS

#' @importFrom stats pnorm

ALT <- function(Y, X, nperm = 10, ntree = 50, distrib = 'approx', alpha = 0.05, measure=NULL, parallel=FALSE, ...) { 

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
    vi_fct <- function(x, .parallel) { fastvarImpAUC(x, parallel=.parallel) }
  } else {
    vi_fct <- function(x, .parallel) { fastvarImp(x, parallel=.parallel, measure=measure, ...) }
  }
    
  forest <- fastcforest(response ~ ., data = dat, # fit a forest
                        controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                        parallel = parallel)
  obs.varimp <- vi_fct(forest, parallel) # compute initial variable importances
  selection <- names(obs.varimp)

  foo <- function(x) {
    perm.dat <- dat # perm.dat will be the data after permutation
    perm.dat[, "response"] <- sample(perm.dat[, "response"]) # permute the response
    perm.forest <- party::cforest(response ~ ., data = perm.dat, # recompute the forests
                                 controls = party::cforest_unbiased(mtry = mtry, ntree = ntree))
    res <- vi_fct(perm.forest, FALSE) # recompute the variable importances
    return(res)
  }
  liste.res <- plyr::alply(1:nperm, 1, .fun=foo, .parallel=parallel, .paropts=list(.packages="party"))
  perm.mat <- do.call('rbind',liste.res)
    
  if(distrib=='approx') {
    p.vals <- sapply(selection, function(x) sum(perm.mat[, x]>=obs.varimp[x]) / nperm) # compute p-values
  } else {
    p.vals <- sapply(selection, function(x) {fit <- MASS::fitdistr(perm.mat[, x], "normal")$estimate; round(1-stats::pnorm(obs.varimp[x],fit[1],fit[2]),5) })
    names(p.vals) <- names(obs.varimp)
  }
  
  if (any(p.vals < alpha)) { # keep significant variables
    selection <- names(p.vals)[which(p.vals<alpha)]
    exclusion <- base::setdiff(names(X),selection)
    if(is.factor(Y)) mtry <- ceiling(sqrt(length(selection))) # set mtry to sqrt() of remaining variables
    if(class(Y) %in% c("numeric","integer")) mtry <- ceiling(length(selection)/3) # set mtry to ()/3 of remaining variables
    forest <- fastcforest(as.formula(paste("response", paste(selection, 
                                                         collapse = " + "), sep = " ~ ")), data = dat, 
                          controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                          parallel = parallel)
    }
  if (!any(p.vals < alpha)) { # if there are not significant variables
    selection <- c()
    exclusion <- names(X)
    forest <- c()
  }
  
  oob.error <- error(forest, ...)

  return(list("p.vals" = p.vals, "selection" = selection, "exclusion" = exclusion, "forest" = forest, "oob.perf" = oob.error))
}

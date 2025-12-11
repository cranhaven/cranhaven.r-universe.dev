# @import party
#
#' @importFrom stats predict sd
#'
# @export


RFE <- function(Y, X, ntree = 3000, measure = NULL, parallel = FALSE, ...) {
  
  CLASS = is.factor(Y)
  
  if(CLASS) mtry <- ceiling(sqrt(ncol(X))) # automatically set mtry to sqrt(p)
  if(!CLASS) mtry <- ceiling(ncol(X)/3) # automatically set mtry to p/3
  
  if(CLASS & is.null(measure)) measure = 'AUC'
  if(!CLASS & is.null(measure)) measure = 'RMSE'
  
  measureList = measures::listAllMeasures()
  if(!(measure %in% measureList[,1])) stop("measure should be a measure of the measures package")
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
  names(dat) <- c("response", paste("V", 1:ncol(X), sep = ""))
  
  if(measure=="AUC") {
    vi_fct <- function(x) { fastvarImpAUC(x, parallel=parallel) }
  } else {
    vi_fct <- function(x) { fastvarImp(x, measure=measure, parallel=parallel, ...) }
  }
  
  print('initial step')
  forest <- fastcforest(response ~ ., data = dat, # fit a forest
                        controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                        parallel = parallel)
  selections <- list() # a list that contains the sequence of selected variables
  selections[[ncol(X)]] <- names(sort(vi_fct(forest), decreasing = T))
  errors <- c()
  
  for (i in ncol(X):1) { # take backward rejection steps
    print(paste('rejection steps remaining =',i))
    if(is.factor(Y)) mtry <- ceiling(sqrt(i)) # set mtry to sqrt() of remaining variables
    if(class(Y) %in% c('numeric','integer')) mtry <- ceiling(i/3) # set mtry to ()/3 of remaining variables
    forest <- fastcforest(as.formula(paste("response", paste(selections[[i]],
                                                             collapse = " + "), sep = " ~ ")), data = dat, # fit forest
                          controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                          parallel = parallel)

    errors[i] <- error(forest, ...)
    
    # define the next set of variables
    # if (recompute == F & i > 1) selections[[i - 1]] <- selections[[i]][-i]
    if (i > 1) selections[[i - 1]] <- names(sort(vi_fct(forest), decreasing = T))[-i]
  }
  
  print('final step')

  # define the number of variables determined by the 0 s.e. and 1 s.e. rule
  optimum.number.0se <- which.min(minSign*errors)
  optimum.number.1se <- which((minSign*errors) <= min(minSign*errors) + 1 * stats::sd(errors))[1]
  
  # compute the corresponding forests and OOB-errors
  # if (optimum.number.0se == 1) {forest.0se <- c(); selection.0se <- c(); exclusion.0se <- names(X)}
  # if (optimum.number.1se == 1) {forest.1se <- c(); selection.1se <- c(); exclusion.1se <- names(X)}
  # if (optimum.number.0se != 1) {
    selection.0se <- selections[[optimum.number.0se]]
    if(is.factor(Y)) mtry <- ceiling(sqrt(length(selection.0se))) # set mtry to sqrt() of remaining variables
    if(class(Y) %in% c('numeric','integer')) mtry <- ceiling(length(selection.0se)/3) # set mtry to ()/3 of remaining variables
    forest.0se <- fastcforest(as.formula(paste("response", paste(selection.0se,
                                                                 collapse = " + "), sep = " ~ ")), data = dat,
                              controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                              parallel = parallel)
  # }
  # if (optimum.number.1se != 1) {
    selection.1se <- selections[[optimum.number.1se]]
    if(is.factor(Y)) mtry <- ceiling(sqrt(length(selection.1se))) # set mtry to sqrt() of remaining variables
    if(class(Y) %in% c('numeric','integer')) mtry <- ceiling(length(selection.1se)/3) # set mtry to ()/3 of remaining variables
    forest.1se <- fastcforest(as.formula(paste("response", paste(selection.1se,
                                                                 collapse = " + "), sep = " ~ ")), data = dat,
                              controls = party::cforest_unbiased(mtry = mtry, ntree = ntree),
                              parallel = parallel)
  # }
  oob.error.0se <- errors[optimum.number.0se]
  oob.error.1se <- errors[optimum.number.1se]
  selection.0se <- names(X)[as.numeric(gsub('V','',selection.0se))]
  selection.1se <- names(X)[as.numeric(gsub('V','',selection.1se))]
  exclusion.0se <- base::setdiff(names(X),selection.0se)
  exclusion.1se <- base::setdiff(names(X),selection.1se)
  return(list("selection.0se" = selection.0se, "exclusion.0se" = exclusion.0se, "forest.0se" = forest.0se, "oob.perf.0se" = oob.error.0se,
              "selection.1se" = selection.1se, "exclusion.1se" = exclusion.1se, "forest.1se" = forest.1se, "oob.perf.1se" = oob.error.1se))
}

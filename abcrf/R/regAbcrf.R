regAbcrf.formula <- function(formula, data, ntree=500, mtry=max(floor((dim(data)[2]-1)/3), 1),
                             sampsize=min(1e5, nrow(data)), paral=FALSE, ncores=if(paral) max(detectCores()-1,1) else 1, ...)
{
  
  if (!inherits(formula, "formula"))
    stop("regAbcrf.formula is only for formula objects")
  if (!inherits(data, "data.frame"))
    stop("data needs to be a data.frame object")
  if (nrow(data) == 0L || is.null(nrow(data)) )
    stop("no simulation in the reference table (resp, sumstat)")
  if(sampsize > nrow(data))
    stop("sampsize too large")
  if ( (!is.logical(paral)) || (length(paral) != 1L) )
    stop("paral should be TRUE or FALSE")
  if(is.na(ncores)){
    warning("Unable to automatically detect the number of CPU cores, \n1 CPU core will be used or please specify ncores.")
    ncores <- 1
  }
  
  mf <- match.call(expand.dots=FALSE)
  m <- match(c("formula", "data"), names(mf))
  mf <- mf[c(1L,m)]
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame() )
  mt <- attr(mf, "terms")
  
  if ( !is.numeric(model.response(mf)) ) 
     stop("response variable should be numeric")
    
  model.rf <- ranger(formula, data=data, num.trees=ntree, mtry=mtry, sample.fraction=sampsize/nrow(data), 
                     num.threads = ncores, keep.inbag = TRUE, importance = 'impurity', ...)
  
  model.rf$NMAE <- mean( abs( (model.response(mf) - model.rf$predictions) / model.response(mf) ) )
  
  cl <- match.call()
  cl[[1]] <- as.name("regAbcrf")
  x <- list(call=cl, formula=formula, model.rf = model.rf)
  class(x) <- "regAbcrf"
  x
}

regAbcrf.default <- 
function(...) {
  cl <- match.call()
  cl[[1]] <- as.name("regAbcrf")
  cat("call:\n")
  print(cl)
  stop("the first argument should be a formula or a factor")
}

regAbcrf <-
function(...) UseMethod("regAbcrf")

print.regAbcrf <-
function(x, ...){
  cat("\nCall:\n", deparse(x$call), "\n\n")
  cat("Number of simulations: ", x$model.rf$num.samples, "\n", sep="")
  cat("Number of trees: ", x$model.rf$num.trees, "\n", sep="")
  cat("Number of variables tried at each split: ", x$model.rf$mtry, "\n", sep="")
  cat("\nOut-of-bag prior mean squared error: ", x$model.rf$prediction.error, "\n", sep = "")
  cat("Out-of-bag prior normalized mean absolute error: ", x$model.rf$NMAE, "\n", sep = "")
}
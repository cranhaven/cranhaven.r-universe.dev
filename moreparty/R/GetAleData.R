# @importFrom 

#' @export

GetAleData <- function(object, xnames=NULL, order=1, grid.size=20, parallel=FALSE) {
  response = object@responses
  CLASS = all(response@is_nominal | response@is_ordinal)
  if(CLASS==TRUE) {
    pred.fun <- function(model, newdata) { do.call('rbind',predict(model,newdata=newdata,type='prob'))[,2] }
  }
  if(CLASS==FALSE) {
    pred.fun <- function(model,newdata) { predict(model,newdata=newdata)[,1] }
  }
  input = object@data@get("input")
  if(order==2 & length(xnames)!=2) stop("for 2nd order ALE, xnames should be of length 2")
  if(is.null(xnames)) xnames = colnames(input)
  y = object@responses@variables[[1]]
  dt <- data.frame(y,input)
  dt <- dt[complete.cases(dt),]
  predictor = iml::Predictor$new(object, data=dt[,-1], y=dt[,1], predict.fun=pred.fun)
  if(order==1) {
    foo <- function(feat) {
      temp <- iml::FeatureEffect$new(predictor, feature=feat, method='ale', grid.size=grid.size)$results
      res <- data.frame(var=rep(feat,nrow(temp)), cat=as.character(temp[,feat]), value=temp[,'.value'])
      return(res)
    }
    ale <- plyr::alply(xnames, 1, .fun=foo, .parallel=parallel, .paropts=list(.packages="iml"))
    ale <- do.call('rbind.data.frame',ale)
    rownames(ale) <- NULL
    ale$cat <- as.character(ale$cat)
    # ale$var <- as.character(ale$var)
  }
  if(order==2) {
    ale <- iml::FeatureEffect$new(predictor, feature=xnames, method='ale')$results
    ale <- data.frame(ale[,xnames], value=ale[,'.ale'])
  }
  return(ale)
}
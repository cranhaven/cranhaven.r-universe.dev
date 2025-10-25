#' Feature selection
#'
#' This function selects important features from the dataset
#'
#' @param x a matrix of predictor variables
#' @param y a vector of binary outcome
#' @param method feature selection method, default is iHCT
#'
#' @return \code{featureselection()} returns selected features and other outcomes needed for sample size determination.
#'
#' @import glmnet caret lubridate Matrix
#' @importFrom stats coef
#'
#' @examples
#'
#' ## load data
#' #pilot.data = readRDS(system.file("extdata", "pilotdata.rds", package = "planningML"))
#' #x = pilot.data[,-ncol(pilot.data)]
#' #y = pilot.data$DEPRESSION
#'
#' ## select important features
#' #features = featureselection(x = x, y = y)
#' #summary(features)
#'
#' @export
#'

featureselection = function(x=NULL, y=NULL, method = "iHCT"){

  # check contents of input dataset
  if (is.null(x) | is.null(y)){
    warning("The input dataset is not complete.")
  }

  # preprocess data
  # assume users give pilot data or historical data they have
  pilot.data<-data.frame(x,y)

  ## remove columns with all zeros
  pilot.data.filt<-pilot.data[vapply(pilot.data, function(x) length(unique(x)) > 1, logical(1L))]

  #Scale pilot data (has 9315 covariates)
  scaled.pilot.data<- data.frame( apply(pilot.data.filt, 2,scale))
  scaled.pilot.data[,ncol(scaled.pilot.data)]<-y

  ## remove features with no variation or whose values are almost constant for all subjects
  sd_check<-which(is.na(apply(pilot.data[,-ncol(pilot.data)],2,sd))== TRUE)
  if (length(sd_check) != 0){
    pilot.data<-pilot.data[,-sd_check]
  }

  x = as.matrix(pilot.data[,-ncol(pilot.data)])
  y = pilot.data[,ncol(pilot.data)]

  #run lasso over many seeds on pilot data and take mean of m and delta as parameters

  selectnum<-rep(NA,100)
  delta.pilot.mean<-rep(NA,100)
  truselects.list <- list()
  for (i in 1:100) {
    set.seed(i)
    mod <- cv.glmnet(as.matrix(pilot.data[,-ncol(pilot.data)]), y=pilot.data[,ncol(pilot.data)], alpha=1, type.measure = "auc",family="binomial")
    best_lambda <- mod$lambda.min
    bmod <- glmnet(as.matrix(pilot.data[,-ncol(pilot.data)]), pilot.data[,ncol(pilot.data)], alpha = 1, lambda = best_lambda, type.measure = "auc",family="binomial")
    truselects = names(which(as.matrix(coef(bmod))[,1] != 0))[-1]
    if (!identical(truselects, character(0))){
      truselects.list[[i]] = truselects
    } else {
      truselects.list[[i]] = NA
    }

    if (length(truselects) == 1){
      tmp = pilot.data[,truselects]
      effect_size_pilot<- abs(mean(tmp[which(pilot.data[,ncol(pilot.data)] == 0)]) - mean(tmp[which(pilot.data[,ncol(pilot.data)] == 1)]))
    } else {
      effect_size_pilot<- apply(pilot.data[,truselects] , 2, function(x)
        abs(mean(x[which(pilot.data[,ncol(pilot.data)] == 0)]) - mean(x[which(pilot.data[,ncol(pilot.data)] == 1)])))
    }
    delta.pilot = effect_size_pilot/2
    selectnum[i] = length(truselects)
    delta.pilot.mean[i] = mean(effect_size_pilot/2)
    cat(i)
  }
  selectmean = mean(selectnum)
  delta.p.mean=mean(delta.pilot.mean,na.rm=T)
  delta.pilot<-rep(delta.p.mean,round(mean(selectnum)))

  truselects.total <-unlist(truselects.list, recursive = FALSE)

  truselects.freq <- as.data.frame(table(truselects.total))

  truselects.freq <- truselects.freq[order(truselects.freq$Freq, decreasing = TRUE),]

  # #Indices of the selected features
  # index = match(selected_features,colnames(pilot.data))
  index = match(truselects.freq$truselects.total,colnames(pilot.data))

  selected_features = colnames(pilot.data)[index[1:round(selectmean)]]

  output = list(x = x,
                y = y,
                data = pilot.data,
                index =  index,
                selectnum = round(selectmean),
                features = selected_features,
                delta.pilot = delta.pilot)


  class(output)<-"planningML"
  return(output)


}

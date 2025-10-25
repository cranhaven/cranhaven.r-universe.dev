#'
#' Generate descriptive summary for objects returned by functions in EHRsampling
#'
#' Generate descriptive summary for objects returned by functions in EHRsampling.
#'
#'
#' @param df data for learning curve fitting; first column is sample size, second column is AUC measurement.
#' @param testX test data for prediction
#' @param target target MCC/AUC that you want to achieve
#'
#' @importFrom stats deriv3 nls relevel
#' @importFrom stats vcov na.exclude predict
#' @return \code{fit_learningcurve()} returns the estimated power law model for the learning curve.
#'
#'
#' @export


fit_learningcurve <- function(df, testX, target=NULL) {
  N <- nrow(df)
  x <- df[,1]
  y <- df[,2]
  w <- lapply(x, function(i) { i / N}) %>% unlist

  metric <- colnames(df)[2]

  gradientF<-deriv3(~(1-a)-(b*(x^c)), c("a","b","c"), function(a,b,c,x) NULL)
  startParams <- list(a=0, b=1, c=-0.5)

  error_m <- try(m <- nls(y~gradientF(a,b,c,x), start = startParams, weights=w,
           control = list(maxiter=1000, warnOnly = TRUE),
           algorithm = "port", upper = list(a=10, b = 10, c = -0.1), lower = list(a = 0, b = 0, c=-10),
           data = data.frame(y=y, x=x)))

  # if (class(error_m) == "try-error"){
  #   return("The learning curve model cannot be fit.")
  # }

  prediction<-predict(m, list(x=testX))

  #confidence intervals
  vcovmatrix = NA
  error<- try(vcovmatrix <- vcov(m))
  if(!any(is.na(vcovmatrix))){
    se.fit <- sqrt(apply(attr(predict(m, list(x=testX)),"gradient"),1,
                         function(x) sum(vcovmatrix*outer(x,x))))
    prediction.ci <- prediction + outer(se.fit,qnorm(c(.5, .025,.975)))
    predictY<-prediction.ci[,1]
    predictY.lw<-prediction.ci[,2]
    predictY.up<-prediction.ci[,3]

    res = list(model = m,
               x = testX,
               metric = metric,
               predY = predictY,
               predY.lw = predictY.lw,
               predY.up = predictY.up)

    if (!is.null(target)){
      targetsize = testX[which(prediction > target)[1]]
      res = list(model = m,
                 x = testX,
                 metric = metric,
                 predY = predictY,
                 predY.lw = predictY.lw,
                 predY.up = predictY.up,
                 sample.size.for.target.metric = targetsize)
    }

  } else {
    #predictions on unseen data

    res = list(model = m,
               x = testX,
               metric = metric,
               predY = prediction[1:length(testX)])

    if (!is.null(target)){
      targetsize = testX[which(prediction > target)[1]]
      res = list(model = m,
                 x = testX,
                 metric = metric,
                 predY = prediction[1:length(testX)],
                 sample.size.for.target.metric = targetsize)
    }
  }

  class(res)<-"planningML"
  return(res)

}

predict.expectreg <-
function(object,newdata=NULL,with_intercept=T,...)
{
  object$predict(newdata,with_intercept=with_intercept)
}

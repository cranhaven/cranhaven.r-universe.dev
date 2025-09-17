summary <- function(object, Nbt=20) UseMethod("summary")
summary.ilse <- function(object, Nbt=20){
  Est <- object$beta
  res <- matrix(0, nrow=length(Est), ncol=4)
  Acov <- bootstrap(object, repTimes = Nbt)
  stdErr <- sqrt(diag(Acov))
  Zvalue <- Est / stdErr
  Pvalue <- 2*(1-pnorm(abs(Zvalue)))
  res[,1] <- Est
  res[,2] <- stdErr
  res[,3] <- Zvalue
  res[,4] <- Pvalue
  row.names(res) <- names(Est)
  colnames(res) <- c('Estimate', 'std. Error', 'Z value', 'Pr(>|Z|)')
  res
}

summary.fiml <- function(object, Nbt=20){
  Est <- object$beta
  res <- matrix(0, nrow=length(Est), ncol=4)
  Acov <- bootstrap(object, repTimes = Nbt)
  stdErr <- sqrt(diag(Acov))
  Zvalue <- Est / stdErr
  Pvalue <- 2*(1-pnorm(abs(Zvalue)))
  res[,1] <- Est
  res[,2] <- stdErr
  res[,3] <- Zvalue
  res[,4] <- Pvalue
  row.names(res) <- names(Est)
  colnames(res) <- c('Estimate', 'std. Error', 'Z value', 'Pr(>|Z|)')
  res
}

print <- function(object) UseMethod("print")
print.ilse <- function(object) print(object[c(1,3:5)])
print.fiml <- function(object) print(object[1:2])

Coef <- function(object) {
  if(!is.element(class(object), c('ilse', 'fiml')))
    stop('object must be class "ilse" or "fiml"!\n')
  return(object$beta)
}
Fitted.values <- function(object){
  if(!is.element(class(object), c('ilse')))
    stop('object must be class "ilse"!\n')
  return(object$fitted.values)
}
Residuals <- function(object){
  if(!is.element(class(object), c('ilse')))
    stop('object must be class "ilse"!\n')
  return(object$residuals)
}

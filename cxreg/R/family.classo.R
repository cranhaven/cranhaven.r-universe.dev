#' @method family classo
#' @export
family.classo <- function(object,...){
  families <- c(elnet = "gaussian", lognet = "binomial", fishnet = "poisson",
                multnet = "multinomial", coxnet = "cox", mrelnet = "mgaussian")
  cl <- class(object)[1]
  families[cl]
}

#' @method family classofit
#' @export
family.classofit <- function(object,...){
  object$family
}

#' @method family cv.classo
#' @export
family.cv.classo <- function(object,...){
  family(object$classo.fit)
}
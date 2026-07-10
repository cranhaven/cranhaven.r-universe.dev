#' Extract data of model
#'
#' @param fit fitted results
#' @name model.data
#' @importFrom stats as.formula update
#' @return dataframe in the model
#' @export
#'
#' @examples
#' fit <- lm(mpg~vs+am+poly(qsec,2),data=mtcars)
#' head(model.data(fit))
#' model.y(fit)
#' model.x(fit)
model.data<-function(fit){
    eval(fit$call$data)
}
#' @rdname model.data
#' @return
#' @export
#'
model.y <- function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[c(1,2)]
    }else{
        all.vars(fit$terms)[1]
    }
}
#' @rdname model.data
#' @return
#' @export
#'
model.x <- function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[-c(1,2)]
    }else{
        all.vars(fit$terms)[-1]
    }
}

#' Convert results of lm() to ols()
#' @description Convert results of lm() from 'stats' package to ols() from 'rms' package.
#' @param fit reults of lm()
#' @importFrom rms ols
#' @return results of ols()
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ disp,data=mtcars)
#' lm2ols(fit)
lm2ols <- function(fit){
    fit=update(fit,x=TRUE,y=TRUE,model=TRUE)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('lm','ols',call)
    call.new=trans.base2rms(call.new)
    data.name=fit$call$data
    if (!is.null(data.name)){
        data=paste0(deparse(data.name),'=','fit$model')
        eval(parse(text = data))
        eval(parse(text=call.new))
    }else{
        stop('data must be given in formula')
    }
}
#' Convert results of ols() to lm()
#' @description Convert results of ols() from 'rms' package to lm() from 'stats' package.
#' @param fit reults of ols()
#' @importFrom rms ols
#' @return results of lm()
#' @export
#'
#' @examples
#' library(rms)
#' fit <- ols(mpg ~ disp^2,data=mtcars)
#' ols2lm(fit)
ols2lm <- function(fit){
    fit=update(fit,x=TRUE,y=TRUE,model=TRUE)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('ols','lm',call)
    call.new=trans.rms2base(call.new)
    data.name=fit$call$data
    if (!is.null(data.name)){
        data=paste0(deparse(data.name),'=','fit$model')
        eval(parse(text = data))
        eval(parse(text=call.new))
    }else{
        stop('data must be given in formula')
    }
}

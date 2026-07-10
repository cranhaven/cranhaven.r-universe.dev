#' Convert results of logistic regression from glm() to lrm()
#' @description Convert results of logistic regression from glm() in 'stats' package to lrm() in 'rms' package.
#' @param fit logistic regression reults of glm()
#' @importFrom rms lrm
#' @return results of lrm()
#' @export
#'
#' @examples
#' fit <- glm(vs~mpg+cyl,data=mtcars, family = binomial(link = 'logit'))
#' logit2lrm(fit)
logit2lrm <- function(fit){
    fit=update(fit,x=TRUE,y=TRUE,model=TRUE)
    fit=update(fit,family=)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('glm','lrm',call)
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
#' Convert results of logistic regression from lrm() to glm()
#' @description Convert results of logistic regression from lrm() in 'rms' package to glm() in 'stats' package.
#' @param fit logistic regression reults of lrm()
#' @return results of glm()
#' @export
#'
#' @examples
#' library(rms)
#' fit <- lrm(vs ~ mpg + cyl, data = mtcars)
#' lrm2logit(fit)
lrm2logit <- function(fit){
    fit=update(fit,x=TRUE,y=TRUE,model=TRUE)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('lrm','glm',call)
    call.new=trans.rms2base(call.new)
    data.name=fit$call$data
    if (!is.null(data.name)){
        data=paste0(deparse(data.name),'=','fit$model')
        eval(parse(text = data))
        fit2=eval(parse(text=call.new))
        update(fit2, family = binomial(link = 'logit'))
    }else{
        stop('data must be given in formula')
    }
}

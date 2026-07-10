#' Convert results of cox regression from coxph() to cph()
#' @description Convert results of cox regression from coxph() in 'survival' package to cph() in 'rms' package.
#' @param fit cox regression results of coxph()
#' @importFrom rms cph
#' @importFrom stats update binomial
#' @return results of coxph()
#' @export
#'
#' @examples
#' library(survival)
#' fit <- coxph(Surv(mpg,vs)~am+gear+carb,data=mtcars)
#' coxph2cph(fit)
coxph2cph <- function(fit){
    if (class(fit) != 'coxph') stop('fit must be coxph results()')
    fit=update(fit,x=TRUE,y=TRUE,model=TRUE)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('coxph','cph',call)
    call.new=trans.base2rms(call.new)
    data.name=fit$call$data
    if (!is.null(data.name)){
        fit$model$timeggg=as.numeric(fit$model[,1])[1:nrow(fit$model)]
        fit$model$eventggg=as.numeric(fit$model[,1])[-c(1:nrow(fit$model))]
        colnames(fit$model)[(ncol(fit$model)-1):ncol(fit$model)]=        strsplit(do::Replace0(call,c('.*formula = Surv\\(','\\) ~.*')),', ')[[1]]
        fit$model=fit$model[,-1]
        data=paste0(deparse(data.name),'=','fit$model')
        eval(parse(text = data))
        eval(parse(text=call.new))
    }else{
        stop('data must be given in formula')
    }
}
#' Convert results of cox regression from cph() to coxph()
#' @description Convert results of cox regression from cph() in 'rms' package to coxph() in 'stats' package.
#' @param fit cox regression results of cph()
#' @importFrom survival coxph
#' @return results of coxph()
#' @export
#'
#' @examples
#' library(rms)
#' fit <- cph(formula = Surv(mpg, vs) ~ am + gear + carb, data = mtcars)
#' cph2coxph(fit)
cph2coxph <- function(fit){
    if (class(fit)[1] != 'cph') stop('fit must be result of cph()')
    fit=update(fit,x=TRUE,y=TRUE,model=TRUE)
    call=paste0(deparse(fit$call),collapse = '')
    call.new=sub('cph','coxph',call)
    call.new=trans.rms2base(call.new)
    data.name=fit$call$data
    if (!is.null(data.name)){
        fit$model$timeggg=as.numeric(fit$model[,1])[1:nrow(fit$model)]
        fit$model$eventggg=as.numeric(fit$model[,1])[-c(1:nrow(fit$model))]
        colnames(fit$model)[(ncol(fit$model)-1):ncol(fit$model)]=        strsplit(do::Replace0(call,c('.*formula = Surv\\(','\\) ~.*')),', ')[[1]]
        fit$model=fit$model[,-1]
        data=paste0(deparse(data.name),'=','fit$model')
        eval(parse(text = data))
        eval(parse(text=call.new))
    }else{
        stop('data must be given in formula')
    }
}

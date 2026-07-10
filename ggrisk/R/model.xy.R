model.data<-function(fit){
    eval(fit$call$data)
}

model.y<-function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[c(1,2)]
    }else{
        all.vars(fit$terms)[1]
    }
}
model.x<-function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[-c(1,2)]
    }else{
        all.vars(fit$terms)[-1]
    }
}

createPhi <- function(obj, data, argvals, nPhi = NULL){

    if(is.null(nPhi)){
        nPhi <- ncol(obj$eigenfunctions)
    }

    ut <- obj$argvals.new
    for(i in 1:nPhi){
        cur    <- vapply(data[[argvals]], function(x) obj$eigenfunctions[ut==x,i], numeric(1))
        data[[paste("phi",i,sep="")]] <- cur

        cur_sp <- obj$sigma2/obj$eigenvalues[i]
        assign(paste("sp",i,sep=""),cur_sp , envir = parent.frame())
    }

    data
}


createFormula <- function(obj, formula = NULL, sp = FALSE, nPhi = NULL){
    stopifnot(!is.null(formula))
    if(is.null(nPhi)){
        nPhi <- ncol(obj$eigenfunctions)
    }

    if(!sp){
        phiForm <- paste("s(g, by = phi",1:nPhi,", bs='re') ", collapse="+",sep="")
    }
    if(sp){
        phiForm <- paste("s(g, by = phi",1:nPhi,", bs='re', sp = sp",1:nPhi,") ", collapse="+",sep="")
    }
    model <- as.character(formula)
    paste(model[2], model[1], model[3], "+", phiForm)
}


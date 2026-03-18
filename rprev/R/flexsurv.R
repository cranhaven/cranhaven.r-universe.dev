
#' @export
extract_covars.flexsurvreg <- function(object) {
    attr(object$concat.formula, "covnames")
}

#' @export
predict_survival_probability.flexsurvreg <- function(object, newdata=NULL,
                                                     times=NULL)
{
    x <- object
    dat <- x$data
    t <- times  # Needed for the call as it uses current environment rather than passing vars
    # in directly
    Xraw <- model.frame(x)[,unique(attr(model.frame(x),"covnames.orig")),drop=FALSE]
    isfac <- sapply(Xraw, function(x){is.factor(x) || is.character(x)})

    X <- form.model.matrix(object, as.data.frame(newdata))

    fn <- summary.fns(x)
    fn <- expand.summfn.args(fn)
    beta <- if (x$ncovs==0) 0 else x$res[x$covpars,"est"]

    if (ncol(X) != length(beta)){
        ## don't think we should ever reach here - error should be caught in newdata or X
        isare <- if(length(beta)==1) "is" else "are"
        plural <- if(ncol(X)==1) "" else "s"
        pluralc <- if(length(beta)==1) "" else "s"
        stop("Supplied X has ", ncol(X), " column",plural," but there ",isare," ",
             length(beta), " covariate effect", pluralc)
    }

    dlist <- x$dlist

    # Obtain distribution parameters for each individual
    all_pars <- lapply(1:nrow(X), function(i) {
        basepars.mat <- add.covs(x, x$res.t[dlist$pars,"est"], beta, X[i,,drop=FALSE], transform=FALSE)
        as.list(as.data.frame(basepars.mat))
    })

    # Now convert this to a list of parameters rather than a list of patients
    fnlist <- list(t)
    for (par in dlist$pars) {
        fnlist[[par]] <- sapply(all_pars, function(x) x[[par]])
    }

    do.call(fn, fnlist)
}

form.model.matrix <- function(object, newdata){
    mfo <- model.frame(object)

    ## If required covariate missing, give a slightly more informative error message than, e.g.
    ## "Error in eval(expr, envir, enclos) (from flexsurvreg.R#649) : object 'sex' not found"
    covnames <- attr(mfo, "covnames")
    missing.covs <- unique(covnames[!covnames %in% names(newdata)])
    if (length(missing.covs) > 0){
        missing.covs <- sprintf("\"%s\"", missing.covs)
        plural <- if (length(missing.covs)>1) "s" else ""
        stop(sprintf("Value%s of covariate%s ",plural,plural), paste(missing.covs, collapse=", "), " not supplied in \"newdata\"")
    }

    ## as in predict.lm
    tt <- attr(mfo, "terms")
    Terms <- delete.response(tt)
    mf <- model.frame(Terms, newdata, xlev = .getXlevels(tt, mfo))
    if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, mf)

    forms <- object$all.formulae
    mml <- vector(mode="list", length=length(object$dlist$pars))
    names(mml) <- names(forms)
    forms[[1]] <- delete.response(terms(forms[[1]]))
    for (i in names(forms)){
        mml[[i]] <- model.matrix(forms[[i]], mf)
    }
    X <- compress.model.matrices(mml)

    attr(X, "newdata") <- mf # newdata with any extra variables stripped.  Used to name components of summary list
    X
}

compress.model.matrices <- function(mml){
    cbind.drop.intercept <- function(...)do.call("cbind", lapply(list(...), function(x)x[,-1,drop=FALSE]))
    X <- do.call("cbind.drop.intercept",mml)
    loc.cnames <- colnames(mml[[1]])[-1]
    anc.cnames <- unlist(mapply(function(x,y)sprintf("%s(%s)",x,y), names(mml[-1]), lapply(mml[-1], function(x)colnames(x)[-1])))
    cnames <- c(loc.cnames, anc.cnames)
    colnames(X) <- cnames
    X
}

#' @exportS3Method base::summary
summary.fns <- function(object, ...){
   function(t, ...) {
       1 - object$dfns$p(t,...)
   }
}

expand.summfn.args <- function(summfn){
    summfn2 <- summfn
    args <- c(alist(t=), formals(summfn))
    formals(summfn2) <- args[!duplicated(names(args))]
    body(summfn2) <- body(summfn)
    summfn2
}

add.covs <- function(x, pars, beta, X, transform=FALSE){  ## TODO option to transform on input
    nres <- nrow(X)
    if (!is.matrix(pars)) pars <- matrix(pars, nrow=nres, ncol=length(pars), byrow=TRUE)
    if (!is.matrix(beta)) beta <- matrix(beta, nrow=1)
    for (j in seq(along=x$dlist$pars)){
        covinds <- x$mx[[x$dlist$pars[j]]]
        if (length(covinds) > 0){
            pars[,j] <- pars[,j] + beta[,covinds] %*% t(X[,covinds,drop=FALSE])
        }
        if (!transform)
            pars[,j] <- x$dlist$inv.transforms[[j]](pars[,j])
    }
    colnames(pars) <- x$dlist$pars
    pars
}

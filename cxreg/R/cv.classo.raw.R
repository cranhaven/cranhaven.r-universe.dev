cv.classo.raw <- function (x,y,weights,lambda,type.measure,nfolds,foldid,
                            alignment,keep,parallel,trace.it,classo.call,cv.call,...) {

    # ------------------------------------------------ #
    if (isTRUE(trace.it == 1)) {
      cat("Training\n")
    }

    classo.object <- classo(x, y,
                            weights=weights,
                            lambda=lambda,
                            standardized=TRUE,
                            intercept=FALSE,
                            maxit=100000,
                            trace.it=0,...)
    classo.object$call <- classo.call
    subclass <- class(classo.object)[[1]]

    # ------------------------------------------------ #
    # cvtype.R
    type.measure <- cvtype(type.measure,subclass)

    # # ------------------------------------------------ #
    lambda <- classo.object$lambda
    nz <- sapply(predict(classo.object, type = "nonzero"), length)
    outlist <- as.list(seq(nfolds))
    N <- nrow(x)
    # if (parallel) {
    #   #  if (parallel && require(foreach)) {
    #   outlist = foreach(i = seq(nfolds), .packages = c("classo")) %dopar% {
    #       which = foldid == i
    #       #      if (is.matrix(y))
    #       if (length(dim(y))>1){
    #         y_sub <- y[!which, ]
    #       } else {
    #         y_sub <- y[!which]
    #       }
    #       classo(x[!which, , drop = FALSE], y_sub, lambda = lambda,
    #              weights = weights[!which])
    #
    #   }
    # }else {
    #   for (i in seq(nfolds)) {
    #     if (trace.it) {
    #       cat(sprintf("Fold: %d/%d\n", i, nfolds))
    #     }
    #
    #     which = foldid == i
    #     if (length(dim(y))>1){
    #       y_sub <- y[!which, ]
    #     } else {
    #       y_sub = y[!which]
    #     }
    #     outlist[[i]] <- classo(x[!which, , drop = FALSE], y_sub, lambda = lambda,
    #                           weights = weights[!which],trace.it=trace.it)
    #   }
    # }

    outlist <- as.list(seq(nfolds))
    lambda <- classo.object$lambda
    for (i in seq(nfolds)) {
      if (isTRUE(trace.it == 1)) {
        cat(sprintf("Fold: %d/%d\n", i, nfolds))
      }

      which = foldid == i
      if (length(dim(y))>1){
        y_sub <- y[!which, ]
      } else {
        y_sub = y[!which]
      }
      x_sub <- x[!which, , drop = FALSE]
      weights_sub <- weights[!which]
      outlist[[i]] <- classo(x = x_sub, y = y_sub,
                             weights=weights_sub,
                             lambda = lambda,
                             standardized=TRUE,
                             intercept=FALSE,
                             maxit=100000,
                             trace.it=0)
    }

    # ------------------------------------------------ #
    # buildPredmat.default.R
    class(outlist) <- paste0(subclass,"list")
    predmat <- buildPredmat(outlist,lambda,x,foldid,alignment)

    # ------------------------------------------------ #
    ### Next we compute the measures
    fun <- paste("cv", subclass, sep = ".")
    cvstuff <- do.call(fun, list(predmat,y,type.measure,weights,foldid))


    # ------------------------------------------------ #
    # cvstats.R
    out <- cvstats(cvstuff,foldid,nfolds,lambda)
    cvname <- names(cvstuff$type.measure)
    names(cvname) <- cvstuff$type.measure
    out <- c(out, list(
      call = cv.call,
      name = cvname,
      classo.fit = classo.object,
      nzero = nz
    ))
    if (keep){
      out <- c(out, list(fit.preval = predmat, foldid = foldid))
    }

    # ------------------------------------------------ #
    # getOptcv.classo.R
    lamin <- with(out,getOptcv.classo(lambda, cvm, cvsd, cvname))
    obj <- c(out, as.list(lamin))
    class(obj) <- "cv.classo"
    obj
  }

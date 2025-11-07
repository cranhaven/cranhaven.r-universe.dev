funLBM <-
  function(X,K,L,maxit=50,burn=25,basis.name='fourier',nbasis=15,nbinit=1,
           gibbs.it=3,display=FALSE,init='funFEM',mc.cores=1,...){
    call = match.call()
    if (length(K) > 1 | length(L) >1 | nbinit > 1){

      if(Sys.info()[['sysname']] == "Windows"){

        models= expand.grid(K=K,L=L)

        RES<-list()
        outNA<-data.frame(matrix(nrow=1,ncol=2))
        colnames(outNA)<-c("K","icl")

        for (a in 1:nrow(models)){
          RES[[a]]<-tryCatch(funLBM(X=X,K=models[a,1],L=models[a,2],maxit=maxit,burn=burn,basis.name=basis.name,nbasis=nbasis,gibbs.it=gibbs.it,display=FALSE,
                           init=init,simplify=FALSE),warning=function(w)" ",error=function(e)outNA)
        }

        models$icl=unlist(sapply(RES,function(x){if (is.list(x)){x$icl} else NA}))
        best = which.max(models$icl)
        if (length(best)==0) stop("Any models converge")
        out = RES[[best]]
        models = models[order(models$icl,decreasing = TRUE),]
        if (display) print(models)
        out$allRes = RES
        out$criteria = models
        # mc.cores=1
        # models = expand.grid(K=K,L=L)
        # models = do.call(rbind, replicate(nbinit, models, simplify=FALSE))
        # MoreArgs = list(X=X,maxit=maxit,burn=burn,basis.name=basis.name,nbasis=nbasis,gibbs.it=gibbs.it,display=FALSE,
        #                 init=init,simplify=FALSE)
        # RES = do.call(mcmapply, c(list(FUN=funLBM.main, MoreArgs = MoreArgs, mc.cores = mc.cores,
        #                                mc.preschedule = FALSE),models))
        # if (is.matrix(RES)) {
        #   models$icl = unlist(apply(RES,2,function(x){if (is.list(x)){x$icl} else NA}))
        #   best = which.max(models$icl)
        #   out = RES[,best]
        # }else {
        #   models$icl = unlist(sapply(RES,function(x){if (is.list(x)){x$icl} else NA}))
        #   best = which.max(models$icl)
        #   out = RES[[best]]
        # }
        # models = models[order(models$icl,decreasing = TRUE),]
        # if (display) print(models)
        # out$allRes = RES
        # out$criteria = models

      }else{
        models = expand.grid(K=K,L=L)
        models = do.call(rbind, replicate(nbinit, models, simplify=FALSE))
        MoreArgs = list(X=X,maxit=maxit,burn=burn,basis.name=basis.name,nbasis=nbasis,gibbs.it=gibbs.it,display=FALSE,
                        init=init,simplify=FALSE)
        RES = do.call(mcmapply, c(list(FUN="funLBM.main", MoreArgs = MoreArgs, mc.cores = mc.cores,
                                       mc.preschedule = FALSE),models))
        if (is.matrix(RES)) {
          models$icl = unlist(apply(RES,2,function(x){if (is.list(x)){x$icl} else NA}))
          best = which.max(models$icl)
          out = RES[,best]
        }else {
          models$icl = unlist(sapply(RES,function(x){if (is.list(x)){x$icl} else NA}))
          best = which.max(models$icl)
          out = RES[[best]]
        }
        models = models[order(models$icl,decreasing = TRUE),]
        if (display) print(models)
        out$allRes = RES
        out$criteria = models
      }
    }else{out = funLBM.main(X=X,K=K,L=L,maxit=maxit,burn=burn,basis.name=basis.name,nbasis=nbasis,
                            gibbs.it=gibbs.it,display=display,init=init)
    }
    out$call = call
    class(out) = 'funLBM'
    out
  }

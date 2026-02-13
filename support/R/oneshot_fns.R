sp <- function(n, p, ini=NA,
               dist.str=NA, dist.param=vector("list",p),
               dist.samp=NA, scale.flg=TRUE, wts=NA, bd=NA,
               num.subsamp=ifelse(any(is.na(dist.samp)),
               max(10000,10*n),min(10000,nrow(dist.samp))),
               rnd.flg=ifelse(any(is.na(dist.samp)),
               TRUE,ifelse(num.subsamp<=10000,FALSE,TRUE)),
               iter.max=max(250,iter.min), iter.min=50,
               tol=1e-10, par.flg=TRUE, n0=n*p){
  
  #Set std.flg (standard distn or data reduction)
  if (!any(is.na(dist.samp))&&any(is.na(dist.str))){
    # data reduction
    std.flg = F;
  }
  else if(any(is.na(dist.samp))&&!any(is.na(dist.str))){
    # standard dist'n
    std.flg = T;
  }
  else{
    stop("Exactly one of 'dist.samp' or 'dist.str' should be NA.")
  }
  
  #ensure num.subsamp==nrow(dist.samp) if rnd.flg = FALSE
  if (!rnd.flg){
    num.subsamp <- nrow(dist.samp)
  }
  
  #Set weights
  if (any(is.na(wts))){
    if (std.flg){
      wts <- rep(1.0,num.subsamp)
    }else{
      wts <- rep(1.0,nrow(dist.samp))
    }
  }else{
    if (std.flg){
      stop("wts must be NA for standard distributions.")
    }else{
      wts <- nrow(dist.samp)*wts;
    }
  }
  
  # Set cores
  if (par.flg){
    num.cores <- parallel::detectCores()
  }else{
    num.cores <- 1
  }
  # print(num.cores)
  
  #Standard distributions
  if (std.flg){
    dist.samp <- matrix(-1,nrow=2,ncol=2)
    #Encoding distribution string
    dist.vec <- c("uniform","normal","exponential","gamma","lognormal","student-t","weibull","cauchy","beta")
    dist.ind <- rep(NA,p)
    for (i in 1:p){
      dist.ind[i] <- which(dist.vec==dist.str[i])
      if (!any(dist.vec==dist.str[i])){
        stop("Please provide a valid distribution!")
      }
    }
    
    #Setting bounds and distribution parameters
    ini.flg <- TRUE
    if (any(is.na(ini))){
      ini.flg <- FALSE
      if (p==1){
        ini <- matrix(randtoolbox::sobol(n,p,scrambling=T,seed=sample(1e6,1)),ncol=1)
      }else{
        ini <- randtoolbox::sobol(n,p,scrambling=T,seed=sample(1e6,1))
      }
    }
    
    if (any(is.na(bd))){
      bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
      bd.flg <- FALSE
    }else{
      bd.flg <- TRUE
    }
    
    for (i in 1:p){
      if (is.null(dist.param[[i]])){
        switch(dist.ind[i],
               "1" = {dist.param[[i]] <- c(0,1)}, #uniform
               "2" = {dist.param[[i]] <- c(0,1)}, #normal
               "3" = {dist.param[[i]] <- c(1)}, #exponential
               "4" = {dist.param[[i]] <- c(1,1)}, #gamma
               "5" = {dist.param[[i]] <- c(0,1)}, #lognormal
               "6" = {dist.param[[i]] <- c(1)}, #student-t
               "7" = {dist.param[[i]] <- c(1,1)}, #weibull
               "8" = {dist.param[[i]] <- c(0,1)}, #cauchy
               "9" = {dist.param[[i]] <- c(2,4)} #beta
        )
      }
      if (!ini.flg){
        switch(dist.ind[i],
               "1" = {ini[,i] <- stats::qunif(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
               "2" = {ini[,i] <- stats::qnorm(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
               "3" = {ini[,i] <- stats::qexp(ini[,i], dist.param[[i]][1])},
               "4" = {ini[,i] <- stats::qgamma(ini[,i], shape=dist.param[[i]][1], scale=dist.param[[i]][2])},
               "5" = {ini[,i] <- stats::qlnorm(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
               "6" = {ini[,i] <- stats::qt(ini[,i], df=dist.param[[i]][1])},
               "7" = {ini[,i] <- stats::qweibull(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
               "8" = {ini[,i] <- stats::qcauchy(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
               "9" = {ini[,i] <- stats::qbeta(ini[,i], dist.param[[i]][1],dist.param[[i]][2])}
        )
      }
      if (!bd.flg){
        switch(dist.ind[i],
               "1" = {bd[i,] <- c(0,1);},
               "2" = {bd[i,] <- c(-1e8,1e8);},
               "3" = {bd[i,] <- c(0,1e8);},
               "4" = {bd[i,] <- c(0,1e8);},
               "5" = {bd[i,] <- c(0,1e8);},
               "6" = {bd[i,] <- c(-1e8,1e8);},
               "7" = {bd[i,] <- c(0,1e8);},
               "8" = {bd[i,] <- c(-1e8,1e8);},
               "9" = {bd[i,] <- c(0,1);}
        )
      }
    }
    
    des <- sp_cpp(n,p,ini,dist.ind,dist.param,dist.samp,FALSE,
                  bd,num.subsamp,iter.max,iter.min,tol,num.cores,n0,wts,rnd.flg)
    
  }else{
    
    #ESS check
    
    #Standardize
    if (scale.flg==T){
      sdpts <- sqrt(apply(dist.samp,2,stats::var))
      mmpts <- apply(dist.samp,2,mean)
      dist.samp <- sweep(sweep(dist.samp,2,mmpts,"-"),2,sdpts,"/")
    }
    
    #Set bounds
    if (any(is.na(bd))){
      bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
      for (i in 1:p){
        bd[i,] <- range(dist.samp[,i])
      }
    }
    
    #Jitter if repeats
    if (any(duplicated(dist.samp))){
      dist.samp <- jitter(dist.samp)
      for (i in 1:p){ #enforce bounds
        # show(bd[i,])
        dist.samp[,i] <- pmin(pmax(dist.samp[,i],bd[i,1]),bd[i,2])
      }
    }
    
    #Set initial points
    dist.ind <- c(NA)
    dist.param <- list(NA)
    if (any(is.na(ini))){
      ini <- matrix(jitter(dist.samp[sample(1:nrow(dist.samp),n,F),]),ncol=p)
      # print(ini)
      for (i in 1:p){ #enforce bounds
        ini[,i] <- pmin(pmax(ini[,i],bd[i,1]),bd[i,2])
      }
    }else{
      ini <- sweep(sweep(ini,2,mmpts,"-"),2,sdpts,"/")
    }
    if (p==1){
      ini <- matrix(ini,ncol=1)
    }
    
    #Compute points
    des <- sp_cpp(n,p,ini,dist.ind,dist.param,dist.samp,TRUE,
                  bd,num.subsamp,iter.max,iter.min,tol,num.cores,n0,wts,rnd.flg)
    
    #Scale back
    if (scale.flg==T){
      ini <- sweep(sweep(ini,2,sdpts,"*"),2,mmpts,"+")
      des <- sweep(sweep(des,2,sdpts,"*"),2,mmpts,"+")
    }
    
    
  }

  return(list(sp=des,ini=ini,bd=bd))
}

# psp <- function(n, p,
#                 kern.str="spin", kern.param=c(lambda=0.1,nu=0.01), ini=NA,
#                dist.str=NA, dist.param=vector("list",p),
#                dist.samp=NA, scale.flg=T, wts=NA, bd=NA,
#                num.subsamp=ifelse(any(is.na(dist.samp)),
#                                   max(10000,10*n),min(10000,nrow(dist.samp))),
#                niter=10, warm.sp=TRUE,
#                tol=1e-10, par.flg=TRUE, ...){
# 
#   #Set std.flg (standard distn or data reduction)
#   if (!any(is.na(dist.samp))&&any(is.na(dist.str))){
#     # data reduction
#     std.flg = F;
#   }
#   else if(any(is.na(dist.samp))&&!any(is.na(dist.str))){
#     # standard dist'n
#     std.flg = T;
#   }
#   else{
#     stop("Exactly one of 'dist.samp' or 'dist.str' should be NA.")
#   }
# 
#   #Set weights
#   if (any(is.na(wts))){
#     if (std.flg){
#       wts <- rep(1.0,num.subsamp)
#     }else{
#       wts <- rep(1.0,nrow(dist.samp))
#     }
#   }else{
#     if (std.flg){
#       stop("wts must be NA for standard distributions.")
#     }else{
#       wts <- nrow(dist.samp)*wts;
#     }
#   }
# 
#   # Set cores
#   if (par.flg){
#     num.cores <- parallel::detectCores()
#   }else{
#     num.cores <- 1
#   }
#   # print(num.cores)
# 
#   #Standard distributions
#   if (std.flg){
#     # dist.samp <- matrix(-1,nrow=2,ncol=2)
#     #Encoding distribution string
#     dist.vec <- c("uniform","normal","exponential","gamma","lognormal","student-t","weibull","cauchy","beta")
#     dist.ind <- rep(NA,p)
#     for (i in 1:p){
#       dist.ind[i] <- which(dist.vec==dist.str[i])
#       if (!any(dist.vec==dist.str[i])){
#         stop("Please provide a valid distribution!")
#       }
#     }
# 
#     #Setting bounds and distribution parameters
#     ini.flg <- TRUE
#     if (any(is.na(ini))){
#       ini.flg <- FALSE
#       # if (p==1){
#       #   ini <- matrix(randtoolbox::sobol(n,p,scrambling=T,seed=sample(1e6,1)),ncol=1)
#       # }else{
#       #   ini <- randtoolbox::sobol(n,p,scrambling=T,seed=sample(1e6,1))
#       # }
#       ini <- matrix(runif(n*p),ncol=p)
#     }
# 
#     if (any(is.na(bd))){
#       bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
#       bd.flg <- FALSE
#     }
#     for (i in 1:p){
#       if (is.null(dist.param[[i]])){
#         switch(dist.ind[i],
#                "1" = {dist.param[[i]] <- c(0,1)}, #uniform
#                "2" = {dist.param[[i]] <- c(0,1)}, #normal
#                "3" = {dist.param[[i]] <- c(1)}, #exponential
#                "4" = {dist.param[[i]] <- c(1,1)}, #gamma
#                "5" = {dist.param[[i]] <- c(0,1)}, #lognormal
#                "6" = {dist.param[[i]] <- c(1)}, #student-t
#                "7" = {dist.param[[i]] <- c(1,1)}, #weibull
#                "8" = {dist.param[[i]] <- c(0,1)}, #cauchy
#                "9" = {dist.param[[i]] <- c(2,4)} #beta
#         )
#       }
#       if (!ini.flg){
#         switch(dist.ind[i],
#                "1" = {ini[,i] <- stats::qunif(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                "2" = {ini[,i] <- stats::qnorm(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                "3" = {ini[,i] <- stats::qexp(ini[,i], dist.param[[i]][1])},
#                "4" = {ini[,i] <- stats::qgamma(ini[,i], shape=dist.param[[i]][1], scale=dist.param[[i]][2])},
#                "5" = {ini[,i] <- stats::qlnorm(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                "6" = {ini[,i] <- stats::qt(ini[,i], df=dist.param[[i]][1])},
#                "7" = {ini[,i] <- stats::qweibull(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                "8" = {ini[,i] <- stats::qcauchy(ini[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                "9" = {ini[,i] <- stats::qbeta(ini[,i], dist.param[[i]][1],dist.param[[i]][2])}
#         )
#       }
#       if (!bd.flg){
#         switch(dist.ind[i],
#                "1" = {bd[i,] <- c(0,1);},
#                "2" = {bd[i,] <- c(-1e8,1e8);},
#                "3" = {bd[i,] <- c(0,1e8);},
#                "4" = {bd[i,] <- c(0,1e8);},
#                "5" = {bd[i,] <- c(0,1e8);},
#                "6" = {bd[i,] <- c(-1e8,1e8);},
#                "7" = {bd[i,] <- c(0,1e8);},
#                "8" = {bd[i,] <- c(-1e8,1e8);},
#                "9" = {bd[i,] <- c(0,1);}
#         )
#       }
#     }
# 
#     # Warm-start with SPs
#     if (warm.sp){
#       ini <- sp(n, p, ini=ini,
#                 dist.str=dist.str, dist.param=dist.param,
#                 dist.samp=dist.samp, scale.flg=scale.flg, bd=bd,
#                 num.subsamp=num.subsamp,
#                 tol=tol, par.flg=par.flg)$sp
#     }
# 
#     # Optimize via block L-BFGS
#     opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=tol)
#     des <- ini
# 
#     for (jj in 1:niter){
#       print(paste0("Iteration ",jj))
#       for (ii in 1:n){
# 
#         inipt <- des[ii,]
# 
#         # if (p==1){
#         #   dist.subsamp <- matrix(randtoolbox::sobol(num.subsamp,p,scrambling=T,seed=sample(1e6,1)),ncol=1)
#         #   
#         # }else{
#         #   # print(nseq)
#         #   dist.subsamp <- randtoolbox::sobol(num.subsamp,p,scrambling=T,seed=sample(1e6,1))
#         # }
#         dist.subsamp <- matrix(runif(num.subsamp*p),ncol=p)
#         
#         for (i in 1:p){
#           switch(dist.ind[i],
#                  "1" = {dist.subsamp[,i] <- stats::qunif(dist.subsamp[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                  "2" = {dist.subsamp[,i] <- stats::qnorm(dist.subsamp[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                  "3" = {dist.subsamp[,i] <- stats::qexp(dist.subsamp[,i], dist.param[[i]][1])},
#                  "4" = {dist.subsamp[,i] <- stats::qgamma(dist.subsamp[,i], shape=dist.param[[i]][1], scale=dist.param[[i]][2])},
#                  "5" = {dist.subsamp[,i] <- stats::qlnorm(dist.subsamp[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                  "6" = {dist.subsamp[,i] <- stats::qt(dist.subsamp[,i], df=dist.param[[i]][1])},
#                  "7" = {dist.subsamp[,i] <- stats::qweibull(dist.subsamp[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                  "8" = {dist.subsamp[,i] <- stats::qcauchy(dist.subsamp[,i], dist.param[[i]][1],dist.param[[i]][2])},
#                  "9" = {dist.subsamp[,i] <- stats::qbeta(dist.subsamp[,i], dist.param[[i]][1],dist.param[[i]][2])}
#           )
#         }
# 
#         if (kern.str=="spin"){
#           lambda <- kern.param[1]
#           nu <- kern.param[2]
#           res <- nloptr::nloptr( x0=inipt,
#                                  eval_f=function(xx){-pspobj_full(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  eval_grad_f=function(xx){-pspgrad_full(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  lb = bd[,1], ub = bd[,2],
#                                  opts=opts)
#         }else if (kern.str=="spin2"){
#           lambda <- kern.param[1]
#           nu <- kern.param[2]
#           res <- nloptr::nloptr( x0=inipt,
#                                  eval_f=function(xx){-pspobj_full2(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  eval_grad_f=function(xx){-pspgrad_full2(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  lb = bd[,1], ub = bd[,2],
#                                  opts=opts)
#         }else{
#           sigma <- kern.param[1]
#           res <- nloptr::nloptr( x0=inipt,
#                                  eval_f=function(xx){-herdobj_full(xx,des,ii,dist.subsamp,sigma)},
#                                  eval_grad_f=function(xx){-herdgrad_full(xx,des,ii,dist.subsamp,sigma)},
#                                  lb = bd[,1], ub = bd[,2],
#                                  opts=opts)
#         }
#         des[ii,] <- res$solution
# 
#       }
#     }
# 
#   }else{
#     #Standardize
#     if (scale.flg==T){
#       sdpts <- sqrt(apply(dist.samp,2,stats::var))
#       mmpts <- apply(dist.samp,2,mean)
#       dist.samp <- sweep(sweep(dist.samp,2,mmpts,"-"),2,sdpts,"/")
#     }
# 
#     #Set bounds
#     if (any(is.na(bd))){
#       bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
#       for (i in 1:p){
#         bd[i,] <- range(dist.samp[,i])
#       }
#     }
# 
#     #Jitter if repeats
#     if (any(duplicated(dist.samp))){
#       dist.samp <- jitter(dist.samp)
#       for (i in 1:p){ #enforce bounds
#         show(bd[i,])
#         dist.samp[,i] <- pmin(pmax(dist.samp[,i],bd[i,1]),bd[i,2])
#       }
#     }
# 
#     #Set initial point set
#     dist.ind <- c(NA)
#     dist.param <- list(NA)
#     if (any(is.na(ini))){
#       ini <- jitter(dist.samp[sample(1:nrow(dist.samp),n,F),])
#     }else{
#       ini <- sweep(sweep(ini,2,mmpts,"-"),2,sdpts,"/")
#     }
#     if (p==1){
#       ini <- matrix(ini,ncol=1)
#     }
# 
#     # Optimize via block L-BFGS
#     opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=tol)
#     des <- ini
#     for (jj in 1:niter){
#       print(paste0("Iteration ",jj))
#       for (ii in 1:n){
#         inipt <- des[ii,]
#         dist.subsamp <- dist.samp[sample.int(nrow(dist.samp),min(nrow(dist.samp),num.subsamp)),]
#         if (kern.str=="spin"){
#           lambda <- kern.param[1]
#           nu <- kern.param[2]
#           res <- nloptr::nloptr( x0=inipt,
#                                  eval_f=function(xx){-pspobj_full(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  eval_grad_f=function(xx){-pspgrad_full(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  lb = bd[,1], ub = bd[,2],
#                                  opts=opts)
#         }else if (kern.str=="spin2"){
#           lambda <- kern.param[1]
#           nu <- kern.param[2]
#           res <- nloptr::nloptr( x0=inipt,
#                                  eval_f=function(xx){-pspobj_full2(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  eval_grad_f=function(xx){-pspgrad_full2(xx,des,ii,dist.subsamp,lambda,nu)},
#                                  lb = bd[,1], ub = bd[,2],
#                                  opts=opts)
#         }else{
#             sigma <- kern.param[1]
#             res <- nloptr::nloptr( x0=inipt,
#                                    eval_f=function(xx){-herdobj_full(xx,des,ii,dist.subsamp,sigma)},
#                                    eval_grad_f=function(xx){-herdgrad_full(xx,des,ii,dist.subsamp,sigma)},
#                                    lb = bd[,1], ub = bd[,2],
#                                    opts=opts)
#         }
#         des[ii,] <- res$solution
#       }
#     }
# 
#     #Scale back
#     if (scale.flg==T){
#       ini <- sweep(sweep(ini,2,sdpts,"*"),2,mmpts,"+")
#       des <- sweep(sweep(des,2,sdpts,"*"),2,mmpts,"+")
#     }
# 
# 
#   }
# 
#   return(list(psp=des,ini=ini))
# }

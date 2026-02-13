# ###############################################################
# # Functions for kernel herding
# ###############################################################
# 
# herd <- function(n,p,sigma,tol=1e-3,x0=rep(0,pp),ini.des=NA){
#   #Kernel herding for Gaussian distribution
#   # x0 - starting point
#   # ini - initial design
# 
#   opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=tol)
#   pp <- p
#   nn <- n
# 
#   if (is.na(ini.des)){
#     des <- matrix(x0,ncol=pp)
#     for (i in 2:nn){
#       print(paste0("Herding: point ",i))
#       ini <- rnorm(pp)
#       res <- nloptr::nloptr( x0=ini,
#                              eval_f=function(xx){-herdobj(xx,des,sigma)},
#                              eval_grad_f=function(xx){-herdgrad(xx,des,sigma)},
#                              opts=opts)
#       des <- rbind(des,res$solution)
#     }
#   }else{
#     des <- ini.des
#     for (i in 2:nn){
#       print(paste0("Herding: point ",i))
#       ini <- rnorm(pp)
#       res <- nloptr::nloptr( x0=ini,
#                              eval_f=function(xx){-herdobj(xx,des,sigma)},
#                              eval_grad_f=function(xx){-herdgrad(xx,des,sigma)},
#                              opts=opts)
#       des <- rbind(des,res$solution)
#     }
#   }
# 
#   return(des)
# }
# 
# herd_thin <- function(n,sigma,dist.samp,tol=1e-10,ini.des=NA,num.subsamp=10000){
#   #Kernel herding from samples
#   # dist.samp - data to reduce
#   # ini - initial design
# 
#   opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=tol)
#   pp <- ncol(dist.samp)
#   nn <- n
# 
#   bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
#   for (i in 1:p){
#     bd[i,] <- range(dist.samp[,i])
#   }
# 
#   if (is.na(ini.des)){
#     x0 <- rep(0,pp) #Start at rep(0)
#     # x0 <- dist.samp[sample.int(nrow(dist.samp),1),]
#     des <- matrix(x0,ncol=pp)
#     for (i in 2:nn){
#       print(paste0("Herding: point ",i))
#       ini <- dist.samp[sample.int(nrow(dist.samp),1),]
#       dist.subsamp <- dist.samp[sample.int(nrow(dist.samp),min(nrow(dist.samp),num.subsamp)),]
#       res <- nloptr::nloptr( x0=ini,
#                              eval_f=function(xx){-herdobj_thin(xx,des,dist.subsamp,sigma)},
#                              eval_grad_f=function(xx){-herdgrad_thin(xx,des,dist.subsamp,sigma)},
#                              lb = bd[,1], ub = bd[,2],
#                              opts=opts)
#       des <- rbind(des,res$solution)
#     }
#   }else{
#     des <- ini.des
#     for (i in (nrow(ini.des)+1):nn){
#       print(paste0("Herding: point ",i))
#       ini <- dist.samp[sample.int(nrow(dist.samp),1),]
#       dist.subsamp <- dist.samp[sample.int(nrow(dist.samp),min(nrow(dist.samp),num.subsamp)),]
#       res <- nloptr::nloptr( x0=ini,
#                              eval_f=function(xx){-herdobj_thin(xx,des,dist.subsamp,sigma)},
#                              eval_grad_f=function(xx){-herdgrad_thin(xx,des,dist.subsamp,sigma)},
#                              lb = bd[,1], ub = bd[,2],
#                              opts=opts)
#       des <- rbind(des,res$solution)
#     }
#   }
# 
#   return(des)
# }
# 
# psp_seq <- function(n,lambda,nu,dist.samp,
#                      tol=1e-10,ini.des=NA,num.subsamp=10000,
#                      bd=NA){
#   #Kernel herding from samples
# 
#   opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=tol)
#   pp <- ncol(dist.samp)
#   nn <- n
# 
#   bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
#   for (i in 1:p){
#     bd[i,] <- range(dist.samp[,i])
#   }
# 
#   if (is.na(ini.des)){
#     # x0 <- rep(0.5,pp) #Start at rep(0)
#     x0 <- dist.samp[sample.int(nrow(dist.samp),1),]
#     des <- matrix(x0,ncol=pp)
#     for (i in 2:nn){
#       print(paste0("PSPs: point ",i))
#       # curbst <- NA
#       # curval <- 1e99
#       # for (ii in 1:nstart){
#         # # ini <- rep(0.5,pp)
#         ini <- dist.samp[sample.int(nrow(dist.samp),1),]
#         dist.subsamp <- dist.samp[sample.int(nrow(dist.samp),min(nrow(dist.samp),num.subsamp)),]
#         res <- nloptr::nloptr( x0=ini,
#                                eval_f=function(xx){-pspobj_seq(xx,des,dist.subsamp,lambda,nu)},
#                                eval_grad_f=function(xx){-pspgrad_seq(xx,des,dist.subsamp,lambda,nu)},
#                                lb = bd[,1], ub = bd[,2],
#                                opts=opts)
#         # if (curval > res$objective){
#         #   curval <- res$objective
#         #   curbst <- res$solution
#         # }
#       # }
# 
#       des <- rbind(des,res$solution)
#     }
#   }else{
#     des <- ini.des
#     for (i in (nrow(ini.des)+1):nn){
#       print(paste0("PSPs: point ",i))
#       ini <- dist.samp[sample.int(nrow(dist.samp),1),]
#       dist.subsamp <- dist.samp[sample.int(nrow(dist.samp),min(nrow(dist.samp),num.subsamp)),]
#       res <- nloptr::nloptr( x0=ini,
#                              eval_f=function(xx){-pspobj_thin(xx,des,dist.subsamp,lambda,nu)},
#                              eval_grad_f=function(xx){-herdgrad_thin(xx,des,dist.subsamp,sigma)},
#                              lb = bd[,1], ub = bd[,2],
#                              opts=opts)
#       des <- rbind(des,res$solution)
#     }
#   }
# 
#   return(des)
# }
# 
# psp_full <- function(n,lambda,nu,dist.samp,
#                      tol=1e-10,ini.des=NA,num.subsamp=10000,
#                      bd=NA,niter=100){
#   #Kernel herding from samples
# 
#   opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=tol)
#   pp <- ncol(dist.samp)
#   nn <- n
# 
#   bd <- matrix(NA,nrow=p,ncol=2,byrow=T)
#   for (i in 1:p){
#     bd[i,] <- range(dist.samp[,i])
#   }
# 
#   des <- dist.samp[sample.int(nrow(dist.samp),n),] #initialize
#   for (jj in 1:niter){
#     print(paste0("PSPs: iteration ",jj))
#     for (i in 1:nn){
#       ini <- des[i,]
#       dist.subsamp <- dist.samp[sample.int(nrow(dist.samp),min(nrow(dist.samp),num.subsamp)),]
#       res <- nloptr::nloptr( x0=ini,
#                              eval_f=function(xx){-pspobj_full(xx,des,i,dist.subsamp,lambda,nu)},
#                              eval_grad_f=function(xx){-pspgrad_full(xx,des,i,dist.subsamp,lambda,nu)},
#                              lb = bd[,1], ub = bd[,2],
#                              opts=opts)
#       des[i,] <- res$solution
#     }
#   }
# 
# 
#   return(des)
# }
# 
# 

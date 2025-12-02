## EntropyMCMC package
## High Performance Computing (HPC) versions of selected functions
## Multicore (mc) and snow versions of some package functions
## requires R version >= 2.15 and some packages, either
## parallel, or snow and Rmpi


############  cutTask()  ############
# spreading task in nbc // tasks, new version returning everything
# to split a task of size n into nbcores subtasks
# - if the n individual computations are identical, just call 
#   the original task loop nbcores times with size nc[j]'s  
# - if the individual tasks depend on n, call nbcores times
#   the original loop with the nseq[j] indices for core j
# nr = nb of repetitions of the global task
# nc = size of task per core (n/core)
# ni = indices for splitting task given in one dim of an array
# nseq = explicit 1st and last indices, somehow redundant with ni
#
# note: this function is somehow redundant with clusterSplit() provided
# by the parallel package for the snow cluster versions
# should be deprecated in the futur

# dec 2017: improved version, better re-distribution and load balancing
# nr = number of tasks to split
# value: nc = number of tasks per core
cutTask.mc <- function(nr, nbcores=detectCores()) {
  if (nr < nbcores) stop("More cores than tasks, cannot distribute")
  if (nr == nbcores) stop("Same number of cores and tasks, cannot simulate")
  k <- nr %/% nbcores # as floor; too small
  klast <- nr - (nbcores-1)*k  # remains due to floor truncation
  nc <- c(rep(k, nbcores-1), klast) # nb per core
  # then redistribute to first cores as needed
  j = 1 
  while (nc[nbcores] > k+1) {
    nc[j] <- nc[j] + 1
    nc[nbcores] <- nc[nbcores] - 1
    j <- j+1
  }
  if (sum(nc) != nr) {cat("warning")}
  ni <- c(1,cumsum(nc)+1) # indices for splitting arrays per cores	
  # launch in a loop and collect the list of children processes id's
  # build the n sequence/core boundaries
  nseq <- matrix(NA, nrow=nbcores, ncol=2)
  for (j in 1:nbcores) {
    nseq[j,1] <- ni[j]     # 1st index for jth core
    nseq[j,2] <- ni[j+1]-1 # last index for jth core
  }
  a = list(nc=nc, ni=ni, nseq=nseq)
  return(a)
}




############# MCMCcopies - multicore version ############
# mcmc_algo is the user-defined MCMC to study, which must be
# defined like the ones in Algo_defs.R, i.e. a list holding
# definitions for the required functions, such as
# mcmc_algo$chain(theta0, n, target, f_param, q_param)
# see RWHM_algo for an example; functions from the MCMCpack 
# package can also be called by complying to this definition
# d = space dimension of theta
# n = nb of (time) iterations of each single chain
# nmc = nb of iid copies
# Ptheta0 = (nmc,d) matrix of "P"arallel theta0 values
# output = object of class "plMCMC", in which
# Ptheta holds the nmc copies of chains in an 
# array(n,d,nmc) of simulated values
# where 1st value (1,d,nmc) is Ptheta0
# ToDo: snow and Rmpi cluster version
#
# update (dec2017): check if nmc is too small compared with nbcores
# so that at least one core have only 1 task to do, generate errors 

MCMCcopies.mc <- function(mcmc_algo, n=100, nmc=10, Ptheta0,
                          target, f_param, q_param, verb=TRUE,
                          nbcores=detectCores()){
  # require(parallel) # not a good practice, use parallel::function instead
  d <- dim(Ptheta0)[2]
  Ptheta <- array(NA,c(n,d,nmc))
  Ptheta[1,,] <- Ptheta0 # stores inits in output array
  pba <- 0 # overall chains proba of acceptation
  # loop over nb of iid chains
  tsk <- cutTask.mc(nmc,nbcores)  # splitting task/core
  if (min(tsk$nc) < 2) {
    cat("Number of parallel chaines", nmc, "too small, each core needs at least 2 chains\n")
    stop("use the single core version MCMCcopies() instead.")
  } else if (verb)	cat("multicore version: splitting",nmc,"chains between",nbcores,"cores.\n")
  # q=round(nmc/10); pc <- 0 # for verbose print of % done SINGLE CORE ONLY
  # if (q==0) q <- 1
  # lauch in a loop and collect the list of children processes id's
  # verb=F since no output required in parallel package
  pids <- rep(NA,nbcores)
  for (j in 1:nbcores) {
    # cut Ptheta0 in its subset for core j
    Ptheta0j <- matrix(NA, nrow=tsk$nc[j], ncol=d) # define the matrix (essayer <- NULL)!!
    Ptheta0j <- Ptheta0[tsk$nseq[j,1]:tsk$nseq[j,2],]
    pp <-  parallel::mcparallel(MCMCcopies(mcmc_algo, n, tsk$nc[j], Ptheta0j,
                                 target, f_param, q_param, verb=FALSE), 
                      mc.set.seed=TRUE)
    pids[j] <- pp$pid 
  }
  cr <- parallel::mccollect(pids) # collect all results from pid numbers
  # concatenates back all the elts in the return list
  pba <- rep(NA, nbcores) # for storing results
  for (j in 1:nbcores) {
    Ptheta[,,tsk$nseq[j,1]:tsk$nseq[j,2]] <- cr[[j]]$Ptheta
    pba[j] <- cr[[j]]$prob.accept  # average over this sub-run
  }
  return(structure(list(Ptheta=Ptheta, prob.accept=mean(pba),
                        algo=mcmc_algo$name, target=target,
                        f_param=f_param, q_param=q_param),
                   class="plMCMC"))
}

############# MCMCcopies - GENERAL CLUSTER VERSION  ############
# Simulation of the MCMC in parallel computing
# simulation of MCMC "in parallel" i.e. for each "slices of time" t
# list of input parameters matches that of MCMCcopies for consistency
#
# d = space dimension of theta
# n = length of (time) iterations of each single chain
# nmc = nb of iid copies
# Ptheta0 = (nmc,d) matrix of "P"arallel theta0 values
#
# mcmc_algo is the algorithm definition, with name, chain, step, etc
# Ptheta holds the nmc copies of chains in an 
# array(n,d,nmc) of simulated values
# where 1st value (1,d,nmc) is Ptheta0
########## GENERAL CLUSTER VERSION WITH cluster type ARGUMENT
# cltype = cluster type, 
# "PAR_SOCK" parallel socket cluster, the default for a single workstation
# "SNOW_SOCK" snow socket cluster
# "SNOW_RMPI" for snow MPI cluster with Rmpi library 
#             typically for an actual cluster

MCMCcopies.cl <- function(mcmc_algo, n=100, nmc=10, Ptheta0,
                          target, f_param, q_param, 
                          cltype="PAR_SOCK", nbnodes=4) {
  d <- dim(Ptheta0)[2];
  pba <- 0; # overall chains proba & # of acceptation, when meaningful
  Ptheta <- array(0,c(n,d,nmc)); Ptheta[1,,] <- Ptheta0 # stores inits in output array
  
  # cluster creation depending on cltype
  switch(cltype,
         PAR_SOCK = {  # parallel socket cluster  
           # require(parallel) # deprecated, see good practice 
           cat("starting parallel ")
           cl <- parallel::makeCluster(nbnodes)
           print(cl)
           # make objects visible for the nodes, envir option seems to work that way
           parallel::clusterExport(cl, c("target","f_param","q_param","mcmc_algo"),
                         envir=environment())
           }, 
         SNOW_SOCK = {   # snow socket cluster
           # require(snow)  # deprecated, see good practice
           # actually, see ?makeCluster in parallel, type SOCK is passed to snow
           # automatically from the function with same name
           cl <- snow::makeCluster(nbnodes, type="SOCK")
           cat("starting snow socket cluster with",nbnodes,"nodes\n")
           # make objects visible for the nodes, envir option seems to work that way
           snow::clusterExport(cl, c("target","f_param","q_param","mcmc_algo"),
                         envir=environment())
           }, 
         SNOW_RMPI = {
           # require(Rmpi); require(snow)
           cl <- snow::makeMPIcluster(nbnodes)
           # make objects visible for the nodes, envir option seems to work that way
           snow::clusterExport(cl, c("target","f_param","q_param","mcmc_algo"),
                         envir=environment())
           })
  
  
  # using parallel apply per row (R) of Ptheta0; CHECK with "::" package dependencies?
  ## loop over chain number
  upd <- parRapply(cl, Ptheta0, mcmc_algo$chain, 
                   n, target, f_param, q_param, 
                   mcmc_algo$q_pdf, mcmc_algo$q_proposal)
  # Collectinsg updates sent by nodes:
  # returs theta (nxd) and paccept
  for(k in 1:nmc){
    Ptheta[,,k] <- upd[[k]]$theta
    pba <- pba + upd[[k]]$paccept
  }

  pba <- pba/nmc
  stopCluster(cl); cat("cluster stopped.\n")
  
  return(structure(list(Ptheta=Ptheta, prob.accept=pba,
                        algo=mcmc_algo$name, target=target,
                        f_param=f_param, q_param=q_param),
                   class="plMCMC"))
}







######### EntropyMCMC - multicore version on subsequences in n ########
## for now first developped for the NN and kNN methods 
# requires an additional intermediate function called for each core
# to save time an avoiding building a full plMCMC object for each core
# mcc = object similar to Ptheta, ie array(n,d,nmc) 
# but holding only the subsequence of iterations computed by core j
# NB: not suitable for EntropyParallel usage
Entropy.NN.mc <- function(mcc, target, f_param) {
  n <- dim(mcc)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mcc)[2]    # space dimension of theta
  nmc <- dim(mcc)[3]  # nb of iid copies
  Kb <- Entp <- rep(NA,n)
  for (it in 1:n){
    if (d==1) x <- mcc[it,,] else x <- t(mcc[it,,])
    ep <- Entropy.NN(x, target, f_param)
    Kb[it] <- ep$Kb; Entp[it] <- ep$Entp		
  }
  return(list(Kb=Kb, Entp=Entp))
}

Entropy.kNN.mc <- function(mcc, target, f_param, k) {
  n <- dim(mcc)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mcc)[2]    # space dimension of theta
  nmc <- dim(mcc)[3]  # nb of iid copies
  Kb <- Entp <- matrix(NA,n,1)
  for (it in 1:n){
    if (d==1) x <- mcc[it,,] else x <- t(mcc[it,,])
    ep <- Entropy.kNN(x, target, f_param, k)
    Kb[it,1] <- ep$Kb; Entp[it,1] <- ep$Entp		
  }
  return(list(Kb=Kb, Entp=Entp))
}


Entropy.kNN.vect.mc <- function(mcc, target, f_param, k) {
  n <- dim(mcc)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mcc)[2]    # space dimension of theta
  nmc <- dim(mcc)[3]  # nb of iid copies
  Kb <- Entp <- matrix(NA,n,k)
  for (it in 1:n){
    if (d==1) x <- mcc[it,,] else x <- t(mcc[it,,])
    ep <- Entropy.kNN.vect(x, target, f_param, k)
    for(j in 1:k) {Kb[it,j] <- ep$Kb[j]; Entp[it,j] <- ep$Entp[j]}		
    
  }
  return(list(Kb=Kb, Entp=Entp))
}


# New May 2018: uselogtarget option added and passed to Entropy.ANN
Entropy.ANN.mc <- function(mcc, target, f_param, k, eps, uselogtarget, logtarget) {
  n <- dim(mcc)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mcc)[2]    # space dimension of theta
  nmc <- dim(mcc)[3]  # nb of iid copies
  Kb <- Entp <- matrix(NA,n,1)
  for (it in 1:n){
    if (d==1) x <- mcc[it,,] else x <- t(mcc[it,,])
    ep <- Entropy.ANN(x, target, f_param, k, eps, uselogtarget, logtarget)
    Kb[it,1] <- ep$Kb; Entp[it,1] <- ep$Entp		
  }
  return(list(Kb=Kb, Entp=Entp))
}


Entropy.ANN.vect.mc <- function(mcc, target, f_param, k, eps) {
  n <- dim(mcc)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mcc)[2]    # space dimension of theta
  nmc <- dim(mcc)[3]  # nb of iid copies
  Kb <- Entp <- matrix(NA,n,k)
  for (it in 1:n){
    if (d==1) x <- mcc[it,,] else x <- t(mcc[it,,])
    ep <- Entropy.ANN.vect(x, target, f_param, k, eps)
    for(j in 1:k) {Kb[it,j] <- ep$Kb[j]; Entp[it,j] <- ep$Entp[j]}		
    
  }
  return(list(Kb=Kb, Entp=Entp))
}


#################################
## NEW may 2018 (DC): add an option for using a logtarget() function 
# instead of computing log(target), that may return Inf when target(x) < 1.e-360 or so
# useful in eg product of gaussian-like targets with exp() functions
# add to .Rd
# uselogtarget: if FALSE, log(target(theta)) is computed directly from target(), 
# if FALSE, a specific function logtarget must be provided, which is used instead, to solve numerical problem
# generation of Inf's, and a bit faster
# ToDo: check if this is compatible with all.f ?
## IN TESTING PHASE, JUST FOR DEFAULT "A.Nearest.Neighbor" METHOD YET

EntropyMCMC.mc <- function(plmc1, # plmc2=NULL, 
                           method = "A.Nearest.Neighbor", k = 1, trim = 0.02, eps = 0,
                           all.f = TRUE, verb = FALSE, EntVect = FALSE, nbcores=detectCores(),
                           uselogtarget = FALSE, logtarget = NULL){
  
  # require(parallel) # cf NAMESPACE etc
  # if (method=="A.Nearest.Neighbor") require(RANN) # not neede since in depends
  if (EntVect == TRUE && k==1) {stop("Error: multinearest neighbour version not available for k < 2\n")}
  
  # retrieving parameters needed for all Entropy estimation methods
  mc1 <- plmc1$Ptheta # copies of MCMCs, first value is init theta0
  n <- dim(mc1)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mc1)[2]    # space dimension of theta
  nmc <- dim(mc1)[3]  # nb of iid copies
  Kb1 <- Entp <- rep(0,n) # for storing results
  if(EntVect == TRUE) {Kb1k <- Entpk <- matrix(0,n,k)} else {Kb1k <- Entpk <- matrix(0,n,1)}
  
  # q=round(n/10); if (q==0) q <- 1 # for verbose print of % done
  if (n < 2*nbcores) {stop("Error: multicore version not available for n < 2*nbcores\n")}
  
  if (method=="Nearest.Neighbor") {
    # loop over time iteration
    tsk <- cutTask.mc(n,nbcores)  # splitting nb iterations/core
    cat("multicore version: splitting",n,"iterations between",nbcores,"cores\n")
    #		cat(tsk$nc[1],"/core + ",tsk$nc[nbcores],")\n", sep="")
    pids <- rep(NA,nbcores)
    for (j in 1:nbcores) {
      # cut mc1 in its subset of iterations for core j
      # mcj <- array(NA, c(tsk$nc[j],d,nmc)) # definition needed when size change
      # or just set to NULL before?
      mcj <- NULL
      mcj <- mc1[tsk$nseq[j,1]:tsk$nseq[j,2],,]			
      pp <-  parallel::mcparallel(Entropy.NN.mc(mcj, plmc1$target, plmc1$f_param), 
                        mc.set.seed=TRUE)
      pids[j] <- pp$pid 
    }	
    cr <- parallel::mccollect(pids) # collect all results from pid numbers
    # concatenates back all the elts in the return list
    for (j in 1:nbcores) {
      Kb1[tsk$nseq[j,1]:tsk$nseq[j,2]] <- cr[[j]]$Kb
      Entp[tsk$nseq[j,1]:tsk$nseq[j,2]] <- cr[[j]]$Entp
    }
  }
  
  if (method=="k.Nearest.Neighbor") {
    # loop over time iteration
    tsk <- cutTask.mc(n,nbcores)  # splitting nb iterations/core
    cat("multicore version: splitting",n,"iterations between",nbcores,"cores\n")
    #		cat(tsk$nc[1],"/core + ",tsk$nc[nbcores],")\n", sep="")
    pids <- rep(NA,nbcores)
    for (j in 1:nbcores) {
      # cut mc1 in its subset of iterations for core j
      # mcj <- array(NA, c(tsk$nc[j],d,nmc)) # definition needed when size change
      # or just set to NULL before?
      mcj <- NULL
      mcj <- mc1[tsk$nseq[j,1]:tsk$nseq[j,2],,]			
      if(EntVect == TRUE) pp <-  parallel::mcparallel(Entropy.kNN.vect.mc(mcj, plmc1$target, plmc1$f_param, k), 
                                            mc.set.seed=TRUE)
      else                pp <-  parallel::mcparallel(Entropy.kNN.mc(mcj, plmc1$target, plmc1$f_param, k), 
                                            mc.set.seed=TRUE)
      pids[j] <- pp$pid 
    }	
    cr <- parallel::mccollect(pids) # collect all results from pid numbers
    # concatenates back all the elts in the return list
    for (j in 1:nbcores) {
      Kb1k[tsk$nseq[j,1]:tsk$nseq[j,2],] <- cr[[j]]$Kb
      Entpk[tsk$nseq[j,1]:tsk$nseq[j,2],] <- cr[[j]]$Entp
    }
  }
  
  if (method=="A.Nearest.Neighbor") {
    # loop over time iteration
    tsk <- cutTask.mc(n,nbcores)  # splitting nb iterations/core
    cat("multicore version: splitting",n,"iterations between",nbcores,"cores\n")
    #		cat(tsk$nc[1],"/core + ",tsk$nc[nbcores],")\n", sep="")
    pids <- rep(NA,nbcores)
    for (j in 1:nbcores) {
      # cut mc1 in its subset of iterations for core j
      # mcj <- array(NA, c(tsk$nc[j],d,nmc)) # definition needed when size change
      # or just set to NULL before?
      mcj <- NULL
      mcj <- mc1[tsk$nseq[j,1]:tsk$nseq[j,2],,]			
      if (EntVect == TRUE) pp <-  parallel::mcparallel(Entropy.ANN.vect.mc(mcj, plmc1$target, plmc1$f_param, k, eps), 
                                            mc.set.seed=TRUE) else {
                              pp <-  parallel::mcparallel(Entropy.ANN.mc(mcj, plmc1$target, plmc1$f_param, 
                                                                               k, eps, 
                                                                               uselogtarget, logtarget), 
                                            mc.set.seed=TRUE) }
      pids[j] <- pp$pid 
    }	
    cr <- parallel::mccollect(pids) # collect all results from pid numbers
    # concatenates back all the elts in the return list
    for (j in 1:nbcores) {
      Kb1k[tsk$nseq[j,1]:tsk$nseq[j,2],] <- cr[[j]]$Kb
      Entpk[tsk$nseq[j,1]:tsk$nseq[j,2],] <- cr[[j]]$Entp
    }
  }
  
  
  if (method != "Nearest.Neighbor" && method != "k.Nearest.Neighbor" && method != "A.Nearest.Neighbor") {
    cat("multicore version not imlemented yet for this method,\n")
    cat("running the single core version.\n")
    EntropyMCMC(plmc1, method, trim, all.f, verb)
  }
  # Output object	
  
  if (method=="k.Nearest.Neighbor" || method=="A.Nearest.Neighbor") {
    return(structure(list(Kullback=Kb1k, Entp=Entpk, nmc=nmc, dim=d,
                          algo=plmc1$algo, target = plmc1$target, method=method, k=k, eps=eps,
                          f_param=plmc1$f_param, q_param=plmc1$q_param),
                     class="KbMCMC"))
  }
  else{
    return(structure(list(Kullback=Kb1, Entp=Entp, nmc=nmc, dim=d,
                          algo=plmc1$algo, target=plmc1$target, method=method, k=k, 
                          f_param=plmc1$f_param, q_param=plmc1$q_param),
                     class="KbMCMC"))
  }
}	



################### EntropyParallel GENERAL CLUSTER VERSION ###################
# Simulation of the MCMC and Entropy computation in parallel computing
# simulation of MCMC "in parallel" i.e. for each "slices of time" t
# the next step for each copy of the nmc chains are generated,
# then the Entropy is computed, and the past is forgotten
# so probably only non-adaptive Markov chains or adaptive chains for which
# the past can be summarized by some sufficient statistics will be eligible 
# NB the option keep.all removed for snow version: no storing of the
# entire array of simulations (n,d,nmc)
# list of input parameters matches that of MCMCcopies for consistency
#
# d = space dimension of theta
# n = length of (time) iterations of each single chain
# nmc = nb of iid copies
# Ptheta0 = (nmc,d) matrix of "P"arallel theta0 values
#
# mcmc_algo is the algorithm definition, with name, chain, step, etc

########## GENERAL CLUSTER VERSION WITH cluster type ARGUMENT
# cltype = cluster type, 
# "PAR_SOCK" parallel socket cluster, the default for a single workstation
# "SNOW_SOCK" snow socket cluster
# "SNOW_RMPI" for snow MPI cluster with Rmpi library 
#             typically for an actual cluster
# Another difference is that for this version, entropy and Kullback estimates
# are computed without calling Entropy.NN, to avoid passing array etc, and 
# if parameter par.logf = TRUE then computation of log(target(theta_i))
# for i=1..nmc is sent to the available nodes using parRapply()
# 2015: add the return of the last slice time n, Stheta (nmc,d), for PCA strategy
# 2018: add the options uselogtarget = FALSE, logtarget = NULL for specific target defs
#  yet only for "A.Nearest.Neighbor" method

EntropyParallel.cl <- function(mcmc_algo, n = 100, nmc = 10, Ptheta0,
                               target, f_param, q_param, 
                               method = "A.Nearest.Neighbor", k = 1, eps = 0, 
                               trim = 0.02, verb = TRUE,
                               EntVect = FALSE, cltype="PAR_SOCK", nbnodes = 4, 
                               par.logf = FALSE,
                               uselogtarget = FALSE, logtarget = NULL) {
  if (EntVect == TRUE && k==1) {stop("Error: multinearest neighbour version not available for k < 2\n")}
  # if (method=="A.Nearest.Neighbor") require(RANN) # in Depends now
  d <- dim(Ptheta0)[2]
  # constants for NN estimation
  # CEuler <- 0.57721566490153286 # Euler Cte int_R^+ exp(-u)log(u) du
  #  not used anymore: defined in the C code
  # logc1 <- log(pi^(d/2)/gamma(d/2 + 1)) # first version
  logc1 <- (d/2)*log(pi) - lgamma(d/2 + 1) # lgamma better than log(gamma())
  
  if (method != "Nearest.Neighbor" && method != "k.Nearest.Neighbor"  && method != "A.Nearest.Neighbor") {
    cat("only method = Nearest.Neighbor, method = k.Nearest.Neighbor and method = A.Nearest.Neighbor are implemented,\n")
    cat("running with that method.\n")
    method <- "k.Nearest.Neighbor"
  }
  q=round(n/10); pc <- 0 # for verbose print of % done over n
  if (q==0) q <- 1
  if (n < 20 && verb) {
    cat("verbose mode disabled for n<20 iterations.\n")
    verb <- FALSE}		
  Stheta <- Ptheta0	# "Slice" of theta's at any time t (nmc,d)
  Kb1 <- rep(0,n); Entp <- rep(0,n) # for storing entropy results
  if(EntVect == TRUE) {Kb1k <- Entpk <- matrix(0,n,k)} # for storing results
  else {Kb1k <- Entpk <- matrix(0,n,1)}
  pba <- nba <- 0 # overall chains proba & # of acceptation, when meaningful
  
  # cluster creation depending on cltype	
  switch(cltype,
         PAR_SOCK = {
           # require(parallel) # in NAMESPACE
           cat("starting parallel ")
           cl <- parallel::makeCluster(nbnodes)
           print(cl)
           parallel::clusterExport(cl, c("target","logtarget","f_param","q_param","mcmc_algo"),
                         envir=environment()) # make objects visible for the nodes
           }, # socket cluster under parallel
         SNOW_SOCK = {
           # require(snow)
           cat("starting snow socket cluster with",nbnodes,"nodes\n")
           cl <- snow::makeCluster(nbnodes, type="SOCK")
           # make objects visible for the nodes, envir option seems to work that way
           snow::clusterExport(cl, c("target","logtarget","f_param","q_param","mcmc_algo"),
                         envir=environment())
           }, # snow socket cluster
         SNOW_RMPI = {
           # require(Rmpi); require(snow) # see NAMESPACE
           cat("starting snow MPI cluster:\n")
           cl <- snow::makeMPIcluster(nbnodes)
           # make objects visible for the nodes, envir option seems to work that way
           snow::clusterExport(cl, c("target","logtarget","f_param","q_param","mcmc_algo"),
                         envir=environment())
           })
  # definition for computing log(target) in parallel		
  if ((par.logf) & (uselogtarget == FALSE))
      logtarget <- function(x, param) {log(target(x,param))} # CAUTION! side effect with new logtarget def?
  
  ### time-dependent loop over time iterations
  for (it in 1:n) {	
    # call SNOW cluster on these independent nmc processes!!
    # using parallel apply per row (R) of Stheta
    ## time-dependent loop over time iterations
    upd <- parRapply(cl, Stheta, mcmc_algo$step,
                     target,
                     mcmc_algo$q_pdf, mcmc_algo$q_proposal,
                     f_param, q_param, nba)
    
    # collecting updates sent by nodes: needed since mcmc_algo$step
    # returns itself a list with update and nba				
    for (i in 1:nmc) { 
      Stheta[i,] <- upd[[i]]$theta_new # update for the ith chain
      nba <- nba + upd[[i]]$nba  # ??? check that!
    }
    ## Entropy & Kullback estimation on "slice it" Stheta
    
    if (method == "Nearest.Neighbor"){
      # C call for computing NN entropy with C loops
      # add PACKAGE="" option when moving to package; not needed?
      z <- .C("entropyNNC", as.integer(nmc), as.integer(d),
              x=as.double(Stheta), logc1=as.double(logc1), result=double(1))
      Entp[it] <- z$result
      # computing E_{p}(log f) single or parallel version
      if (!par.logf) REnt <- mean(log(target(Stheta, f_param))) else {
        REcl <- parRapply(cl, Stheta, logtarget, f_param)
        REnt <- mean(REcl)}
      Kb1[it] <- Entp[it] - REnt
    }
    
    if (method == "k.Nearest.Neighbor"){
      if(EntVect == TRUE) ep <- Entropy.kNN.vect(Stheta, target, f_param, k)
      else  ep <- Entropy.kNN(Stheta, target, f_param, k)
      Kb1k[it,] <- ep$Kb; Entpk[it,] <- ep$Entp
    }
    
    if (method == "A.Nearest.Neighbor"){
      if(EntVect == TRUE) ep <- Entropy.ANN.vect(Stheta, target, f_param, k, eps = eps)
      else  ep <- Entropy.ANN(Stheta, target, f_param, k, eps = eps,
                              uselogtarget = uselogtarget, logtarget = logtarget)
      Kb1k[it,] <- ep$Kb; Entpk[it,] <- ep$Entp
    }
    
    # % done printout
    b=round(it/q); r=it-b*q
    if (r==0 & verb) {
      pc <- pc+10; 
      if (EntVect == FALSE) cat(pc,"% done, Kullback at ", k, "th Nearest Neighbor = ", Kb1k[it,],"\n")
      else  cat(pc,"% done, Kullback at ", k, "th Nearest Neighbor = ", Kb1k[it,k],"\n")}
  } # time iteration loop
  
  pba <- nba/(n*nmc)
  stopCluster(cl); cat("cluster stopped.\n")
  # Output object	of class KbMCMC like EntropyMCMC
  # with Ptheta set to NULL since keep.all forced to FALSE
  
  if (method == "k.Nearest.Neighbor" || method == "A.Nearest.Neighbor"){
    return(structure(list(Kullback=Kb1k, Entp=Entpk, nmc=nmc, dim=d,
                          algo=mcmc_algo$name, target=target, method=method, k=k, eps=eps,
                          f_param=f_param, q_param=q_param, prob.accept=pba,
                          Ptheta=NULL),
                     class="KbMCMC"))
  }
  
  
  else{
    return(structure(list(Kullback=Kb1, Entp=Entp, nmc=nmc, dim=d,
                          algo=mcmc_algo$name, target=target, method=method,
                          f_param=f_param, q_param=q_param, prob.accept=pba,
                          nbnodes=nbnodes, lastTheta=Stheta, Ptheta=NULL),
                     class="KbMCMC")) 
  }
}




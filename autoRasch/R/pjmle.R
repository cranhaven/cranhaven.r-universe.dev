# Penalized Joint Maximum likelihood Estimation
#
# Estimate the theta, beta and gamma parameter of the full dataset
# X is the dataset which is want to be estimated
# init_par is a vector contains the initial values of the estimated parameters
# setting contains the parameter setting used  in the estimation. See autoRaschOptions().

pjmle <- function(X, init_par = c(), setting = c(), method = c("fast","novel")){

  dset <- as.data.frame(X)                          ### makes sure that the dataset has a matrix format
  dset <- downCoding(dset)

  if(is.null(setting)){
    opts <- autoRaschOptions()
  } else {
    opts <- setting
  }

  oriGroupsMap <- opts$groups_map
  if(!is.null(opts$groups_map)){
    exclRespIdx <- which(is.na(opts$groups_map[,1]))
    if(length(exclRespIdx) != 0){
      dset <- as.matrix(dset[-c(exclRespIdx),])
      opts$groups_map <- opts$groups_map[-c(exclRespIdx),]
    }
  } else {
    exclRespIdx <- NULL
  }

  ### preprocessing the data
  dataPrep <- data_prep(dset = dset, fixed_par = opts$fixed_par, groups_map = opts$groups_map, method = method)

  ### Intializing the parameters ###
  if(opts$randomized){
    theta <- runif(nrow(dataPrep$dset),-1,1)*opts$random.init.th
    beta <- runif(dataPrep$allcat,-1,1)*opts$random.init.th
    gamma <- runif(ncol(dataPrep$dset),-1,1)*opts$random.init.th
    if(opts$mode == "DIF"){
      delta <- rep(0,(ncol(dataPrep$dset)*ncol(dataPrep$groups_map)))
    } else {
      delta <- rep(0,(dataPrep$allcat*ncol(dataPrep$groups_map)))
    }
  } else {
    theta <- rep(0,nrow(dataPrep$dset))
    beta <- rep(0,dataPrep$allcat)
    gamma <- rep(0,ncol(dataPrep$dset)) #gpcm uses different gamma for each item
    if(opts$mode == "DIF"){
      delta <- rep(0,(ncol(dataPrep$dset)*ncol(dataPrep$groups_map)))
    } else {
      delta <- rep(0,(dataPrep$allcat*ncol(dataPrep$groups_map)))
    }
  }

  ### setting the optimized parameter
  nlmPar <- c(theta,beta,gamma,delta)
  length_theta <- length(theta)
  length_beta <- length(beta)
  length_gamma <- length(gamma)
  length_delta <- length(delta)
  length_array <- c(length_theta,length_beta,length_gamma,length_delta)
  fullPar_arr <- c("theta","beta","gamma","delta")


  if(is.null(opts$fixed_par)){
    estPar_arr <- fullPar_arr
  } else {
    estPar_arr <- fullPar_arr[-c(which(fullPar_arr %in% opts$fixed_par))]
  }
  fixLength_arr <- length_array[c(which(fullPar_arr %in% opts$fixed_par))]

  fixValue <- c()
  estLength_array <- length_array

  if(!identical(grep("delta",opts$fixed_par), integer(0))){
    nlmPar <- nlmPar[-c((sum(length_array[1:3])+1):(sum(length_array[1:4])))]
    estLength_array <- estLength_array[-c(4)]
    if(!is.null(opts$fixed_delta)){
      fixValue <- c(opts$fixed_delta,fixValue)
    } else {
      fixValue <- c(rep(0,(ncol(dset)*ncol(dataPrep$groups_map))),fixValue)
    }
  }
  if(!identical(grep("^gamma",opts$fixed_par), integer(0))){
    if(length(nlmPar) == length(length_array)){
      nlmPar <- nlmPar[-c((sum(length_array[1:2])+1):(sum(length_array[1:3])),(sum(length_array[1:4])+1):(sum(length_array[1:5])))]
      estLength_array <- estLength_array[-c(3,5)]
      if(!is.null(opts$fixed_gamma)){
        fixValue <- c(opts$fixed_gamma, rep(0,(ncol(dset)*ncol(dataPrep$groups_map))), fixValue)
      } else {
        fixValue <- c(rep(0,ncol(dset)), rep(0,(ncol(dset)*ncol(dataPrep$groups_map))), fixValue)
      }
    } else {
      nlmPar <- nlmPar[-c((sum(length_array[1:2])+1):(sum(length_array[1:3])))]
      estLength_array <- estLength_array[-c(3)]
      if(!is.null(opts$fixed_gamma)){
        fixValue <- c(opts$fixed_gamma, fixValue)
        # print(opts$fixed_gamma)
      } else {
        fixValue <- c(rep(0,ncol(dset)), fixValue)
      }
    }
  }
  if(!identical(grep("^beta",opts$fixed_par), integer(0))){
    nlmPar <- nlmPar[-c((sum(length_array[1])+1):(sum(length_array[1:2])))]
    estLength_array <- estLength_array[-c(2)]
    fixValue <- c(opts$fixed_beta, fixValue)
  }
  if(!identical(grep("theta",opts$fixed_par), integer(0))){
    nlmPar <- nlmPar[-c((1):(sum(length_array[1])))]
    estLength_array <- estLength_array[-c(1)]
    fixValue <- c(opts$fixed_theta, fixValue)
  }


  if(!is.null(init_par)) {
    nlmPar <- init_par
  }

  ### Maximizing the loglikelihood function ###
  nameCol <- colnames(as.data.frame(X))
  exclResp <- NULL
  if(length(exclRespIdx) != 0){
    # totalResp <- c(1:nrow(X))
    # rownames(dset) <- totalResp[-c(exclRespIdx)]
    exclResp <- exclRespIdx
  }
  output <- list("X" = X, "mt_vek" = dataPrep$mt_vek, "real_vek" = dataPrep$na_catVec, "itemName" = nameCol,
                 "penalty.coeff" = list("lambda_theta" = opts$lambda_theta,"lambda_in" = opts$lambda_in,
                                        "lambda_out" = opts$lambda_out,"lambda_delta" = opts$lambda_delta),
                 "exclResp" = exclResp, "mode" = opts$mode)

  if(!is.null(opts$groups_map)){
    output[["groups_map"]] <- oriGroupsMap
  }

  if(opts$isTraced){
    print("Estimation starts...")
  }

  if(opts$optz_method == "optim"){
    if(method[1] == "novel"){
      ll_fun <- loglik_fun
      gr_fun <- grad_fun
    } else {
      ll_fun <- loglik_fun_fast
      gr_fun <- grad_fun_fast
    }

    (minRes <- optim(nlmPar, ll_fun, gr = gr_fun, hessian = opts$isHessian, dset = dataPrep$dset,
                     estPar_arr = estPar_arr, estLength_array = estLength_array, dataPrep = dataPrep, opts = opts,
                     fixLength_arr = fixLength_arr, fixValue = fixValue, fixed_par = opts$fixed_par,
                     method = "BFGS", control = list(maxit = opts$optim_control$maxit, reltol =  opts$optim_control$reltol,
                                                     fnscale = opts$optim_control$fnscale)))

    est <- minRes$par
    output[["loglik"]] <- -minRes$value
  } else if(opts$optz_method == "mixed"){
    (minRes <- mixed.min(nlmPar, dset = dset, dataPrep = dataPrep, opts = opts, estPar_arr = estPar_arr,
                         estLength_array = estLength_array, fixLength_arr = fixLength_arr,
                         length_array = length_array, fixValue = fixValue, method = method))

    est <- minRes$par
    output[["loglik"]] <- -minRes$value
  }

  if(opts$isHessian){
    output[["hessian"]] <- minRes$hessian
  }

  if(opts$isTraced){
    print("...done!")
  }


  ### parsing the parameters estimate
  checkdata <- c("delta","gamma","beta","theta")

  for(cd in checkdata){
    if(!identical(grep(cd,estPar_arr), integer(0))){
      parNo <- grep(cd,estPar_arr)
      if(parNo == 1){
        output[[cd]] <- est[c((1):(sum(estLength_array[1])))]
      } else {
        output[[cd]] <- est[c((sum(estLength_array[1:(parNo-1)])+1):(sum(estLength_array[1:parNo])))]
      }
      if(method[1] == "fast" & cd == "beta"){
        output[["beta.raw"]] <- output[[cd]]
        output[[cd]] <- output[[cd]]*dataPrep$na_catVec
      }
      if(method[1] == "fast" & cd == "delta" & opts$mode == "DSF"){
        output[["delta.raw"]] <- output[[cd]]
        output[[cd]] <- output[[cd]]*dataPrep$na_catVec
      }
    }
  }

  return(output)

}

data_prep <- function(dset, fixed_par, groups_map, method){

  if(!is.null(groups_map)){
    groups_map <- as.matrix(groups_map)
    if(nrow(groups_map) != nrow(dset)){
      stop("autoRasch ERROR: `groups_map` must be has the same number of rows with observation.")
    }
    if(max(groups_map, na.rm = TRUE) > 1 | min(groups_map, na.rm = TRUE) < 0 | length(levels(factor(unlist(groups_map)))) > 2){
      stop("autoRasch ERROR: `groups_map` must be binary matrix with only 1 and 0 values.")
    }
  } else {
    groups_map <- matrix(0,nrow = nrow(dset),ncol = 1)
  }

  ### Manages catagory's vector
  mt_vek <- apply(dset, 2L, max, na.rm = TRUE)      ### create vector of max categories for each item
  n_th <- max(mt_vek)                               ### number of thresholds
  mt_vek_min <- apply(dset, 2L, min, na.rm = TRUE)  ### create vector of min categories for each item
  true_catVec <- (mt_vek-n_th)*(-1)
  if(n_th != 1){
    na_catVec <- as.vector(apply(rbind(mt_vek_min,true_catVec),2,function(x){     ### To remove the unavailable categories if items contain different number of categories (used for output filter)
      temp <- c(rep(NA,x[1]),rep(1,(n_th-sum(x))),rep(NA,x[2]))
      return(temp)
    }))
  } else {
    na_catVec <- mt_vek
  }

  if(method[1] == "fast"){
    mt_vek <- rep(max(dset,na.rm = TRUE),ncol(dset))
  }
  allcat <- sum(mt_vek)                             ### number of items * categories (assumption : item has the same number of categories)

  mt_idx <- rep(seq_along(mt_vek), mt_vek)
  dimResp <- dim(dset)

  ### Create the xn.mat (matrix of x_ni) and the xna.mat (matrix of missing value) to map the responses and the missing values
  ### XNA is infused to XN
  if(method[1] == "novel"){
      xn.vec <- xna.vec <- c()
      for(rIdx in 1:dimResp[1]){
        for(cIdx in 1:dimResp[2]){
          if(is.na(dset[rIdx,cIdx])){
            xn.cell <- xna.cell <- rep(NA,mt_vek[cIdx])
          } else {
            xn.cell <- c(rep(1,dset[rIdx,cIdx]),rep(0,(mt_vek[cIdx]-dset[rIdx,cIdx])))
            xna.cell <- rep(1,(mt_vek[cIdx]))
          }
          xn.vec <- c(xn.vec,xn.cell)
          xna.vec <- c(xna.vec,xna.cell)
        }
      }
      xn.mat <- matrix(xn.vec, nrow = dimResp[1], byrow = TRUE)
      xna.mat <- matrix(xna.vec, nrow = dimResp[1], byrow = TRUE)

      XN <- as.vector(t(xn.mat))
      XNA <- as.vector(t(xna.mat))

      ret <- list("dset" = dset, "mt_vek" = mt_vek, "mt_idx" = mt_idx, "dimResp" = dimResp, "groups_map" = groups_map, "n_th" = n_th, "na_catVec" = na_catVec, "allcat" = allcat, "XN" = XN, "XNA" = XNA)#,"XREAL" = XREAL)

  } else {
    ret <- list("dset" = dset, "mt_vek" = mt_vek, "mt_idx" = mt_idx, "dimResp" = dimResp, "groups_map" = groups_map, "n_th" = n_th, "na_catVec" = na_catVec, "allcat" = allcat)#, "XN" = XN, "XNA" = XNA)#,"XREAL" = XREAL)
  }

  return(ret)

}


### Rescores the dataset to have a minimum score of 0 and removes unused categories by re-codes the higher categories
downCoding <- function(X){

  datebeforecheck <- X
  nitem <- ncol(X)
  dataaftercheck <- c()

  for(i in 1:nitem){
    tempResp <- datebeforecheck[,i]
    maxResp <- max(tempResp,na.rm = TRUE)
    minResp <- min(tempResp,na.rm = TRUE)
    nfactor <- factor(tempResp)
    catLevel <- levels(nfactor)
    tempCatLevel <- catLevel
    for(j in seq_along(catLevel)) {
      tempResp[which(tempResp >= as.numeric(catLevel[j]))] <- tempResp[which(tempResp >= as.numeric(catLevel[j]))] - (as.numeric(catLevel[j]) - (j-1))
      tempnfactor <- factor(tempResp)
      tempCatLevel <- levels(tempnfactor)
      nfactor <- factor(tempResp)
      catLevel <- levels(nfactor)
    }
    dataaftercheck <- cbind(dataaftercheck,tempResp)
  }

  colnames(dataaftercheck) <- paste("V",c(1:nitem))
  return(dataaftercheck)
}

coord.descent <- function(nlmPar, dset, dataPrep, opts, fixed_par = c(), step.vec = NULL,
                               estPar_arr = c(), estLength_array = c(), fixLength_arr = c(), fixValue = c(), method){

  nlmpar.new <- nlmPar

  if(method[1] == "novel"){
    ll_fun <- loglik_fun
  } else {
    ll_fun <- loglik_fun_fast
  }

  n.par.idx <- seq_along(nlmPar)

  if(is.null(step.vec)){
    step.vec <- rep(opts$cd_control$init.step,length(nlmPar))
  }

  opts$isPenalized_delta <- TRUE

  delta.vector <- c()
  for(n.par in n.par.idx){
    stepsize <- step.vec[n.par]

    ll.val.old <- ll_fun(nlmpar.new, dset = dataPrep$dset, estPar_arr = estPar_arr, estLength_array = estLength_array,
                         fixLength_arr = fixLength_arr, fixed_par = fixed_par, fixValue = fixValue, opts = opts,
                         dataPrep = dataPrep)

    ll.val.baseline <- ll.val.old
    nlmpar.old <- nlmpar.newminus <- nlmpar.newplus <- nlmpar.new

    nlmpar.newminus[n.par] <- nlmpar.old[n.par] - stepsize
    nlmpar.newplus[n.par] <- nlmpar.old[n.par] - (-stepsize)

    ll.val.newminus <- ll_fun(nlmpar.newminus, dset = dataPrep$dset, estPar_arr = estPar_arr, estLength_array = estLength_array,
                              fixLength_arr = fixLength_arr, fixed_par = fixed_par, fixValue = fixValue, opts = opts,
                              dataPrep = dataPrep)
    ll.val.newplus <- ll_fun(nlmpar.newplus, dset = dataPrep$dset, estPar_arr = estPar_arr, estLength_array = estLength_array,
                             fixLength_arr = fixLength_arr, fixed_par = fixed_par, fixValue = fixValue, opts = opts,
                             dataPrep = dataPrep)

    if(ll.val.newminus < ll.val.old){
      stepsize <- stepsize
      ll.val.new <- ll.val.newminus
      nlmpar.new[n.par] <- nlmpar.newminus[n.par]
    } else if(ll.val.newplus < ll.val.old){
      stepsize <- -stepsize
      ll.val.new <- ll.val.newplus
      nlmpar.new[n.par] <- nlmpar.newplus[n.par]
    } else {
      stepsize <- 0
      step.vec[n.par] <- step.vec[n.par]*opts$cd_control$scale.down
      ll.val.new <- ll.val.old
    }


    i <- 0
    if(stepsize == 0){
    } else {
      i <- 1
      while((diff <- abs(abs(ll.val.new) - abs(ll.val.old))) > opts$cd_control$abs.tol & ll.val.new < ll.val.old & (i < opts$cd_control$maxit.cd.lower)){
        nlmpar.old<- nlmpar.new
        ll.val.old <- ll.val.new
        nlmpar.new[n.par] <- nlmpar.old[n.par] - stepsize

        ll.val.new <- ll_fun(nlmpar.new, dset = dataPrep$dset, estPar_arr = estPar_arr, estLength_array = estLength_array,
                             fixLength_arr = fixLength_arr, fixed_par = fixed_par, fixValue = fixValue, opts = opts,
                             dataPrep = dataPrep)

        diff <- abs(ll.val.new) - abs(ll.val.old)

        if(diff > 0){
          nlmpar.new[n.par] <- nlmpar.new[n.par] + stepsize
          stepsize <- 0
        }

        i <- i+1
      }

    }
  }

  return(list("value" = ll.val.new, "par" = nlmpar.new, iterations = i, step.vec = step.vec))

}

mixed.min <- function(nlmPar, dset, dataPrep, opts,
                           estPar_arr = NULL, estLength_array = NULL, fixLength_arr = NULL, fixValue = c(),
                           length_array = NULL, method){

  if(method[1] == "novel"){
    ll_fun <- loglik_fun
    gr_fun <- grad_fun
  } else {
    ll_fun <- loglik_fun_fast
    gr_fun <- grad_fun_fast
  }

  ll.val.old <- 1e+10
  ll.value <- c()

  par.diff <- c()

  par.arr <- c("theta", "beta","gamma","delta")
  par.fix.idx <-  which(par.arr %in% opts$fixed_par)

  fixed_par_gpcm <- unique(c(par.arr[par.fix.idx],"delta"))
  estPar_arr_gpcm <- par.arr[-c(which(par.arr %in% fixed_par_gpcm))]

  fixLength_arr_gpcm <- length_array[c(which(par.arr %in% fixed_par_gpcm),5)]
  estLength_array_gpcm <- length_array[which(par.arr %in% estPar_arr_gpcm)]

  fixed_par_gpcmdif <- c("theta","beta","gamma")
  estPar_arr_gpcmdif <- c("delta")
  fixLength_arr_gpcmdif <- length_array[c(1:3,5)]
  estLength_array_gpcmdif <- length_array[4]

  par.old <- c(nlmPar)
  fixed_delta <- nlmPar[(sum(estLength_array_gpcm)+1):(sum(estLength_array_gpcm)+estLength_array_gpcmdif)]

  nlmPar <- nlmPar[1:sum(estLength_array_gpcm)]

  fixValue <- c(fixed_delta)
  if(length(which("gamma" %in% fixed_par_gpcm)) != 0){
    fixValue <- c(rep(0,(dataPrep$dimResp[2])),fixValue)
  }
  if(!is.null(opts$fixed_beta)){
    fixValue <- c(opts$fixed_beta,fixValue)
  }
  if(!is.null(opts$fixed_theta)){
    fixValue <- c(opts$fixed_theta,fixValue)
  }

  opts$isPenalized_delta <- FALSE
  (gpcm <- optim(nlmPar, ll_fun, gr = gr_fun, hessian = opts$isHessian, dset = dataPrep$dset,
                 estPar_arr = estPar_arr_gpcm, estLength_array = estLength_array_gpcm, fixed_par = fixed_par_gpcm,
                 fixLength_arr = fixLength_arr_gpcm, fixValue = fixValue, opts = opts, dataPrep = dataPrep,
                 method = "BFGS", control = opts$optim_control))

  if(length(which("gamma" %in% fixed_par_gpcm)) != 0){
    fixValue <- c(opts$fixed_theta,gpcm$par,rep(0,(dataPrep$dimResp[2])))
  } else {
    fixValue <- c(opts$fixed_theta,gpcm$par)
  }

  nlmPar <- fixed_delta

  (gpcmdif <- coord.descent(nlmPar, dset = dset, dataPrep = dataPrep, opts = opts, fixed_par = fixed_par_gpcmdif,
                                 fixValue = fixValue, estPar_arr = estPar_arr_gpcmdif, estLength_array = estLength_array_gpcmdif,
                                 fixLength_arr = fixLength_arr_gpcmdif, method = method))


  ll.val.new <- gpcmdif$value
  par.new <- c(gpcm$par,gpcmdif$par)

  i <- 0

  while((diff <- (abs(abs(ll.val.new) - abs(ll.val.old)))) > opts$cd_control$abs.tol & i < opts$cd_control$maxit.cd.higher &
        ((diff.par <- (max(abs(abs(par.new) - abs(par.old))))) > opts$cd_control$max.diff.par)){
    ll.val.old <- ll.val.new
    par.old <- par.new


    nlmPar <- gpcm$par
    if(is.null(opts$fixed_theta)){
      if(length(which("gamma" %in% fixed_par_gpcm)) != 0){
        fixValue <- c(rep(0,(dataPrep$dimResp[2])),gpcmdif$par)
      } else {
        fixValue <- c(gpcmdif$par)
      }
    } else {
      if(length(which("gamma" %in% fixed_par_gpcm)) != 0){
        fixValue <- c(opts$fixed_theta,rep(0,(dataPrep$dimResp[2])),gpcmdif$par)
      } else {
        fixValue <- c(opts$fixed_theta,gpcmdif$par)
      }
    }

    opts$isPenalized_delta <- FALSE
    (gpcm <- optim(nlmPar, ll_fun, gr = gr_fun, hessian = opts$isHessian, dset = dataPrep$dset,
                   estPar_arr = estPar_arr_gpcm, estLength_array = estLength_array_gpcm, fixed_par = fixed_par_gpcm,
                   fixLength_arr = fixLength_arr_gpcm, fixValue = fixValue, opts = opts, dataPrep = dataPrep,
                   method = "BFGS", control = opts$optim_control))

    if(length(which("gamma" %in% fixed_par_gpcm)) != 0){
      fixValue <- c(opts$fixed_theta,gpcm$par,rep(0,(dataPrep$dimResp[2])))
    } else {
      fixValue <- c(opts$fixed_theta,gpcm$par)
    }
    nlmPar <- gpcmdif$par


    (gpcmdif <- coord.descent(nlmPar, dset = dset, dataPrep = dataPrep, opts = opts, fixed_par = fixed_par_gpcmdif,
                                   fixValue = fixValue, estPar_arr = estPar_arr_gpcmdif, estLength_array = estLength_array_gpcmdif,
                                   fixLength_arr = fixLength_arr_gpcmdif, step.vec = gpcmdif$step.vec, method = method))



    ll.val.new <- gpcmdif$value
    par.new <- c(gpcm$par,gpcmdif$par)
    # par.diff <- c(par.diff, mean(abs(par.new - par.old)))
    # par.max <- c(par.max, max(abs(abs(par.new) - abs(par.old))))
    ll.value <- c(ll.value,ll.val.new)

  }

  pardif <- gpcmdif$par
  pardif_idx <- which(abs(pardif) < 1e-8)   ### forces the small delta values to be zero
  pardif[pardif_idx] <- 0
  par.new <- c(gpcm$par,pardif)

  if(ll.val.new > ll.val.old){
    ll.val.new <- ll.val.old
    par.new <- par.old
  }

  return(list("value" = ll.val.new, "par" = par.new, iterations = i))

}

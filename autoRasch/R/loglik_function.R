##### The general implementation of the log-likelihood and gradient functions of the moodels ######

##### Estimated parameters extractor #####
#
# Mapping the estimated parameters to each variable, i.e., theta, beta, gamma, and delta.
#
# @param nlmPar A vector of the estimated parameters values.
# @param estPar_arr A vector of the types of the estimated parameters.
# @param estLength_array A vector of the lengths of each types of the estimated parameters.
# @param fixed_par A vector of the types of the fixed parameters.
# @param fixLength_arr A vector of the lengths of each types of the fixed parameters.
# @param fixValue A vector of the fixed parameters values.
#
# @return output A list of the extracted estimated parameter values.
#
##########################################
par_map <- function(nlmPar, estPar_arr, estLength_array, fixValue, fixed_par, fixLength_arr){

  output <- list()

  checkdata <- c("delta","gamma","beta","theta")

  for(cdat in checkdata){
    if(!identical(grep(cdat,estPar_arr), integer(0))){
      parNo <- grep(cdat,estPar_arr)
      if(parNo == 1){
        output[[cdat]] <- nlmPar[c((1):(sum(estLength_array[1])))]
      } else {
        output[[cdat]] <- nlmPar[c((sum(estLength_array[1:(parNo-1)])+1):(sum(estLength_array[1:parNo])))]
      }
    } else {
      parNo <- grep(cdat,fixed_par)
      if(parNo == 1){
        output[[cdat]] <- fixValue[c((1):(sum(fixLength_arr[1])))]
      } else {
        output[[cdat]] <- fixValue[c((sum(fixLength_arr[1:(parNo-1)])+1):(sum(fixLength_arr[1:parNo])))]
      }
    }
  }

  return(output)
}

##########################################################################################
##### THE LOG-LIKELIHOOD FUNCTION #####
#
# nlmPar : the estimated parameters
# dset : the dataset
# lamda_theta : the penalty coefficient on the theta parameters.
# lambda_in : the penalty coefficient on the included set
# lambda_out : the penalty coefficient on the excluded set
# lambda delta : the penalty coefficiant on the delta parameters
# estPar_arr : A vector of the types of the estimated parameters.
# estLength_array : A vector of the lengths of each types of the estimated parameters.
# fixed_par : A vector of the types of the fixed parameters.
# fixLength_arr : A vector of the lengths of each types of the fixed parameters.
# fixValue : A vector of the fixed parameters values.
# groups_map : Binary matrix. Respondents membership to DIF groups; rows represent individuals, column represent group partitions.
# mt_vek : A vector of the number of thresholds
# mt_idx : A vector of indexes to map the categories in each items.
# dimResp : A vector of the dimension of the response. Columns represent items and rows represent subjects.
# allcat : The number of beta parameters/thresholds for all items
# n_th : the number of beta for each items (the assumption for now is that every item has the same number of thresholds)
# XN : matrix for mapping x_vi (true response of person v to item i)
# XNA : matrix for mapping the NA responses
# eps : Small constant value as a workaround to solve the lasso penalty.
# isPenalized_theta : It is a logical parameter whether, in the estimation procedure, theta is penalized or not.
# isPenalized_gamma : It is a logical parameter whether, in the estimation procedure, gamma is penalized or not.
# isPenalized_delta : It is a logical parameter whether, in the estimation procedure, delta is penalized or not.
#
##########################################################################################
loglik_fun <- function(nlmPar, dset, opts, dataPrep,
                       estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array){


  # map the nlmPar
  map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
                        fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)
  theta <- map_nlmPar$theta
  beta <- map_nlmPar$beta
  gamma <- map_nlmPar$gamma
  delta <- map_nlmPar$delta
  deltagamma <- map_nlmPar$deltagamma

  # get the value of alpha, i.e. exp(gamma)
  exp_gamma <- exp(gamma)
  exp_gamma <- rep.int(exp_gamma, dataPrep$mt_vek)

  # groups_map should be formed as matrix, size is V x G
  groups_map <- as.matrix(dataPrep$groups_map)

  # takes the total of DIF effect for all group for beta
  # here we iterate over V, delta is a vector of length I X G, where the rows are stacked
  # dset is actually data.frame(X) and has dimension V X I, so ncol(dset) == I
  # delta_tot is written as an I x V matrix, as the outer product of delta (I x G) and k / groups_map (G x V)
  # !alternatively delta_tot <- delta %*% t(groups_map)
  delta_tot <- 0
  for(i in seq_len(ncol(groups_map))) {
    delta_tot <- delta_tot + outer(delta[(((i-1)*ncol(dset))+1):(i*ncol(dset))],groups_map[,i],"*")
  }

  # the total delta which has been replicated to all categories
  # - theta, size V, where each element is repeated I x J times
  # - vec(beta), size I x J, is repeated V times, for which the columns of beta (length J) are stacked ?!
  # # !! stacked on J first and then on I
  # # idx = I * J * (v - 1) + J * (i - 1) + (j - 1) + 1
  delta_tot_rep <- rep.int((delta_tot), rep.int(dataPrep$mt_vek,nrow(groups_map)))

  # compute the theta - (beta+delta)
  t_diff <- rep(theta,each = dataPrep$allcat) - rep.int(beta,length(theta))
  t_diff <- t_diff - delta_tot_rep

  # multiplied by exp(gamma)
  disc_diff <- t_diff * exp_gamma

  # map the corresponding NA value of the dataset to the matrix
  disc_diff <- dataPrep$XNA * disc_diff
  disc_diff <- matrix(disc_diff, nrow = dataPrep$allcat)

  # compute the first part of the log-likelihood (simple addition part)
  l1 <- sum((dataPrep$XN * disc_diff), na.rm = TRUE)

  ### compute the second part of the log-likelohood (with log)
  ### begin

  length.mt_idx <- table(dataPrep$mt_idx)
  temp_prob_split <- split(disc_diff,dataPrep$mt_idx)

  temp_l2 <- c()
  for(j in seq_along(temp_prob_split)){
    if(length.mt_idx[j] > 1){
      temp_l2 <- rbind(temp_l2,colSums(exp(apply(matrix(temp_prob_split[[j]], nrow = length.mt_idx[j]),2,cumsum)), na.rm = TRUE))
    } else {
      temp_l2 <- rbind(temp_l2,exp(temp_prob_split[[j]]))
    }
  }

  l2 <- sum(log(temp_l2+1), na.rm = TRUE)

  ### end

  st1st2 <- c(l1,l2)

  lnL <- st1st2[1] - st1st2[2]

  if(opts$isPenalized_theta){
    lnL <- lnL - (opts$lambda_theta*(sum(theta^2)))
  }

  if(opts$isPenalized_gamma & opts$isPenalized_theta){
    lnL <- lnL - (opts$lambda_in*(sum(gamma^2)))
  } else if(opts$isPenalized_gamma){
    lnL <- lnL - (opts$lambda_out*(sum(gamma^2)))
  }

  if(opts$isPenalized_delta){
    lnL <- lnL - (opts$lambda_delta*(sum(abs(delta)^(1+opts$eps))))
  }

  return(-lnL)
}


# Different way of implementing the log-likelihood function
loglik_fun_novel <- function(nlmPar, dset, lambda_theta, lambda_in, lambda_out, lambda_delta,
                             estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array,
                             groups_map, mt_vek, mt_idx, dimResp, allcat, n_th, XN, XNA, eps = 0,
                             isPenalized_gamma, isPenalized_theta, isPenalized_delta) {

  # TODO: incorporate the XNA part

  # map the nlmPar
  map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
                        fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)
  theta <- map_nlmPar$theta
  beta <- map_nlmPar$beta
  gamma <- map_nlmPar$gamma
  delta <- map_nlmPar$delta
  # deltagamma <- map_nlmPar$deltagamma

  exp_gamma <- exp(gamma)
  X <- as.matrix(dset)


  I <- ncol(dset)
  V <- nrow(dset)
  J <- max(mt_vek + 1) # max(m_i)

  # this needs to be fixed, beta needs to be passed differently
  beta_mat <- matrix(beta, nrow = I, byrow = TRUE)
  # delta_group <- groups_map %*% t(delta)
  delta_group <- groups_map %*% matrix(delta, nrow = ncol(groups_map), byrow = TRUE)
  Psi <- array(0, dim = c(V, I, J))

  # most efficient way is to iterate first by category
  for (i in 1:I) {
    J <- mt_vek[i] + 1
    for (v in 1:V) {
      for (j in 1:(J-1)) {
        Psi[v, i, j+1] <- Psi[v, i, j] + (theta[v] - beta_mat[i, j] - delta_group[v, i]) * exp_gamma[i]
      }
    }
  }


  l1 <- 0
  for (v in 1:V) {
    for (i in 1:I) {
      if(!is.na(X[v, i])){
        l1 <- l1 + Psi[v, i, X[v, i] + 1]
      }
    }
  }

  ### computed likelihood denominator (second part)

  exp_Psi <- exp(Psi)
  cumul_Psi <- matrix(0, V, I)

  # TODO: is m_i then the maximum number of categories (mt_vek?)

  for (i in 1:I) {
    J <- mt_vek[i] + 1
    for (v in 1:V) {
      if(!is.na(X[v,i])){
        for (j in 1:J) {
          cumul_Psi[v, i] <- cumul_Psi[v, i] + exp_Psi[v, i, j]
        }
      } else {
        cumul_Psi[v, i] <- NA
      }
    }
  }

  l2 <- sum(log(cumul_Psi), na.rm = TRUE)

  # Sidenote: If it were possible to compute everything with matrices
  # ## Compute first term
  # l1 <- t(theta) %*% X %*% exp_gamma
  #
  # # Compute the second term
  # # for (i in 1:I) {
  # #   beta[(i-1) * ]
  # # }
  # ones <- rep(1, length(theta))
  # XB <- matrix(colSums(matrix(XN * rep(beta, length(theta)), nrow = 4)), byrow = TRUE, ncol = ncol(dset))
  # l1 <- l1 - t(ones) %*% XB %*% exp_gamma
  #
  # # Compute the third term
  # # groups_map is V X G, t(delta) is G x I (hopefully)
  # delta_group <- groups_map %*% t(delta) # size V x I
  # XDG <- X * delta_group
  # l1 <- l1 - t(ones) %*% (XDG) %*% exp_gamma


  lnL <- l1 - l2

  if(isPenalized_theta){
    lnL <- lnL - lambda_theta * sum(theta^2)
  }

  if(isPenalized_gamma & isPenalized_theta){
    lnL <- lnL - lambda_in * sum(gamma^2)
  } else if(isPenalized_gamma){
    lnL <- lnL - lambda_out * sum(gamma^2)
  }

  if(isPenalized_delta){
    lnL <- lnL - lambda_delta * sum(abs(delta)^(1+eps))
  }

  return (-lnL)
}

# Wrapper for the Rcpp implementation of the log-likelihood function
loglik_fun_fast <- function(nlmPar, dset, estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array,
                            opts, dataPrep) {

  # TODO: implement XNA part

  map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
                        fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)
  theta <- map_nlmPar$theta
  beta_mat <- matrix(map_nlmPar$beta, ncol(dset), dataPrep$n_th, byrow = TRUE)
  gamma <- map_nlmPar$gamma
  if(opts$mode == "DIF"){
    delta_mat <- matrix(map_nlmPar$delta, ncol(dset), ncol(dataPrep$groups_map))#, byrow = TRUE)
    mode <- 1
  } else {
    delta_mat <- matrix(map_nlmPar$delta, length(beta_mat), ncol(dataPrep$groups_map))#, byrow = TRUE)
    mode <- 2
  }

  ll_cpp(theta, gamma, delta_mat, dataPrep$groups_map, beta_mat, dataPrep$mt_vek, as.matrix(dset),
         opts$isPenalized_gamma, opts$isPenalized_delta, opts$isPenalized_theta,
         opts$lambda_in, opts$lambda_out, opts$lambda_delta, opts$lambda_theta, opts$eps,mode)
}


# loglik_fun_fast <- function(nlmPar, dset, lambda_theta, lambda_in, lambda_out, lambda_delta,
#                             estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array,
#                             groups_map, mt_vek, mt_idx, dimResp, allcat, n_th, XN, XNA, eps = 0,
#                             isPenalized_gamma, isPenalized_theta, isPenalized_delta) {
#
#   # TODO: implement XNA part
#
#   map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
#                         fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)
#   theta <- map_nlmPar$theta
#   beta_mat <- matrix(map_nlmPar$beta, ncol(dset), n_th, byrow = TRUE)
#   gamma <- map_nlmPar$gamma
#   delta_mat <- matrix(map_nlmPar$delta, ncol(dset), ncol(groups_map))#, byrow = TRUE)
#
#   ll_cpp(theta, gamma, delta_mat, groups_map, beta_mat, mt_vek, as.matrix(dset),
#          isPenalized_gamma, isPenalized_delta, isPenalized_theta,
#          lambda_in, lambda_out, lambda_delta, lambda_theta, eps)
# }

############################################################################
#
#THE GRADIENT FUNCTION
#
############################################################################
grad_fun <- function(nlmPar, dset, opts, dataPrep,
                     estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array){

  lambda_theta <- opts$lambda_theta*2
  lambda_in <- opts$lambda_in*2
  lambda_out <- opts$lambda_out*2

  map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
                        fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)

  theta <- map_nlmPar$theta
  beta <- map_nlmPar$beta
  gamma <- map_nlmPar$gamma
  delta <- map_nlmPar$delta
  deltagamma <- map_nlmPar$deltagamma

  exp_gamma <- exp(gamma)
  exp_gamma <- rep.int(exp_gamma, dataPrep$mt_vek)
  t_exp_gamma_mat <- rep.int(1,(length(dataPrep$XN)))*exp_gamma

  groups_map <- as.matrix(dataPrep$groups_map)

  delta_tot <- 0
  for(i in seq_len(ncol(groups_map))) {
    delta_tot <- delta_tot + outer(delta[(((i-1)*ncol(dset))+1):(i*ncol(dset))],groups_map[,i],"*")
  }
  delta_tot_rep <- rep.int((delta_tot), rep.int(dataPrep$mt_vek,nrow(groups_map)))

  total_alpha_mat <- rep(exp_gamma,length(theta))

  t_diff <- rep(theta,each = dataPrep$allcat) - rep.int(beta,length(theta))

  t_diff <- t_diff - delta_tot_rep          #delta.tot.rep is total delta which has been replicated to every categoory
  disc_diff <- t_diff * total_alpha_mat
  disc_diff <- dataPrep$XNA * disc_diff
  disc_diff <- matrix(disc_diff, nrow = dataPrep$allcat)

  ##### first part of gradient ###

  ## theta ##
  t1_theta_mat <- (dataPrep$XN) * total_alpha_mat
  t1_theta_mat <- matrix(t1_theta_mat, nrow = dataPrep$allcat)
  t1_theta <- colSums(t1_theta_mat, na.rm = TRUE)

  ## beta ##
  t1_beta_mat <- t1_theta_mat
  t1_beta_mat <- matrix(t1_beta_mat, nrow = dataPrep$allcat)
  t1_beta <- rowSums(t1_beta_mat, na.rm = TRUE)

  ## gamma ##
  t1_gamma_mat <- t_diff * t1_beta_mat
  t1_gamma_mat <- matrix(t1_gamma_mat, nrow = dataPrep$allcat)
  t1_gamma <- rowSums(t1_gamma_mat, na.rm = TRUE)
  t1_gamma <- tapply(t1_gamma, dataPrep$mt_idx, sum, na.rm = TRUE)

  t1_delta <- c()
  for(i in seq_len(ncol(groups_map))) {
    ## delta ##
    t1_delta_mat <- t(t(t1_theta_mat) * groups_map[,i])
    t1_delta_temp <- rowSums(t1_delta_mat, na.rm = TRUE)
    t1_delta_temp <- tapply(t1_delta_temp, dataPrep$mt_idx, sum, na.rm = TRUE)
    t1_delta <- c(t1_delta, t1_delta_temp)
  }

  ##### first part of gradient end ###

  # make them as a vector

  total_alpha_mat <- matrix(total_alpha_mat, nrow = dataPrep$allcat)

  temp_denom <- c()
  temp_gamma <- c()
  expTempProb <- c()
  temp_theta_nom <- c()
  temp_gamma_nom <- c()

  length.mt_idx <- table(dataPrep$mt_idx)

  temp_prob_split <- split(disc_diff,dataPrep$mt_idx)

  # print(length(temp_prob_split))

  temp_gamma_split <- split(total_alpha_mat, dataPrep$mt_idx)

  temp_denom_part <- c()
  temp_gamma_part <- c()
  expTempProb_part <- c()
  temp_beta_part <- c()
  temp_tot <- c()
  temp_theta_nom_part <- c()
  temp_gamma_nom_part <- c()
  temp_delta_peritem <- list()
  for(j in seq_along(temp_prob_split)){

    if(length.mt_idx[j] > 1){
      temp_prob_part <- matrix(temp_prob_split[[j]], nrow = length.mt_idx[j])
      temp_prob_part <- apply(temp_prob_part,2,cumsum)

      expTempProb_temp <- exp(temp_prob_part)
      temp_denom_part <- rbind(temp_denom_part,colSums(expTempProb_temp, na.rm = TRUE))

      temp_gamma_temp <- matrix(temp_gamma_split[[j]], nrow = length.mt_idx[j])
      temp_gamma_temp <- apply(temp_gamma_temp,2,cumsum)

      temp_theta_nom_mat_part <- expTempProb_temp*temp_gamma_temp
      temp_theta_nom_part <- rbind(temp_theta_nom_part,colSums(temp_theta_nom_mat_part,na.rm = TRUE))

      temp_gamma_nom_mat_part <- expTempProb_temp*temp_prob_part
      temp_gamma_nom_part <- rbind(temp_gamma_nom_part,colSums(temp_gamma_nom_mat_part,na.rm = TRUE))

      temp_delta_peritem_test <- c()
      for(k in seq_len(ncol(groups_map))) {
        temp_delta_temp_peritem <- t(t(temp_theta_nom_mat_part) * groups_map[,k])
        temp_delta_peritem_test <- rbind(temp_delta_peritem_test,colSums(temp_delta_temp_peritem,na.rm = TRUE))
      }
      temp_delta_peritem[[j]] <- temp_delta_peritem_test


      temp_tot_temp <- apply(as.matrix(expTempProb_temp[length.mt_idx[j]:1,]),2,cumsum)
      temp_tot <- rbind(temp_tot,as.matrix(temp_tot_temp[length.mt_idx[j]:1,]))

      temp_gamma_part <- rbind(temp_gamma_part,temp_gamma_temp)
      expTempProb_part <- rbind(expTempProb_part,expTempProb_temp)

    } else {
      temp_prob_part <- temp_prob_split[[j]]

      expTempProb_temp <- exp(temp_prob_part)
      temp_denom_part <- rbind(temp_denom_part,expTempProb_temp)

      temp_gamma_temp <- temp_gamma_split[[j]]

      temp_theta_nom_mat_part <- expTempProb_temp*temp_gamma_temp
      temp_theta_nom_part <- rbind(temp_theta_nom_part,temp_theta_nom_mat_part)

      temp_gamma_nom_mat_part <- expTempProb_temp*temp_prob_part
      temp_gamma_nom_part <- rbind(temp_gamma_nom_part,temp_gamma_nom_mat_part)

      temp_delta_peritem_test <- c()
      for(k in seq_len(ncol(groups_map))) {
        temp_delta_temp_peritem <- temp_theta_nom_mat_part * groups_map[,k]
        temp_delta_peritem_test <- rbind(temp_delta_peritem_test,temp_delta_temp_peritem)
      }
      temp_delta_peritem[[j]] <- temp_delta_peritem_test


      temp_tot_temp <- expTempProb_temp
      temp_tot <- rbind(temp_tot,temp_tot_temp)

      temp_gamma_part <- rbind(temp_gamma_part,temp_gamma_temp)
      expTempProb_part <- rbind(expTempProb_part,expTempProb_temp)

    }

  }

  temp_delta_nom_part <- list()
  for(k in seq_len(ncol(groups_map))) {
    temp <- c()
    for(i in seq_along(temp_delta_peritem)){
      temp <- rbind(temp, temp_delta_peritem[[i]][k,])
    }
    temp_delta_nom_part[[k]] <- temp
  }

  # print(temp_delta_nom_part[[1]][1:3, 1:3])

  temp_gamma <- temp_gamma_part
  expTempProb <- expTempProb_part

  temp_beta_nom <- temp_tot * total_alpha_mat

  Nom_theta <- matrix(temp_theta_nom_part,nrow = ncol(dset))
  Nom_beta <- matrix(temp_beta_nom,nrow = dataPrep$allcat)
  Nom_gamma <- matrix(temp_gamma_nom_part,nrow = ncol(dset))

  Denom <- matrix(temp_denom_part,nrow = ncol(dset))+1
  Denom_mat <- matrix(rep(Denom, rep(dataPrep$mt_vek,dataPrep$dimResp[1])), nrow = dataPrep$allcat)

  t2_theta_mat <- (Nom_theta/Denom)
  t2_theta <- colSums(t2_theta_mat, na.rm = TRUE)


  t2_beta_mat <- (Nom_beta/Denom_mat)
  t2_beta <- rowSums(t2_beta_mat, na.rm = TRUE)

  # print(paste("Nom theta", Nom_theta[1, 1]))
  # print(paste("Nom gamma", Nom_gamma[1, 1]))
  # print(paste("Denom", Denom[1, 1]))

  t2_gamma_mat <- (Nom_gamma/Denom)
  t2_gamma <- rowSums(t2_gamma_mat, na.rm = TRUE)

  t2_delta <- c()
  for(k in seq_len(ncol(groups_map))) {
    Nom_delta <- matrix(temp_delta_nom_part[[k]],nrow = ncol(dset))
    t2_delta_mat <- (Nom_delta/Denom)
    # print(Denom)
   # print(t2_delta_mat)
    #print(dim(t2_delta_mat))
    t2_delta <- c(t2_delta,rowSums(t2_delta_mat, na.rm = TRUE))
  }

  # print(t1_theta - t2_theta)
  #print (t1_beta - t2_beta)
  # print(paste("theta:", length(t1_theta), length(t2_theta)))
  # print(t1_theta - t2_theta)
  # print(paste("beta:", length(t1_beta), length(t2_beta)))
  # print(- t1_beta + t2_beta)
  # print(paste("gamma:", length(t1_gamma), length(t2_gamma)))
  # print(t1_gamma - t2_gamma)
  #print(paste("delta:", length(t1_delta), length(t2_delta)))
  # print(- t1_delta + t2_delta)
  # print(t1_delta)
  #print(t2_delta)

  if(opts$isPenalized_theta){
    # grad_theta <- t1_theta - t2_theta - (0.1*theta)
    grad_theta <- t1_theta - t2_theta - (lambda_theta*theta)
  } else {
    grad_theta <- t1_theta - t2_theta
  }

  # NOTE: this doesn't really do anything, no if statement necessary
  if(opts$isPenalized_theta){
    grad_beta <- (-t1_beta) + t2_beta
  } else {
    grad_beta <- (-t1_beta) + t2_beta
  }

  if(opts$isPenalized_gamma & opts$isPenalized_theta){
    grad_gamma <- t1_gamma - t2_gamma - (lambda_in*(gamma))
  }else if(opts$isPenalized_gamma){
    grad_gamma <- t1_gamma - t2_gamma - (lambda_out*(gamma))
  }else {
    grad_gamma <- t1_gamma - t2_gamma
  }

  if(opts$isPenalized_delta){
    grad_delta <- (-t1_delta) + t2_delta - (opts$lambda_delta*(1+opts$eps)*sign(delta)*(abs(delta)^opts$eps))
  } else {
    grad_delta <- (-t1_delta) + t2_delta
  }

  output <- c()

  if(!identical(grep("delta",estPar_arr), integer(0))){
    output <- c(grad_delta,output)
  }
  if(!identical(grep("^gamma",estPar_arr), integer(0))){
    output <- c(grad_gamma,output)
  }
  if(!identical(grep("^beta",estPar_arr), integer(0))){
    output <- c(grad_beta,output)
  }
  if(!identical(grep("theta",estPar_arr), integer(0))){
    output <- c(grad_theta,output)
  }

  grad_tot <- output

  return(-grad_tot)

}


# Different way of implementing the gradient function
grad_fun_novel <- function(nlmPar, dset, lambda_theta, lambda_in, lambda_out, lambda_delta,
                           estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array,
                           groups_map, mt_vek, mt_idx, dimResp, allcat, n_th, XN, XNA, eps = 0,
                           isPenalized_gamma, isPenalized_theta, isPenalized_delta){

  # TODO: incorporate the XNA part

  lambda_theta <- lambda_theta*2
  lambda_in <- lambda_in*2
  lambda_out <- lambda_out*2

  map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
                        fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)

  theta <- map_nlmPar$theta
  beta <- map_nlmPar$beta
  gamma <- map_nlmPar$gamma
  delta <- map_nlmPar$delta

  exp_gamma <- exp(gamma)
  X <- as.matrix(dset)

  I <- ncol(dset)
  V <- nrow(dset)
  J <- max(mt_vek + 1) # max(m_i)

  # this needs to be fixed, beta needs to be passed differently
  beta_mat <- matrix(beta, nrow = I, byrow = TRUE)

  # size V x G
  groups_mat <- as.matrix(groups_map)
  G <- ncol(groups_mat)
  # size I X G
  delta_mat <- matrix(delta, I, G)

  # delta_group <- groups_mat %*% t(delta_mat)
  delta_group <- groups_mat %*% matrix(delta, nrow = ncol(groups_map), byrow = TRUE)
  Psi <- array(0, dim = c(V, I, J))

  # most efficient way is to iterate first by category
  # NOTE: this could be potentially shared with loglik function
  for (i in 1:I) {
    J <- mt_vek[i] + 1
    for (v in 1:V) {
      for (j in 1:(J-1)) {
        Psi[v, i, j+1] <- Psi[v, i, j] + (theta[v] - beta_mat[i, j] - delta_group[v, i]) * exp_gamma[i]
      }
    }
  }

  ### computed likelihood denominator (second part)

  exp_Psi <- exp(Psi)

  cumul_Psi <- matrix(0, V, I)
  theta_Psi <- matrix(0, V, I)
  gamma_Psi <- matrix(0, V, I)

  # TODO: is m_i then the maximum number of categories (mt_vek?)

  for (i in 1:I) {
    J <- mt_vek[i] + 1
    for (v in 1:V) {
      if(!is.na(X[v,i])){
        for (j in 2:J) {
          cumul_Psi[v, i] <- cumul_Psi[v, i] + exp_Psi[v, i, j]
          theta_Psi[v, i] <- theta_Psi[v, i] + (j - 1) * exp_gamma[i] * exp_Psi[v, i, j]
          gamma_Psi[v, i] <- gamma_Psi[v, i] + Psi[v, i, j] * exp_Psi[v, i, j]
        }
      } else {
        cumul_Psi[v, i] <- NA
        theta_Psi[v, i] <- NA
        gamma_Psi[v, i] <- NA
      }
    }
  }

  beta_Psi <- array(0, c(V, I, J))
  delta_Psi <- array(0, c(V, I, G))

  # NOTE: cumul needs to be fully computed by this point
  for (i in 1:I) {
    J <- mt_vek[i] + 1
    for (v in 1:V) {
      for (j in 2:J) {
        delta_Psi[v, i, ] <- delta_Psi[v, i, ] + (j - 1) * exp_gamma[i] * exp_Psi[v, i, j] * groups_mat[v, ] / (1 + cumul_Psi[v, i])
      }
      for (j in J:2) {
        beta_Psi[v, i, j-1] <- beta_Psi[v, i, j] + exp_gamma[i] * exp_Psi[v, i, j] / (1 + cumul_Psi[v, i])
      }
    }
  }

  grad_theta <- X %*% exp_gamma - rowSums(theta_Psi / (1 + cumul_Psi))

  grad_beta <- matrix(0, I, J-1)

  for (j in 1:(J-1)) {
    grad_beta[,j] <- colSums(X >= j, na.rm = TRUE) * exp_gamma
  }

  grad_beta <- (-grad_beta) + apply(beta_Psi, 2:3, sum, na.rm=TRUE)[, -J]

  grad_gamma <- numeric(I)
  for (i in 1:I) {
    for (v in 1:V) {
      if(!is.na(X[v,i])){
        grad_gamma[i] <- grad_gamma[i] + Psi[v, i, X[v, i] + 1]
      }
    }
  }
  grad_gamma <- grad_gamma - colSums((gamma_Psi / (1 + cumul_Psi)),na.rm = TRUE)

  X_wo_NA <- X
  X_wo_NA[is.na(X_wo_NA)] <- 0

  grad_delta <- - diag(exp_gamma) %*% t(X_wo_NA) %*% groups_mat + apply(delta_Psi, 2:3, sum, na.rm = TRUE)

  if(isPenalized_theta){
    # grad_theta <- t1_theta - t2_theta - (0.1*theta)
    grad_theta <- grad_theta - (lambda_theta*theta)
  }


  if(isPenalized_gamma && isPenalized_theta){
    grad_gamma <- grad_gamma - (lambda_in*(gamma))
  } else if(isPenalized_gamma){
    grad_gamma <- grad_gamma - (lambda_out*(gamma))
  }

  if(isPenalized_delta){
    grad_delta <- grad_delta - (lambda_delta*(1+eps)*sign(delta)*(abs(delta)^eps))
  }

  output <- c()

  if(!identical(grep("delta",estPar_arr), integer(0))){
    output <- c(t(grad_delta),output)
  }
  if(!identical(grep("^gamma",estPar_arr), integer(0))){
    output <- c(grad_gamma,output)
  }
  if(!identical(grep("^beta",estPar_arr), integer(0))){
    output <- c(t(grad_beta),output)
  }
  if(!identical(grep("theta",estPar_arr), integer(0))){
    output <- c(grad_theta,output)
  }

  grad_tot <- output

  return(-grad_tot)

}


# grad_fun_fast(nlmPar, dset = dataPrep$dset,
# lambda_theta = opts$lambda_theta, lambda_in = opts$lambda_in,lambda_out = opts$lambda_out, eps = opts$eps,
# lambda_delta = opts$lambda_delta, estPar_arr = estPar_arr, estLength_array = estLength_array,
# fixLength_arr = fixLength_arr, allcat = dataPrep$allcat, dimResp = dataPrep$dimResp, n_th = dataPrep$n_th, XN = dataPrep$XN, XNA = dataPrep$XNA, #XREAL = dataPrep$XREAL,
# groups_map = dataPrep$groups_map, mt_vek = dataPrep$mt_vek, mt_idx = dataPrep$mt_idx, fixed_par = opts$fixed_par, fixValue = fixValue,
# isPenalized_gamma = opts$isPenalized_gamma, isPenalized_theta = opts$isPenalized_theta,
# isPenalized_delta = opts$isPenalized_delta)

# Wrapper for the Rcpp implementation of the gradient function
grad_fun_fast <- function(nlmPar, dset, estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array,
                          opts, dataPrep) {

  # TODO: implement XNA part

  map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
                        fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)
  theta <- map_nlmPar$theta
  beta_mat <- matrix(map_nlmPar$beta, ncol(dset), dataPrep$n_th, byrow = TRUE)
  gamma <- map_nlmPar$gamma
  # # delta_mat <- matrix(map_nlmPar$delta, ncol(dset), ncol(dataPrep$groups_map))#, byrow = TRUE)
  # delta_mat <- matrix(map_nlmPar$delta, length(beta_mat), ncol(dataPrep$groups_map))#, byrow = TRUE)
  if(opts$mode == "DIF"){
    delta_mat <- matrix(map_nlmPar$delta, ncol(dset), ncol(dataPrep$groups_map))#, byrow = TRUE)
    mode <- 1
  } else {
    delta_mat <- matrix(map_nlmPar$delta, length(beta_mat), ncol(dataPrep$groups_map))#, byrow = TRUE)
    mode <- 2
  }

  result <- grad_cpp(theta, gamma, delta_mat, dataPrep$groups_map, beta_mat, dataPrep$mt_vek, as.matrix(dset),
         opts$isPenalized_gamma, opts$isPenalized_delta, opts$isPenalized_theta,
         opts$lambda_in, opts$lambda_out, opts$lambda_delta, opts$lambda_theta, opts$eps, mode)

  # print(result)

  output <- c()

  if(!identical(grep("delta",estPar_arr), integer(0))){
    output <- c(result$grad_delta,output)
  }
  if(!identical(grep("^gamma",estPar_arr), integer(0))){
    output <- c(result$grad_gamma,output)
  }
  if(!identical(grep("^beta",estPar_arr), integer(0))){
    output <- c(result$grad_beta,output)
  }
  if(!identical(grep("theta",estPar_arr), integer(0))){
    output <- c(result$grad_theta,output)
  }

  grad_tot <- output

  return(-grad_tot)
}

# grad_fun_fast <- function(nlmPar, dset, lambda_theta, lambda_in, lambda_out, lambda_delta,
#                           estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array,
#                           groups_map, mt_vek, mt_idx, dimResp, allcat, n_th, XN, XNA, eps = 0,
#                           isPenalized_gamma, isPenalized_theta, isPenalized_delta) {
#
#   # TODO: implement XNA part
#
#   map_nlmPar <- par_map(nlmPar, estPar_arr = estPar_arr, estLength_array = estLength_array,
#                         fixValue = fixValue, fixed_par = fixed_par, fixLength_arr = fixLength_arr)
#   theta <- map_nlmPar$theta
#   beta_mat <- matrix(map_nlmPar$beta, ncol(dset), n_th, byrow = TRUE)
#   gamma <- map_nlmPar$gamma
#   delta_mat <- matrix(map_nlmPar$delta, ncol(dset), ncol(groups_map))#, byrow = TRUE)
#
#   result <- grad_cpp(theta, gamma, delta_mat, groups_map, beta_mat, mt_vek, as.matrix(dset),
#                      isPenalized_gamma, isPenalized_delta, isPenalized_theta,
#                      lambda_in, lambda_out, lambda_delta,lambda_theta, eps)
#
#   # print(result)
#
#   output <- c()
#
#   if(!identical(grep("delta",estPar_arr), integer(0))){
#     output <- c(result$grad_delta,output)
#   }
#   if(!identical(grep("^gamma",estPar_arr), integer(0))){
#     output <- c(result$grad_gamma,output)
#   }
#   if(!identical(grep("^beta",estPar_arr), integer(0))){
#     output <- c(result$grad_beta,output)
#   }
#   if(!identical(grep("theta",estPar_arr), integer(0))){
#     output <- c(result$grad_theta,output)
#   }
#
#   grad_tot <- output
#
#   return(-grad_tot)
# }


GPCMDIF_LL <- function(dset = c(), fixValue.theta, fixValue.beta, fixValue.gamma, fixValue.delta, lambda_theta = 0.05, lambda_in = 50, lambda_out = 5e-3, lambda_delta = 17, lambda_deltagamma = 100000, eps = 0, resp.info = c(), resp.th = c(1,1), isPenalized_gamma = TRUE, isPenalized_theta = TRUE, isPenalized_delta = TRUE, mt_vek_init = c()){

  if(is.null(dim(dset))){
    dset <- matrix(dset, ncol = 1)
  }

  print(dim(dset))

  lambda_theta <- lambda_theta
  lambda_in <- lambda_in
  lambda_out <- lambda_out
  lambda_delta <- lambda_delta
  lambda_deltagamma <- lambda_deltagamma
  eps <- eps

  groups_map <- as.matrix(resp.info)
  print(dim(groups_map))
  if(!is.null(mt_vek_init)){
    mt_vek <- mt_vek_init
  } else {
    mt_vek <- apply(dset, 2L, max, na.rm = TRUE)      ### create vector of max categories for each item
  }
  # mt_vek <- apply(dset, 2L, max, na.rm = TRUE)   #number of categories - 1 for each item
  mt_vek <- rep(max(mt_vek),length(mt_vek))
  print(mt_vek)
  allcat <- sum(mt_vek)         #number of items * categories (assumption : item has the same number of categories)
  n_th <- max(mt_vek)

  xn.mat <- matrix(0,nrow = nrow(dset), ncol = allcat) ## response position
  xna.mat <- matrix(1,nrow = nrow(dset), ncol = allcat)## NA position

  for(i in 1:n_th){
    idx <- which(dset==i)
    new.idx <- (nrow(dset)*n_th*(ceiling(idx/nrow(dset))-1))+(idx%%nrow(dset))+(ifelse(idx%%nrow(dset) == 0,1,0)*nrow(dset))
    full.idx <- c()
    for(j in 1:i){
      next.idx <- new.idx+(nrow(dset)*(j-1))
      full.idx <- c(full.idx,next.idx)
    }
    xn.mat[full.idx] <- 1
  }

  idx <- which(is.na(dset))
  new.idx <- (nrow(dset)*n_th*(ceiling(idx/nrow(dset))-1))+(idx%%nrow(dset))+(ifelse(idx%%nrow(dset) == 0,1,0)*nrow(dset))
  full.idx <- c()
  for(j in 1:mt_vek[1]){
    next.idx <- new.idx+(nrow(dset)*(j-1))
    full.idx <- c(full.idx,next.idx)
  }
  xn.mat[full.idx] <- NA
  xna.mat[full.idx] <- NA
  # xn.mat[full.idx] <- 9
  XN <- t(xn.mat)  #need to be transposed from the original dataset form
  XN <- as.vector(XN)
  XNA <- t(xna.mat)
  XNA <- as.vector(XNA)

  theta <- fixValue.theta
  beta <- fixValue.beta
  gamma <- fixValue.gamma
  delta <- fixValue.delta
  deltagamma <- rep(0,length(delta))

  ### get the value of alpha
  exp_gamma <- exp(gamma)
  exp_gamma <- rep.int(exp_gamma, mt_vek)

  groups_map <- as.matrix(groups_map) #groups_map should be formed as matrix

  ### take the total of DIF effect for every group for beta or gamma
  delta_tot <- 0
  deltagamma_tot <- 0
  for(i in seq_len(ncol(groups_map))) {
    delta_tot <- delta_tot + outer(delta[(((i-1)*ncol(dset))+1):(i*ncol(dset))],groups_map[,i],"*")
    deltagamma_tot <- deltagamma_tot + outer(deltagamma[(((i-1)*ncol(dset))+1):(i*ncol(dset))],groups_map[,i],"*")
  }
  delta_tot_rep <- rep.int((delta_tot), rep.int(mt_vek,nrow(groups_map)))       #delta_tot_rep is total delta which has been replicated to every categoory
  exp_deltagamma_tot_rep <- rep.int((exp(deltagamma_tot)), rep.int(mt_vek,nrow(groups_map)))     #deltagamma_tot.rep is total deltagamma which has been replicated to every categoory


  ### compute the theta - (beta+delta)
  t_diff <- rep(theta,each = allcat) - rep.int(beta,length(theta))
  t_diff <- t_diff - delta_tot_rep

  ### multiplied by (exp(gamma+deltagamma))
  disc_diff <- t_diff * exp_gamma
  disc_diff <- disc_diff * exp_deltagamma_tot_rep

  ### map the corresponding NA value of the dataset to the matrix
  disc_diff <- XNA * disc_diff

  ### compute the first part of the log-likelihood (simple addition part)
  l1 <- sum((XN * disc_diff), na.rm = TRUE)

  ### compute the second part of the log-likelohood (with log)
  ### begin
  per_cat_list <- matrix(disc_diff, nrow = n_th)
  temp_prob <- as.matrix(per_cat_list[1,])
  temp_l2 <- exp(temp_prob)
  if(n_th > 1){
    for(i in 2:n_th){
      temp_prob <- cbind(temp_prob,(temp_prob[,i-1]+per_cat_list[i,]))
      temp_l2 <- temp_l2 + (exp(temp_prob[,i]))
    }
  }

  l2 <- sum(log(temp_l2+1), na.rm = TRUE)

  st1st2 <- c(l1,l2)

  lnL <- st1st2[1] - st1st2[2]

  if(isPenalized_theta){
    lnL <- lnL - (lambda_theta*(sum(theta^2)))
  }

  if(isPenalized_theta & isPenalized_gamma){
    lnL <- lnL - (lambda_in*(sum(gamma^2)))
  } else if(isPenalized_gamma){
    lnL <- lnL - (lambda_out*(sum(gamma^2)))
  }

  if(isPenalized_delta){
    lnL <- lnL - (lambda_delta*(sum(abs(delta)^(1+eps))))
  }

  return(lnL)
}

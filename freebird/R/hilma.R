hilma <- function(Y, G, S, mediation_setting = 'incomplete', tuning_method = 'uniform', lam_list = NA,
                       min.ratio = 0.1, n.lambda = 5, center = TRUE) {


  n = dim(G)[1]
  p = dim(G)[2]
  q = dim(S)[2]

  if (center == TRUE) {

    Y = Y - mean(Y)
    sm=matrix(rep(colMeans(S),n),nrow=n,ncol=q,byrow=T)
    S=S-sm
    gm=matrix(rep(colMeans(G),n),nrow=n,ncol=p,byrow=T)
    G=G-gm

  }

  if (mediation_setting == 'incomplete') {
    X = cbind(G,S)

    sigma_SS_hat = t(S)%*%S/n
    Sigma_SG_hat = t(S)%*%G/n
    Sigma_GG_hat = t(G)%*%G/n
    Sigma_XX_hat = t(X)%*%X/n

    sigma_SS_hat_inverse = solve(sigma_SS_hat)

    result_scalreg = scalreg(X,Y)
    alpha_hat = result_scalreg$co
    sigma1_hat = result_scalreg$hsigma

    if(sigma1_hat > 10) {
      result_scalreg = scalreg(round(X,2),round(Y,2))
      alpha_hat = result_scalreg$co
      sigma1_hat = result_scalreg$hsigma
    }

    sigma_hat = summary(lm(Y~S))$sigma
    sigma2_hat = sqrt(max(sigma_hat^2-sigma1_hat^2,0))
    lambda.k_hat = t(X)%*%(Y-X%*%alpha_hat)/n

    Dhat = matrix(0,2*q,p+q)
    Dhat[1:q,1:p] = Sigma_SG_hat
    Dhat[(q+1):(2*q),((p+1):(p+q))] = sigma_SS_hat

    Beta.list = list()
    for (qj in 1:(2*q)) {
      if (is.na(lam_list)) outj = slim(X, t(n*Dhat), Y_rachel_column = qj,method = 'dantzig', lambda.min.ratio = min.ratio, nlambda = n.lambda) else outj = slim(X, t(n*Dhat), Y_rachel_column = qj,method = 'dantzig', lambda = lam_list)
      Beta.list[[qj]] = outj$beta
    }

    if (is.na(lam_list)) {
      if (tuning_method == 'uniform') lam_list = sqrt(log(p)/n)/3 else lam_list = outj$lambda
    }

    Omegahat.list = list()
    CV = CV1 = CV2 = rep(0,length(lam_list))
    for (l in 1:length(lam_list)) {

      Omegahat_temp= matrix(0,2*q,p+q)
      for (qj in 1:(2*q)) {
        Omegahat_temp[qj,] = Beta.list[[qj]][,l]
      }

      Omegahat.list[[l]] = Omegahat_temp
      CV1[l] = max(abs(Dhat - Omegahat.list[[l]] %*% Sigma_XX_hat))
      CV2[l] = length(which(Omegahat.list[[l]]!=0))

      if (tuning_method == 'aic')
      {
        CV[l] = n * CV1[l] + 2 * CV2[l]
      } else if (tuning_method == 'bic')
        CV[l] = n * CV1[l] + log(n) * CV2[l]
      }

    i_lam = which(CV == min(CV))[1]
    Omega_hat = Omegahat.list[[i_lam]]
    lam = lam_list[i_lam]

    beta_part1 = kronecker(diag(2),sigma_SS_hat_inverse)%*%Omega_hat%*%lambda.k_hat
    beta_hat = beta_part1[1:q,1] + sigma_SS_hat_inverse%*%Sigma_SG_hat%*%alpha_hat[1:p]
    alpha1_hat = beta_part1[(q+1):(2*q),1] + alpha_hat[p+1]
    total_hat = alpha1_hat + beta_hat


    Cov_part1 = sigma1_hat^2*kronecker(diag(2),sigma_SS_hat_inverse)%*%Omega_hat%*%Sigma_XX_hat%*%t(Omega_hat)%*%kronecker(diag(2),sigma_SS_hat_inverse)
    Sigma_11 = Cov_part1[1:q,1:q] + sigma2_hat^2*sigma_SS_hat_inverse
    Sigma_12 = Cov_part1[(q+1):(2*q),1:q]
    Sigma_22 = Cov_part1[(q+1):(2*q),(q+1):(2*q)]

    sigma_beta_hat = Sigma_11
    sigma_alpha1_hat = Sigma_22

    if (q > 1) {
      teststat_beta = beta_hat/sqrt(diag (sigma_beta_hat) /n)
      teststat_alpha1 = alpha1_hat/sqrt( diag (sigma_alpha1_hat) /n)
    } else if (q == 1) {
      teststat_beta = beta_hat/sqrt(sigma_beta_hat /n)
      teststat_alpha1 = alpha1_hat/sqrt( sigma_alpha1_hat/n)
    }

    for (q_i in 1:q) if (alpha1_hat[q_i] == 0) teststat_alpha1[q_i] = 0
    for (q_i in 1:q) if (beta_hat[q_i] == 0) teststat_beta[q_i] = 0

    p_beta = 2*(1-pnorm(abs(teststat_beta)))
    p_alpha1 = 2*(1-pnorm(abs(teststat_alpha1)))

    infer_out = list()
    infer_out$beta_hat = beta_hat
    infer_out$alpha1_hat = alpha1_hat
    infer_out$pvalue_beta_hat = p_beta
    infer_out$lambda_used = lam
    #infer_out$pvalue_alpha1_hat = p_alpha1



  } else if (mediation_setting == 'complete') {

    sigma_SS_hat = t(S)%*%S/n
    Sigma_SG_hat = t(S)%*%G/n
    Sigma_GG_hat = t(G)%*%G/n
    sigma_hat = summary(lm(Y~S))$sigma

    result_scalreg = scalreg(G,Y)
    alpha_hat = result_scalreg$co
    sigma1_hat = result_scalreg$hsigma
    if(sigma1_hat > 10) {
      result_scalreg = scalreg(round(G,2),round(Y,2))
      sigma1_hat = result_scalreg$hsigma
    }

    sigma2_hat = sqrt(max(sigma_hat^2-sigma1_hat^2,0))
    lambda.k_hat = t(G)%*%(Y-G%*%alpha_hat)/n

    Beta.list = list()
    for (qj in 1:q) {
      if (is.na(lam_list)) outj = slim(G, t(G)%*%S, Y_rachel_column = qj,method = 'dantzig', lambda.min.ratio = min.ratio, nlambda = n.lambda) else outj = slim(G, t(G)%*%S, Y_rachel_column = qj,method = 'dantzig', lambda = lam_list)
      Beta.list[[qj]] = outj$beta
    }

    if (is.na(lam_list)) lam_list = outj$lambda

    Omegahat.list = list()
    CV = CV1 = CV2 = rep(0,length(lam_list))
    for (l in 1:length(lam_list)) {

      Omegahat_temp= matrix(0,q,p)
      for (qj in 1:q) {
        Omegahat_temp[qj,] = Beta.list[[qj]][,l]
      }

      Omegahat.list[[l]] = Omegahat_temp
      CV1[l] = max(abs(Sigma_SG_hat - Omegahat.list[[l]] %*% Sigma_GG_hat))
      CV2[l] = length(which(Omegahat.list[[l]]!=0))

      if (tuning_method == 'aic')
      {
        CV[l] = n * CV1[l] + 2 * CV2[l]
      } else if (tuning_method == 'bic')
        CV[l] = n * CV1[l] + log(n) * CV2[l]
    }

    i_lam = which(CV == min(CV))[1]
    Omega_hat = Omegahat.list[[i_lam]]
    lam = lam_list[i_lam]

    beta_hat = solve(sigma_SS_hat)%*%(Sigma_SG_hat%*%alpha_hat+Omega_hat%*%lambda.k_hat)
    sigma_beta_hat = sigma1_hat^2*solve(sigma_SS_hat)%*%Omega_hat%*%Sigma_GG_hat%*%t(Omega_hat)%*%solve(sigma_SS_hat) + sigma2_hat^2*solve(sigma_SS_hat)
    if (q >1) teststat = beta_hat/sqrt(diag (sigma_beta_hat)/n) else if (q==1) teststat = beta_hat/sqrt(sigma_beta_hat/n)

    p_beta = 2*(1-pnorm(abs(teststat)))

    infer_out = list()
    infer_out$beta_hat = beta_hat
    infer_out$pvalue_beta_hat = p_beta
    infer_out$lambda_used = lam
  }

  return(infer_out)

}


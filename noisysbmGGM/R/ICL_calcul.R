#=======================  When the variance is known =========================
evalICL <- function(ind.all, dataVec, Z, Z_matrix, Q, A, rho, tau,  n0, eta0, zeta0, sigma0, sigma1){  #A is a vector

  p = length(Z)
  N <-  p*(p-1)/2

  nk=rep(NA,Q)
  term2_SBM=0
  term2_orange=0
  term3_orange=0
  term4_orange=0
  term5_orange=0


  for (k in 1:Q){ nk[k] = n0 + sum(Z==k)}
  for (k in 1:Q){
    for (l in k:Q){
      # ---------------- I_kl & n_kl & eta_kl & zeta_kl & autres

      I<-I_fast(ind.all,A,Z_matrix,k,l)
      I_kl<-I$I_kl
      n_kl=I$n_kl
      m_kl <- if (k==l) 1/2*(nk[k]-n0)*(nk[k]-n0 - 1)  else (nk[k]-n0)*(nk[l]-n0)
      eta_kl = eta0 + n_kl
      zeta_kl = zeta0 + m_kl - n_kl

      sum_E_X = sum(I_kl *  dataVec)   #sum_{I_kl} Xij
      sum_E_X2 = sum(I_kl *  dataVec**2)    #sum_{I_kl} (Xij)^2
      if (n_kl == 0){ E_X = 0 ; E_X2 = 0 }
      else{  E_X = sum_E_X / n_kl
      E_X2 = sum_E_X2 / n_kl }

      sum_Ikl_4 = sum_E_X2 -2*rho*sum_E_X +(rho^2)*n_kl


      # --------------- SBM : term2
      term2_SBM = term2_SBM + lbeta(eta_kl, zeta_kl)-lbeta(eta0, zeta0)


      # --------------- Orange
      term2_orange = term2_orange + log(sigma1)*n_kl
      term3_orange = term3_orange + log(1+tau**2*n_kl/(sigma1**2))
      term4_orange = term4_orange + (1/(sigma1**2+tau**2*n_kl))*sum_Ikl_4
      term5_orange = term5_orange + ((n_kl**2)*(E_X2 - E_X**2))/(sigma1**4+(sigma1**2)*tau**2*n_kl)
    }
  }
  term1_SBM =  sum(lgamma(nk)) - lgamma(sum(nk)) - Q* lgamma(n0) + lgamma(Q*n0)

  term_SBM= term1_SBM + term2_SBM
  pi3.14 <- .BaseNamespaceEnv$pi
  term_orange = -N/2 *log(2*pi3.14) - sum((1-A)*((0.5/(sigma0**2))*dataVec^2+log(sigma0)))  -term2_orange -0.5* term3_orange  -0.5* term4_orange  -0.5 * tau**2 * term5_orange
  ICL_exact_NSBM = term_SBM + term_orange
  return(list(ICL_exact_NSBM=ICL_exact_NSBM, term_orange=term_orange, term_SBM=term_SBM))
}

#==============  When the variance is unknown (NIG case) ====================
evalICL_NIG <- function(ind.all, dataVec, Z, Z_matrix, Q, A, a, b, c, d,  n0, eta0, zeta0, sigma0){  #A=Avec

  p = length(Z)
  N <- p*(p-1)/2

  nk=rep(NA,Q)
  term2_SBM=0
  termNSBM=0


  for (k in 1:Q){ nk[k] = n0 + sum(Z==k)}
  for (k in 1:Q){
    for (l in k:Q){
      # ---------------- I_kl & n_kl & eta_kl & zeta_kl & autres

      I<-I_fast(ind.all,A,Z_matrix,k,l)
      I_kl<-I$I_kl
      n_kl=I$n_kl
      m_kl <- if (k==l) 1/2*(nk[k]-n0)*(nk[k]-n0 - 1)  else (nk[k]-n0)*(nk[l]-n0)
      eta_kl = eta0 + n_kl
      zeta_kl = zeta0 + m_kl - n_kl

      sum_E_X = sum(I_kl *  dataVec)   #sum_{I_kl} Xij
      sum_E_X2 = sum(I_kl *  dataVec**2)    #sum_{I_kl} (Xij)^2
      if (n_kl == 0){ E_X = 0 ; E_X2 = 0 }
      else{  E_X = sum_E_X / n_kl
      E_X2 = sum_E_X2 / n_kl }

      # --------------- SBM : term2
      term2_SBM = term2_SBM + lbeta(eta_kl, zeta_kl)-lbeta(eta0, zeta0)


      # --------------- NSBM
      b_x=b+n_kl
      c_x=c+0.5*n_kl
      d_x=d_post_kl(a,b,c,d,E_X,E_X2, n_kl)

      termNSBM = termNSBM + terme_log(b_x,c_x,d_x)
    }
  }
  term1_SBM =  sum(lgamma(nk)) - lgamma(sum(nk)) - Q* lgamma(n0) + lgamma(Q*n0)

  term_SBM= term1_SBM + term2_SBM
  pi3.14 <- .BaseNamespaceEnv$pi
  term_NSBM = -N/2 *log(2*pi3.14) - sum((1-A)*((0.5/(sigma0**2))*dataVec**2+log(sigma0))) - 0.5*Q*(Q+1)*terme_log(b,c,d)+termNSBM
  ICL_exact_NSBM = term_SBM + term_NSBM
  return(list(ICL_exact_NSBM=ICL_exact_NSBM, term_NSBM=term_NSBM, term_SBM=term_SBM))
}





Update_Theta = function(S, nK, lambda2, lambda3, mu_kk_diff, K_c, a = 3, rho=1,
                        maxiter=10, maxiter.AMA=5, tol=1e-2, rho.increment=1, penalty="MCP", theta.fusion=T){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: Update_Theta
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Updating the precision matrices using the ADMM algorithm after updating the mean vectors in the EM algorithm.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: AMA_XI()
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ S: p * p * K array, the estimated pseudo sample covariance matrices of given K subgroups in the iteration of the EM algorithm.
  ## @ nK: K * 1 vector, the estimated sample sizes of K subgroups in the iteration of the EM algorithm.
  ## @ lambda2: a float value, the tuning parameter controlling the sparse of the precision matrix.
  ## @ lambda3: a float value, the tuning parameter controlling the number of subgroup.
  ## @ mu_kk_diff: L2-norm differences in the mean vector between different subgroups.
  ## @ K_c: All combinations of natural numbers within K.
  ## @ a: a float value, regularization parameter in MCP, the default setting is 3.
  ## @ rho: a float value, the penalty parameter in ADMM algorithm of updating precision matrix Theta, the default setting is 1.
  ## @ tol: a float value, algorithm termination threshold.
  ## @ maxiter: int, Maximum number of cycles of the ADMM algorithm.
  ## @ maxiter.AMA: int, Maximum number of cycles of the AMA algorithm.
  ## @ penalty: The type of the penalty, which can be selected from c("MCP", "SCAD", "lasso").
  ## @ theta.fusion: Whether or not the fusion penalty term contains elements of the precision matrices.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Output:
  ## A list Theta_out including:
  ## @ Theta: p * p * K0_hat array, the estimated precision matrices of K0_hat subgroups.
  ## @ Xi: p * p * K0_hat array, the estimated dual variables of the precision matrices in ADMM.
  ## @ V_kk: L2-norm differences in the precision matrices between different subgroups.
  ## @ iters: int, the actual number of cycles of the algorithm.
  ## @ diff: a float value, the L1-norm difference between Theta and Xi.
  ## ------------------------------------------------------------------------------------------------------------------------------------------

  p = dim(S)[1]
  K = dim(S)[3]
  nkk = rep(1,K)
  # initialize Theta:
  Theta = array(0, dim = c(p, p, K))
  for (k in 1:K) { Theta[,,k] =  diag(1,p)}
  # initialize Xi:
  Xi = array(0, dim = c(p, p, K))
  # initialize Phi:
  Phi = array(0, dim = c(p, p, K))

  iter=0
  diff_value = 10
  diff_value_Xi = 10
  while( iter<maxiter && !(diff_value < tol || diff_value_Xi < tol) )
  {
    Theta.prev = Theta
    Xi.prev = Xi
    Theta=Theta.prev
    # update Theta:
    for(k.ind in 1:K){
      Sa=S[,,k.ind] - rho*Xi[,,k.ind]/nkk[k.ind] + rho*Phi[,,k.ind]/nkk[k.ind]
      edecomp = eigen(Sa)
      D = edecomp$values
      if(is.complex(D)){Sa=Symmetrize(Sa);edecomp = eigen(Sa);D = edecomp$values}
      V = edecomp$vectors
      D2 = nkk[k.ind]/(2*rho) * ( -D + sqrt(D^2 + 4*(rho/nkk[k.ind]) ) )
      Theta[,,k.ind] = V %*% diag(D2) %*% t(V)
    }
    # update Xi:
    # define B matrices:
    B = array(0, dim = c(p, p, K))
    for(k in 1:K){ B[,,k] = Theta[,,k] + Phi[,,k] }

    Xi_out_list = AMA_XI(B,K_c,lambda2,lambda3,mu_kk_diff,maxiter=maxiter.AMA, penalty=penalty, theta.fusion=theta.fusion)
    Xi = Xi_out_list$Xi
    Xi[abs(Xi) < 1e-3] <- 0
    V = Xi_out_list$V
    V_kk = round(apply(V^2,3,sum),4)

    # update the dual variable Phi:
    Phi = Phi + Theta - Xi
    iter = iter+1
    diff_value = sum(abs(Theta - Theta.prev)) / sum(abs(Theta.prev))
    diff_value_Xi = sum(abs(Xi - Xi.prev)) / (sum(abs(Xi.prev))+0.001)
    # increment rho by a constant factor:
    rho = rho*rho.increment
  }
  diff = sum(abs(Theta-Xi))
  Theta_out = list(Theta=Theta,Xi=Xi,V_kk=V_kk,diff=diff,iters=iter)
  return(Theta_out)
}

AMA_XI = function(B, K_c, lambda2, lambda3, mu_kk_diff, a = 3,
                  kappa=1, maxiter=5, tol=1e-2, penalty="MCP", theta.fusion=T){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: AMA_XI
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Solving (A.11) in Supplementary Materials using S-AMA algorithm, which is the key step of updating the precision matrices.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: S_soft()   MCP_soft()
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ B: The output of the previous step in ADMM algorithm (see (A.11) in Supplementary Materials).
  ## @ K_c: All combinations of natural numbers within K.
  ## @ lambda2: a float value, the tuning parameter controlling the sparse of the precision matrix.
  ## @ lambda3: a float value, the tuning parameter controlling the number of subgroup.
  ## @ mu_kk_diff: L2-norm differences in the mean vector between different subgroups.
  ## @ a: a float value, regularization parameter in MCP, the default setting is 3.
  ## @ kappa: a float value, the penalty parameter in S-AMA algorithm, the default setting is 1.
  ## @ tol: a float value, algorithm termination threshold.
  ## @ maxiter: int, Maximum number of cycles of the algorithm.
  ## @ penalty: The type of the penalty, which can be selected from c("MCP", "SCAD", "lasso").
  ## @ theta.fusion: Whether or not the fusion penalty term contains elements of the precision matrices.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Output:
  ## A list Xi_out including:
  ## @ Xi: p * p * K0_hat array, the estimated dual variables of the precision matrices in ADMM.
  ## @ V: p * p * K0_hat array, the estimated dual variables of Xis in S-AMA.
  ## @ Delta: Lagrange multiplier in in S-AMA.
  ## @ iters: int, the actual number of cycles of the algorithm.
  ## @ diff: a float value, the convergence criteria value.
  ## ------------------------------------------------------------------------------------------------------------------------------------------

  p = dim(B)[1]
  K = dim(B)[3]
  num_Kc = dim(K_c)[2]
  # initialize Xi:
  Xi = array(0, dim = c(p, p, K))
  for (k in 1:K) { Xi[,,k] =  diag(1,p)}
  diag_index = Xi
  # initialize V:
  V = array(0, dim = c(p, p, num_Kc))
  # initialize Delta:
  Delta = array(0, dim = c(p, p, num_Kc))

  Z = array(0, dim = c(p, p, K))
  Omega = array(0, dim = c(p, p, num_Kc))
  e_k12 = matrix(0,K,num_Kc)
  for (i in 1:K) {e_k12[i,which(K_c[1,] == i)] = 1;e_k12[i,which(K_c[2,] == i)] = -1}

  # iterations
  iter=0
  diff_value = 10
  while( iter<maxiter && diff_value > tol )
  {
    V.prev = V
    Xi.prev = Xi
    Delta.prev = Delta
    for (i in 1:p) {
      for (j in 1:p) {
        Z[i,j,] = B[i,j,] + apply(t(Delta[i,j,] * t(e_k12)),1,sum)
      }
    }

    if(penalty=="MCP"){
      Xi = S_soft(Z,lambda2)/(1-1/a) * (abs(Z) <= a*lambda2 & diag_index==0) + Z * (abs(Z) > a*lambda2 | diag_index==1)
    }
    if(penalty=="SCAD"){
      a = 3.7
      Xi = S_soft(Z,lambda2) * (abs(Z) - 2*lambda2 <= 0  & diag_index==0) +
        S_soft(Z,lambda2*a/(a-1))/(1-1/(a-1)) * (abs(Z) - 2*lambda2 > 0 & abs(Z) - 2*a*lambda2 <= 0 & diag_index==0) +
        Z * (abs(Z) - a*lambda2 > 0 | diag_index==1)
    }
    if(penalty=="lasso"){
      Xi = S_soft(Z,lambda2) * (diag_index==0) + Z * (diag_index==1)
    }

    if(!theta.fusion){lambda3 = 0}
    for (l in 1:num_Kc) {
      Omega[,,l] = Xi[,,K_c[1,l]] - Xi[,,K_c[2,l]] - Delta[,,l]/kappa
      Omegal2 = sum(Omega[,,l]^2)
      if(penalty=="MCP"){
        V[,,l] = Omega[,,l] * MCP_soft(sqrt(Omegal2 + mu_kk_diff[l]),lambda3/kappa) / (sqrt(Omegal2+mu_kk_diff[l])+1e-8)
      }
      if(penalty=="SCAD"){
        V[,,l] = Omega[,,l] * SCAD_soft(sqrt(Omegal2 + mu_kk_diff[l]),lambda3/kappa) / (sqrt(Omegal2+mu_kk_diff[l])+1e-8)
      }
      if(penalty=="lasso"){
        V[,,l] = Omega[,,l] * lasso_soft(sqrt(Omegal2 + mu_kk_diff[l]),lambda3/kappa) / (sqrt(Omegal2+mu_kk_diff[l])+1e-8)
      }
      Delta[,,l] = Delta[,,l] + kappa * ( V[,,l] - Xi[,,K_c[1,l]] + Xi[,,K_c[2,l]] )* (sum(V[,,l]^2) > 0 )
    }

    iter = iter+1
    diff_value = sum(abs(Xi - Xi.prev)^2) / sum(abs(Xi.prev)^2)
  }
  Xi_out = list(Xi=Xi,V=V,Delta=Delta,diff=diff_value,iters=iter)
  return(Xi_out)
}

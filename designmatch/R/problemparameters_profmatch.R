#! Generate the parameters for profmatch
.problemparameters_profmatch = function(mom_covs, mom_tols, mom_targets) {
  
  if (is.vector(mom_covs) == TRUE){
    mom_covs = t(as.matrix(mom_covs))
  }
  
  #! Number of units
  n = nrow(mom_covs)
  
  #! Number of dec. vars.
  n_dec_vars = n
  
  #! Coeffs. of the obj. fun., cvec
  cvec = c(rep(1, n))
  
  #! Constraint matrix, Amat
  row_ind_cur = 0
  #! Mom balance
  rows_mom = NULL
  cols_mom = NULL
  vals_mom = NULL
  n_mom_covs = ncol(mom_covs)
  k = 1
  for (i in 1:n_mom_covs) {
    #! Treated
    rows_mom_plus = rep(row_ind_cur+k, n)
    rows_mom_minus = rep(row_ind_cur+k+1, n)  
    rows_mom = c(rows_mom, rows_mom_plus, rows_mom_minus)
    cols_mom = c(cols_mom, rep(1:n, 2))
    vals_plus = c(mom_covs[, i]-mom_targets[i]-mom_tols[i])
    vals_minus = c(mom_covs[, i]-mom_targets[i]+mom_tols[i])
    vals_mom = c(vals_mom, c(vals_plus, vals_minus)) 
    k = k+2
  }
  row_ind_cur = max(rows_mom)
  
  #! Mom balance
  
  rows_ind = c(rows_mom)
  cols_ind = c(cols_mom)
  vals = c(vals_mom)
  
  aux = cbind(rows_ind, cols_ind, vals)[order(cols_ind), ]
  aux = aux[(aux[, 3] != 0),]
  Amat = simple_triplet_matrix(i = aux[, 1], j = aux[, 2], v = aux[, 3])
  
  #! Constraint vector, bvec
  bvec = NULL
  #! Mom balance
  bvec_mom = rep(0, length(unique(rows_mom)))
  bvec = c(bvec, bvec_mom)
  
  #! Sense, sense
  sense = NULL
  
  #! Mom balance
  sense_covs = rep(c("L", "G"), length(unique(rows_mom))/2)
  sense = c(sense, sense_covs)
  
  #! Variable types, vtype
  vtype = rep("B", n_dec_vars)
  
  # Output
  return(list(n = n, 
              n_dec_vars = n_dec_vars,
              cvec = cvec, 
              Amat = Amat, 
              bvec = bvec, 
              sense = sense,
              vtype = vtype))
  
}

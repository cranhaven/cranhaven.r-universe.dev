#! Generate the parameters for cardmatch
.problemparameters_cardmatch = function(t_ind, mom_covs, mom_tols, mom_targets, fine_covs) {
  
  #! Number of treated units and controls
  n_t = sum(t_ind)
  n_c = length(t_ind)-n_t
  
  #! Number of dec. vars.
  n_dec_vars = n_t+n_c
  
  #! Coeffs. of the obj. fun., cvec
  cvec = c(rep(1, n_t), rep(0, n_c))
  
  #! Constraint matrix, Amat
  row_ind_cur = 0
  #! Mom balance
  if (!is.null(mom_covs)) {
    rows_mom = NULL
    cols_mom = NULL
    vals_mom = NULL
    n_mom_covs = ncol(mom_covs)
    k = 1
    if (!is.null(mom_targets)) {
      for (i in 1:n_mom_covs) {
        #! Treated
        rows_mom_plus = rep(row_ind_cur+k, n_t)
        rows_mom_minus = rep(row_ind_cur+k+1, n_t)  
        rows_mom = c(rows_mom, rows_mom_plus, rows_mom_minus)
        cols_mom = c(cols_mom, rep(1:n_t, 2))
        vals_plus = c(mom_covs[t_ind==1, i]-mom_targets[i]-mom_tols[i])
        vals_minus = c(mom_covs[t_ind==1, i]-mom_targets[i]+mom_tols[i])
        vals_mom = c(vals_mom, c(vals_plus, vals_minus)) 
        #! Controls
        rows_mom_plus = rep(row_ind_cur+k+2, n_c)
        rows_mom_minus = rep(row_ind_cur+k+3, n_c)  
        rows_mom = c(rows_mom, rows_mom_plus, rows_mom_minus)
        cols_mom = c(cols_mom, rep(n_t+(1:n_c), 2))
        vals_plus = c(mom_covs[t_ind==0, i]-mom_targets[i]-mom_tols[i])
        vals_minus = c(mom_covs[t_ind==0, i]-mom_targets[i]+mom_tols[i])
        vals_mom = c(vals_mom, c(vals_plus, vals_minus))       
        k = k+4
      }
    }
    if (is.null(mom_targets)) {
      for (i in 1:n_mom_covs) {
        rows_mom_plus = rep(row_ind_cur+k, n_t+n_c)
        rows_mom_minus = rep(row_ind_cur+k+1, n_t+n_c)  
        rows_mom = c(rows_mom, rows_mom_plus, rows_mom_minus)
        cols_mom = c(cols_mom, rep(1:(n_t+n_c), 2))
        vals_plus = c(mom_covs[t_ind==1, i]-mom_tols[i], -mom_covs[t_ind==0, i])
        vals_minus = c(mom_covs[t_ind==1, i]+mom_tols[i], -mom_covs[t_ind==0, i])
        vals_mom = c(vals_mom, c(vals_plus, vals_minus))     
        k = k+2
      }
    }    
    row_ind_cur = max(rows_mom)
  }
  #! Fine balance    
  if (!is.null(fine_covs)) {
    rows_fine = NULL
    cols_fine = NULL
    vals_fine = NULL
    n_fine_covs = ncol(fine_covs)
    k = 1
    for (i in 1:n_fine_covs) {
      fine_covs_cats = unique(fine_covs[, i])	
      for (j in fine_covs_cats) {
        cols_fine_aux = which(fine_covs[, i]==j)
        rows_fine = c(rows_fine, rep(row_ind_cur+k, length(cols_fine_aux)))
        cols_fine = c(cols_fine, cols_fine_aux)
        vals_fine = c(vals_fine, c(rep(1, sum(cols_fine_aux<=n_t)), rep(-1, sum(cols_fine_aux>n_t))))
        k = k+1
      }
    }
    row_ind_cur = max(rows_fine)
  }
  #! Equal number of matched controls and matched treated units
  if (!is.null(mom_covs) & is.null(fine_covs)) {
    rows_equal = rep(row_ind_cur+1, n_dec_vars)
    cols_equal = 1:n_dec_vars
    vals_equal = c(rep(1, n_t), rep(-1, n_c))
  }
  #! Put together
  rows_ind = NULL
  cols_ind = NULL
  vals = NULL
  #! Mom balance      
  if (!is.null(mom_covs)) {
    rows_ind = c(rows_ind, rows_mom)
    cols_ind = c(cols_ind, cols_mom)
    vals = c(vals, vals_mom)
  }
  #! Fine balance      
  if (!is.null(fine_covs)) {
    rows_ind = c(rows_ind, rows_fine)
    cols_ind = c(cols_ind, cols_fine)
    vals = c(vals, vals_fine)
  }
  #! Equal number of matched controls and matched treated units
  if (!is.null(mom_covs) & is.null(fine_covs)) {
    rows_ind = c(rows_ind, rows_equal)
    cols_ind = c(cols_ind, cols_equal)
    vals = c(vals, vals_equal)
  }    
  #! 
  aux = cbind(rows_ind, cols_ind, vals)[order(cols_ind), ]
  aux = aux[(aux[, 3] != 0),]
  Amat = simple_triplet_matrix(i = aux[, 1], j = aux[, 2], v = aux[, 3])
        
  #! Constraint vector, bvec
  bvec = NULL
  #! Mom balance
  if (!is.null(mom_covs)) {
    bvec_mom = rep(0, length(unique(rows_mom)))
    bvec = c(bvec, bvec_mom)
  }
  #! Fine balance
  if (!is.null(fine_covs)) {
    bvec_fine = rep(0, length(unique(rows_fine)))
    bvec = c(bvec, bvec_fine)
  }
  #! Equal number of matched controls and matched treated units
  if (!is.null(mom_covs) & is.null(fine_covs)) {
    bvec = c(bvec, 0)
  }
    
  #! Sense, sense
  sense = NULL
  #! Mom balance
  if (!is.null(mom_covs)) {
  	if (!is.null(mom_targets)) {
  	  sense_covs = rep(c("L", "G", "L", "G"), length(unique(rows_mom))/4)
      sense = c(sense, sense_covs)
    }   	
  	if (is.null(mom_targets)) {
  	  sense_covs = rep(c("L", "G"), length(unique(rows_mom))/2)
      sense = c(sense, sense_covs)
    } 
  }
  #! Fine balance
  if (!is.null(fine_covs)) {
  	sense_fine = rep("E", length(unique(rows_fine)))
    sense = c(sense, sense_fine) 
  }
  if (!is.null(mom_covs) & is.null(fine_covs)) {
    sense_equal = c("E")
    sense = c(sense, sense_equal) 
  }
  
  #! Variable types, vtype
  vtype = rep("B", n_dec_vars)
    
  # Output
  return(list(n_t = n_t, 
              n_c = n_c,  
              n_dec_vars = n_dec_vars,
              cvec = cvec, 
              Amat = Amat, 
              bvec = bvec, 
              sense = sense,
              vtype = vtype))
              
}
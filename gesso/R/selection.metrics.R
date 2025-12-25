
auroc_ = function(estimated_coef, true_coef) {
  score = abs(estimated_coef); bool = true_coef != 0
  n1 = sum(!bool)
  n2 = sum(bool)
  U = sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

selection.metrics = function(true_b_g, true_b_gxe, estimated_b_g, estimated_b_gxe) {
  index_beta_gxe_non_zero = which(true_b_gxe != 0)
  index_beta_non_zero = which(true_b_g != 0)
  index_beta_gxe_zero = which(true_b_gxe == 0)
  index_beta_zero = which(true_b_g == 0)

    
  if (sum(estimated_b_gxe != 0) == 0) {
    precision_gxe = 1.0
  } else {
    precision_gxe = sum(estimated_b_gxe[index_beta_gxe_non_zero] != 0) / sum(estimated_b_gxe != 0)
  }
  if (sum(estimated_b_g != 0) == 0) {
    precision_g = 1.0
  } else {
    precision_g = sum(estimated_b_g[index_beta_non_zero] != 0) / sum(estimated_b_g != 0)
  }  
  
  return(list(b_g_non_zero=sum(estimated_b_g != 0),
              b_gxe_non_zero=sum(estimated_b_gxe != 0),
              mse_b_g = sqrt(mean((estimated_b_g[index_beta_non_zero] - true_b_g[index_beta_non_zero])^2)),
              mse_b_gxe = sqrt(mean((estimated_b_gxe[index_beta_gxe_non_zero] - true_b_gxe[index_beta_gxe_non_zero])^2)),
              
              sensitivity_g = sum(abs(estimated_b_g[index_beta_non_zero]) != 0)/(length(index_beta_non_zero) + 1e-8),
              specificity_g = sum(abs(estimated_b_g[index_beta_zero]) == 0)/(length(index_beta_zero) + 1e-8),
              precision_g = precision_g,
              sensitivity_gxe = sum(abs(estimated_b_gxe[index_beta_gxe_non_zero]) != 0)/(length(index_beta_gxe_non_zero) + 1e-8),
              specificity_gxe = sum(abs(estimated_b_gxe[index_beta_gxe_zero]) == 0)/(length(index_beta_gxe_zero) + 1e-8),
              precision_gxe = precision_gxe,
              
              auc_g=auroc_(estimated_b_g, true_b_g),
              auc_gxe=auroc_(estimated_b_gxe, true_b_gxe)))
}


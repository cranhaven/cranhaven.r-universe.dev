### function input: G, C, z, N
### parameters estimates: ss.diag, ss.off, sss, mean.diag, mean.off, ss.bound 
### function output : List of Cov Matrix 1,....,C.   
Gcov_c = function(GG, C, z, N)  
{
  
  ss.diag = rep(1,C) 
  mean.diag = rep(1,C)
  ss.off = ss.bound = nn = matrix(0, nrow = C, ncol = C)
  mean.off = matrix(0, nrow = C, ncol = C)
  sss = nnn = array(0,dim = c(C, C, C))
  Cov = list()
  
  # step one - get intiail gamma and labels
  step_one = get_gamma_labels_c(z, C)
  labels = c(step_one$labels)
  gamma = lapply(step_one$gamma, c)
  

  # step two  
  C = max(labels)
  step_two = step_two_c(C, gamma, GG)
  ss.off = step_two$ss_off
  ss.diag = c(step_two$ss_diag)
  mean.off = step_two$mean_off
  mean.diag = c(step_two$mean_diag)
  
  ss.off =  ss.off + t(ss.off) - diag(diag(ss.off))
  
  ####### ss.diag and ss.off are completely cluster specific and symmetric 
  ###Boundary Covariance Estimates:  non-symmetric
  ###Total number of Boundary parameters: C  
  # step three
  step_three = step_three_c(C, gamma, GG, mean.off, mean.diag)
  ss.bound = step_three$ss_bound
  
  #Offdiagonals Estimates: symmetric 
  ## 1<=ii <=jj <= kk <= C 
  ## Total number of parameters = C choose 3 + 2*C choose 2 + C choose 1. 
  ## Hardest part. 
  
  step_four = step_four_c(C, gamma, GG, mean.off, mean.diag)
  sss = step_four$sss
  
  step_five = step_five_c(C, gamma, ss.off, ss.diag, labels, sss, ss.bound, N)
  Cov = step_five$Cov
  
  return(list(Cov = Cov, gamma = gamma))
}
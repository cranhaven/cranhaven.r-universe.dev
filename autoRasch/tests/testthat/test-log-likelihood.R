test_that("R and Rcpp log-likelihood and gradient implementations give the same result", {
  
  # source('../R/gpcmdif.R')
  # source('../R/pjmle.R')
  # source('../R/loglik_function.R')
  # loglik_fast <- function() {
  
  # generate example to work with
  dset <- autoRasch::polydif_inh_dset[,c(14:17,19)]
  V <- nrow(autoRasch::polydif_inh_dset)
  X <- as.matrix(dset)
  
  setting <- autoRasch::autoRaschOptions()
  setting$isHessian <- FALSE
  
  groups_map <- c(rep(1, V/2),rep(0, V/2))
  
  dataPrep <- data_prep(dset = dset, fixed_par = setting$fixed_par, groups_map = groups_map, method = "novel")
  
  I <- ncol(dataPrep$dset)
  J <- dataPrep$n_th
  G <- ncol(dataPrep$groups_map)
  
  set.seed(2021) # for reproducibility
  theta <- runif(V,-1,1) * setting$random.init.th
  beta <- runif(I*J,-1,1) * setting$random.init.th
  beta_mat <- matrix(beta, I, J, byrow = TRUE) # maybe we should agree on by column instead
  gamma <- runif(I,-1,1) * setting$random.init.th
  delta <- runif(I * G, -1, 1) * setting$random.init.th
  delta_mat <- matrix(delta, I, G, byrow = TRUE)
  
  # NOTE: this whole part dealing with the fixed parameters has to be written separately
  nlmPar <- c(theta,beta,gamma,delta)
  length_theta <- length(theta)
  length_beta <- length(beta)
  length_gamma <- length(gamma)
  length_delta <- length(delta)
  length_array <- c(length_theta,length_beta,length_gamma,length_delta)
  fullPar_arr <- c("theta","beta","gamma","delta")

  if(is.null(setting$fixed_par)){
    estPar_arr <- fullPar_arr
  } else {
    estPar_arr <- fullPar_arr[-c(which(fullPar_arr %in% setting$fixed_par))]
  }
  fixLength_arr <- length_array[c(which(fullPar_arr %in% setting$fixed_par))]
  
  fixValue <- c()
  estLength_array <- length_array
  
  ll_classic <- loglik_fun(nlmPar, dset, setting, dataPrep,
              estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array)
  ll_novel <- loglik_fun_novel(nlmPar, dataPrep$dset, setting$lambda_theta, setting$lambda_in, setting$lambda_out, setting$lambda_delta,
                                  estPar_arr, setting$fixed_par, fixValue, fixLength_arr, estLength_array,
                                  dataPrep$groups_map, dataPrep$mt_vek, dataPrep$mt_idx, dataPrep$dimResp,
                                  dataPrep$allcat, dataPrep$n_th, dataPrep$XN, dataPrep$XNA, setting$eps,
                                  setting$isPenalized_gamma, setting$isPenalized_theta, setting$isPenalized_delta)
  ll_fast <- loglik_fun_fast(nlmPar, dset, estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array, setting, dataPrep)
  
  expect_equal(ll_classic, ll_novel)
  expect_equal(ll_classic, ll_fast)
  
  gr_classic <- grad_fun(nlmPar, dset, setting, dataPrep,
                           estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array)
  gr_novel <- grad_fun_novel(nlmPar, dataPrep$dset, setting$lambda_theta, setting$lambda_in, setting$lambda_out, setting$lambda_delta,
                               estPar_arr, setting$fixed_par, fixValue, fixLength_arr, estLength_array,
                               dataPrep$groups_map, dataPrep$mt_vek, dataPrep$mt_idx, dataPrep$dimResp,
                               dataPrep$allcat, dataPrep$n_th, dataPrep$XN, dataPrep$XNA, setting$eps,
                               setting$isPenalized_gamma, setting$isPenalized_theta, setting$isPenalized_delta)
  gr_fast <- grad_fun_fast(nlmPar, dset, estPar_arr, fixed_par, fixValue, fixLength_arr, estLength_array, setting, dataPrep)
  
  expect_equal(gr_classic, gr_novel, ignore_attr = TRUE)
  expect_equal(gr_classic, gr_fast, ignore_attr = TRUE)
})

################################################################################
#                                                                              #
#                 DCSmooth Package: Parallelization Functions                  #
#                                                                              #
################################################################################

  # parallel.LP.const0
  # LP.const0.list
  # parallel.LP.const1
  # LP.const1.list
  # parallel.KR.const0
  # KR.const0.list
  # parallel.KR.const1
  # KR.const1.list

#------------------------Local Polynomial Regression---------------------------#

utils::globalVariables('list_index_parallel')

parallel.LP.const0 = function(opts_list, Y)
{
  n_core = parallel::detectCores() - 1
  doParallel::registerDoParallel(n_core)
  `%dopar%` = foreach::`%dopar%`
  
  output = foreach::foreach(list_index_parallel = 1:3,
                            .packages = c('DCSmooth')) %dopar% {
    opts_list_i = opts_list[[list_index_parallel]]
    opts_list_i$weight_x = weight_fcn_assign(opts_list_i$weight_x)
    opts_list_i$weight_t = weight_fcn_assign(opts_list_i$weight_t)
    result = LP.const0.list(opts_list_i, Y)
    return(result)
  }
  
  doParallel::stopImplicitCluster()
  
  return(output)
}

LP.const0.list = function(opts_list, Y)
{
  LP_dcs_const0_BMod(yMat = Y,
                     hVec = opts_list$h,
                     polyOrderVec = opts_list$p,
                     drvVec = opts_list$drv,
                     muVec = opts_list$mu,
                     weightFcnPtr_x = opts_list$weight_x,
                     weightFcnPtr_t = opts_list$weight_t)
}

parallel.LP.const1 = function(opts_list, Y, n_core)
{
  n_core = parallel::detectCores() - 1
  doParallel::registerDoParallel(n_core)
  `%dopar%` = foreach::`%dopar%`
  
  output = foreach::foreach(list_index_parallel = 1:3,
                            .packages = c('DCSmooth')) %dopar% {
    opts_list_i = opts_list[[list_index_parallel]]
    opts_list_i$weight_x = weight_fcn_assign(opts_list_i$weight_x)
    opts_list_i$weight_t = weight_fcn_assign(opts_list_i$weight_t)
    result = LP.const1.list(opts_list_i, Y)
    return(result)
  }
  
  doParallel::stopImplicitCluster()
  
  return(output)
}

LP.const1.list = function(opts_list, Y)
{
  LP_dcs_const1_BMod(yMat = Y,
                     hVec = opts_list$h,
                     polyOrderVec = opts_list$p,
                     drvVec = opts_list$drv,
                     muVec = opts_list$mu,
                     weightFcnPtr_x = opts_list$weight_x,
                     weightFcnPtr_t = opts_list$weight_t)
}

#-----------------------------Kernel Regression--------------------------------#

parallel.KR.const0 = function(opts_list, Y)
{
  n_core = parallel::detectCores() - 1
  doParallel::registerDoParallel(n_core)
  `%dopar%` = foreach::`%dopar%`
  
  output = foreach::foreach(list_index_parallel = 1:3,
                            .packages = c('DCSmooth')) %dopar% {
    opts_list_i = opts_list[[list_index_parallel]]
    opts_list_i$kernel_x = kernel_fcn_assign(opts_list_i$kernel_x)
    opts_list_i$kernel_t = kernel_fcn_assign(opts_list_i$kernel_t)
    result = KR.const0.list(opts_list_i, Y)
    return(result)
  }
  
  doParallel::stopImplicitCluster()
  
  return(output)
}

KR.const0.list = function(opts_list, Y)
{
  KR_dcs_const0(yMat = Y,
                hVec = opts_list$h,
                drvVec = opts_list$drv,
                kernFcnPtrX = opts_list$kernel_x,
                kernFcnPtrT = opts_list$kernel_t)
}

parallel.KR.const1 = function(opts_list, Y, n_core)
{
  n_core = parallel::detectCores() - 1
  doParallel::registerDoParallel(n_core)
  `%dopar%` = foreach::`%dopar%`
  
  output = foreach::foreach(list_index_parallel = 1:3,
                            .packages = c('DCSmooth')) %dopar% {
    opts_list_i = opts_list[[list_index_parallel]]
    opts_list_i$kernel_x = kernel_fcn_assign(opts_list_i$kernel_x)
    opts_list_i$kernel_t = kernel_fcn_assign(opts_list_i$kernel_t)
    result = KR.const1.list(opts_list_i, Y)
    return(result)
  }
  
  doParallel::stopImplicitCluster()
  
  return(output)
}

KR.const1.list = function(opts_list, Y)
{
  KR_dcs_const1(yMat = Y,
                hVec = opts_list$h,
                drvVec = opts_list$drv,
                kernFcnPtrX = opts_list$kernel_x,
                kernFcnPtrT = opts_list$kernel_t)
}
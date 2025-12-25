
auroc = function(score, bool) {
 n1 = sum(!bool)
 n2 = sum(bool)
 U  = sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
 return(1 - U / n1 / n2)
}

get.matrix.type = function(G) {
  if (is(G, "matrix")) {
    if (typeof(G) != "double")
      stop("G must be of type double")
    mattype_g = 0
  } else if ("dgCMatrix" %in% class(G)) {
    if (typeof(G@x) != "double")
      stop("G must be of type double")
    mattype_g = 1
  } else if (is.big.matrix(G)) {
    if (bigmemory::describe(G)@description$type != "double")
      stop("G must be of type double")
    mattype_g = 2
  } else {
    stop("G must be a standard R matrix, big.matrix, filebacked.big.matrix, or dgCMatrix. If G is a data frame, use as.matrix(G).")
  }
  return(mattype_g)
}

check.is.matrix = function(X, name) {
  if (is(X, "matrix")) {
    if (typeof(X) != "double")
      stop(paste0(name, " must be of type double"))
    mattype_g = 0
  } else {
    stop(paste0(name, " must be a standard R matrix, big.matrix, filebacked.big.matrix, or dgCMatrix. If ", name, " is a data.frame, use as.matrix(", name, ")."))
  }
}

compute.grid = function(G, E, Y, C, normalize, family, grid_size, grid_min_ratio) {
  mattype_g = get.matrix.type(G)
  Y = as.double(Y)
  E = as.double(E)
  if (is.null(C)) {
    C = matrix(numeric(0), nrow=length(Y), ncol=0)
  } else {
    check.is.matrix(C, "C")    
  }
  n = dim(G)[1]
  weights = rep(1, n) / n
  lambda_max = computeLambdaMax(G=G, E=E, Y=Y, C=C,
                                weights=weights, normalize=normalize,
                                family=family, mattype_g=mattype_g)
  lambda_min = grid_min_ratio * lambda_max
  grid = 10^seq(log10(lambda_min), log10(lambda_max), length.out=grid_size)
  return(grid)
}


gesso.fit = function(G, E, Y, C=NULL, normalize=TRUE, normalize_response=FALSE,
                     grid=NULL, grid_size=20, grid_min_ratio=NULL, 
                     alpha=NULL, family="gaussian", weights=NULL,
                     tolerance=1e-3, max_iterations=5000, 
                     min_working_set_size=100,
                     verbose=FALSE) {
  mattype_g = get.matrix.type(G)
  Y = as.double(Y)
  E = as.double(E)
  if (normalize_response) {tolerance = tolerance * sqrt(sum(Y^2)/length(Y) - mean(Y)^2)}
  if (is.null(grid)) {
    if(is.null(grid_min_ratio)){
      grid_min_ratio = ifelse(dim(G)[1] < dim(G)[2], 0.1, 0.01)
    }
    if (verbose) {
      start = Sys.time()
      cat("Compute grid: ", "\n")
    }
    grid = compute.grid(G=G, E=E, Y=Y, C=C,
                        normalize=normalize, family=family,
                        grid_size=grid_size, grid_min_ratio=grid_min_ratio)
    if (verbose) {
      print(Sys.time() - start)
    }
  }
  n = dim(G)[1]
  if (is.null(weights)) {
    weights = rep(1, n) / n
  }
  if (is.null(C)) {
    C = matrix(numeric(0), nrow=length(Y), ncol=0)
  } else {
    check.is.matrix(C, "C")    
  }
  if (is.null(alpha)) {
    alpha = -1
  }

  fit = fitModel(G=G, E=E, Y=Y, C=C,
                 weights=weights, normalize=normalize,
                 grid=grid,
                 alpha=alpha,
                 family=family,
                 tolerance=tolerance,
                 max_iterations=max_iterations, min_working_set_size=min_working_set_size,
                 mattype_g=mattype_g)
  return(fit)
}

gesso.cv = function(G, E, Y, C=NULL, normalize=TRUE, normalize_response=FALSE,
                    grid=NULL, grid_size=20, grid_min_ratio=NULL, 
                    alpha=NULL,
                    family="gaussian", type_measure="loss",
                    fold_ids=NULL, nfolds=4, parallel=TRUE, seed=42,
                    tolerance=1e-3, max_iterations=5000, 
                    min_working_set_size=100,
                    verbose=TRUE) {
  set.seed(seed)
  mattype_g = get.matrix.type(G)
  Y = as.double(Y)
  E = as.double(E)
  if (normalize_response) {tolerance = tolerance * sqrt(sum(Y^2)/length(Y) - mean(Y)^2)}
  if (is.null(grid)) {
    if(is.null(grid_min_ratio)){
      grid_min_ratio = ifelse(dim(G)[1] < dim(G)[2], 0.1, 0.01)
    }
    if (verbose) {
      start = Sys.time()
      cat("Compute grid: ", "\n")
    }
    grid = compute.grid(G=G, E=E, Y=Y, C=C,
                        normalize=normalize, family=family,
                        grid_size=grid_size, grid_min_ratio=grid_min_ratio)
    if (verbose) {
      print(Sys.time() - start)
    }
  }
  if (is.null(alpha)) {
    alpha = -1
  }  

  if (nfolds < 2) {
    stop("number of folds (nfolds) must be at least 2")
  }
  
  if (is.null(C)) {
    C = matrix(numeric(0), nrow=length(Y), ncol=0)
  } else {
    check.is.matrix(C, "C")    
  }
  
  if (is.null(fold_ids)) {
    if (family == "gaussian") {
      fold_ids = sample(seq(1, dim(G)[1]) %% nfolds, replace=FALSE)
    } else {
      fold_ids = c()
      fold_ids[Y == 0] = sample(seq(1, sum(Y == 0)) %% nfolds, replace=FALSE)
      fold_ids[Y == 1] = sample(seq(1, sum(Y == 1)) %% nfolds, replace=FALSE)
    }
  } else {
    nfolds = max(fold_ids)
    fold_ids = fold_ids - 1
  }

  if (parallel) {
   if (verbose) {
     cat("Parallel cv:", "\n")
     start_parallel = Sys.time()
    }
    result = fitModelCV(G=G, E=E, Y=Y, C=C, normalize=normalize, 
                        grid=grid, alpha=alpha, family=family, tolerance=tolerance, 
                        max_iterations=max_iterations, min_working_set_size=min_working_set_size,
                        fold_ids=fold_ids, seed=seed, ncores=nfolds, mattype_g=mattype_g)
    if (verbose) {print(Sys.time() - start_parallel)}
  } else {
    if (verbose) {
      cat("Non-parallel cv:", "\n")
      start_nparallel = Sys.time()
    }
    result = fitModelCV(G=G, E=E, Y=Y, C=C, normalize=normalize, 
                        grid=grid, alpha=alpha, family=family, tolerance=tolerance, 
                        max_iterations=max_iterations, min_working_set_size=min_working_set_size,
                        fold_ids=fold_ids, seed=seed, ncores=1, mattype_g=mattype_g)
    if (verbose) {print(Sys.time() - start_nparallel)}
  }
  fold_ids = fold_ids + 1
  
  if (type_measure == "loss") {
    result_ = colMeans(result$test_loss)
  } else if (type_measure == "auc") {
    if (family != "binomial") {
      stop("type_measure == 'auc' is only for binomial family")
    }
  
    auc_per_fold = c()
    for (fold_id in 1:nfolds) {
      Y_fold = Y[fold_ids == fold_id]
      auc_per_fold = rbind(auc_per_fold,
                            apply(result$xbeta[fold_ids == fold_id,], 2,
                                  function(x) auroc(x, Y_fold)))
    }
    result_ = colMeans(auc_per_fold)
  } else {
    stop("Unknown type_measure: '", type_measure, "'")
  }
  mean_beta_g_nonzero = colMeans(result$beta_g_nonzero)
  mean_beta_gxe_nonzero = colMeans(result$beta_gxe_nonzero)
  
  n = dim(G)[1] 
  weights = rep(1, n)
  weights = weights / sum(weights)
  if (verbose) {
    cat("Fit on the full dataset:", "\n")
    start_all = Sys.time()
  }
  
  fit_all_data = fitModel(G=G, E=E, Y=Y, C=C,
                          weights=weights, normalize=normalize,
                          grid=grid, alpha=alpha, family=family,
                          tolerance=tolerance,
                          max_iterations=max_iterations, min_working_set_size=min_working_set_size,
                          mattype_g=mattype_g)
  if (verbose) {
    print(Sys.time() - start_all)
  }

  if (type_measure == "loss") {
    lambda_min_index = which.min(result_)
  } else {
    lambda_min_index = which.max(result_)
  }
  
  loss_min = result_[lambda_min_index]
  
  result_table = tibble(lambda_1=fit_all_data$lambda_1,
                        lambda_2=fit_all_data$lambda_2, 
                        mean_loss=result_,
                        mean_beta_g_nonzero=mean_beta_g_nonzero,
                        mean_beta_gxe_nonzero=mean_beta_gxe_nonzero) 
  

  lambda_min = result_table[lambda_min_index, 1:2]
  
  result$fold_ids = fold_ids
  
  return(list(cv_result=result_table,
              lambda_min=lambda_min, 
              fit=fit_all_data,
              grid=grid,
              full_cv_result=result,
              type_measure=type_measure))
}

gesso.coef = function(fit, lambda){
 lambda_idx = which(fit$lambda_1 == lambda$lambda_1 & fit$lambda_2 == lambda$lambda_2)
 beta_0 = fit$beta_0[lambda_idx]
 beta_e = fit$beta_e[lambda_idx]
 beta_g = fit$beta_g[,lambda_idx]
 beta_c = fit$beta_c[,lambda_idx]
 beta_gxe = fit$beta_gxe[,lambda_idx]
 
 return(list(beta_0=beta_0, beta_e=beta_e, beta_g=beta_g, beta_c=beta_c, beta_gxe=beta_gxe))
}
  
gesso.coefnum = function(cv_model, target_b_gxe_non_zero, less_than=TRUE){
  type_measure = cv_model$type_measure
  cv_result = cv_model$cv_result; fit = cv_model$fit
  if (type_measure == "loss"){
    if (less_than){
      best_lambdas = cv_result %>%
        filter(.data$mean_beta_gxe_nonzero <= target_b_gxe_non_zero) %>%
        arrange(.data$mean_loss) %>%
        dplyr::slice(1) %>%
        dplyr::select(.data$lambda_1, .data$lambda_2)
    } else {
      best_lambdas = cv_result %>%
        filter(.data$mean_beta_gxe_nonzero >= target_b_gxe_non_zero) %>%
        arrange(.data$mean_loss) %>%
        dplyr::slice(1) %>%
        dplyr::select(.data$lambda_1, .data$lambda_2)
    }
  } else {
    if (less_than){
      best_lambdas = cv_result %>%
        filter(.data$mean_beta_gxe_nonzero <= target_b_gxe_non_zero) %>%
        arrange(desc(.data$mean_loss)) %>%
        dplyr::slice(1) %>%
        dplyr::select(.data$lambda_1, .data$lambda_2)
    } else {
      best_lambdas = cv_result %>%
        filter(.data$mean_beta_gxe_nonzero >= target_b_gxe_non_zero) %>%
        arrange(desc(.data$mean_loss)) %>%
        dplyr::slice(1) %>%
        dplyr::select(.data$lambda_1, .data$lambda_2)
    }
  }
  
  return(gesso.coef(fit, best_lambdas))
}

gesso.predict = function(beta_0, beta_e, beta_g, beta_gxe, new_G, new_E, beta_c=NULL, new_C=NULL,
                              family="gaussian"){
  new_GxE = new_G * new_E
  if (is.null(new_C)){
    lp = (beta_0 + beta_e * new_E +  new_G %*% beta_g + new_GxE %*% beta_gxe)[,1]
  } else {
    lp = (beta_0 + beta_e * new_E +  new_G %*% beta_g + new_C %*% beta_c + new_GxE %*% beta_gxe)[,1]
  }
  
  if (family == "gaussian"){
    return(lp)
  } else if (family == "binomial"){
    prob = 1/(1 + exp(-lp))
    return(prob)
  }
}




 




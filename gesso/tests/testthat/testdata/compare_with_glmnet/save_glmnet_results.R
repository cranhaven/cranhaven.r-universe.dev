library(glmnet)

linear.predictor = function(X, b_0, b_x) {
  return(b_0 + X %*% b_x)
}

glmnet.loss = function(G, Y, b_0, b_x, lambda, penalty.factor, family="gaussian") {
  n = dim(G)[1]
  xbeta = linear.predictor(G, b_0, b_x)
  penalty_loss = lambda * (abs(b_x) %*% penalty.factor)[1,1]
  if (family == "gaussian"){
    loss = sum((Y - xbeta)^2) / (2 * n)
  }
  if (family == "binomial"){
    loss = sum(log(1 + exp(xbeta)) - Y * xbeta) / n
  }
  return(loss + penalty_loss)
}

grid_size = 10
grid = 10^seq(-4, log10(1), length.out=grid_size) 
grid = rev(grid)
max_iterations = 20000
tol = 1e-5

for (family in c("gaussian", "binomial")){
  for (seed in 1:20) {
    if (seed <= 5) {
      sample_size = 200
      p = 50
      n_confounders = NULL
    } else if (seed <= 10) {
      sample_size = 200
      p = 50
      n_confounders = 2
    } else if (seed <= 15) {
      sample_size = 100
      p = 500
      n_confounders = 5
    } else {
      sample_size = 200
      p = 500
      n_confounders = 10
    }
    
    cat("-", seed, family, "\n")
    data = data.gen(sample_size=sample_size, p=p, 
                    n_g_non_zero=10, n_gxe_non_zero=4, 
                    seed=seed,
                    family=family,
                    n_confounders=n_confounders,
                    normalize=TRUE)
    
    file_name = paste0("tests/testthat/testdata/compare_with_glmnet/", seed, "_", family, "_data.rds")
    saveRDS(data, file_name)
    
    start = Sys.time()
    fit = hierNetGxE.fit(G=data$G_train, E=rep(0, sample_size),
                         Y=data$Y_train, C=data$C_train,
                         tolerance=tol, grid=grid, family=family, 
                         normalize=FALSE,
                         max_iterations=max_iterations)
    cat("-- hierNetGxE.fit done in ", Sys.time() - start, " seconds. num not converged ", sum(1 - fit$has_converged), "\n")
    
    glmnet_X = cbind(data$G_train, data$C_train)
    penalty.factor = rep(0, ncol(glmnet_X))
    penalty.factor[1:ncol(data$G_train)] = 1
    glmnet_fit = glmnet(x=glmnet_X, y=data$Y_train,
                        lambda=grid, thresh=1e-10,
                        intercept=TRUE, standardize.response=FALSE,
                        standardize=FALSE,
                        family=family,
                        penalty.factor=penalty.factor)
    objective_value = c()
    for (i in 1:grid_size) {
      cur_objective_value = glmnet.loss(glmnet_X, data$Y_train,
                                        glmnet_fit$a0[i], glmnet_fit$beta[,i],
                                        glmnet_fit$lambda[i], 
                                        penalty.factor=penalty.factor,
                                        family=family)
      objective_value = c(objective_value, cur_objective_value)
    }
    cat("-- max difference in loss", max(fit$objective_value - rep(objective_value, rep(grid_size, grid_size))), "\n")
    
    file_name = paste0("tests/testthat/testdata/compare_with_glmnet/", seed, "_", family, "_glmnet_results.rds")
    saveRDS(list(objective_value=objective_value), file_name)
  }
}

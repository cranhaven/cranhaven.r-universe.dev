context("cross-validation")

test_that("cv and fit return same results on individual folds", {
  tol = 1e-3
  grid_size = 5
  max_iterations = 5000
  sample_size = 100
  n_g_non_zero = 10
  n_gxe_non_zero = 5
  p = 30
  nfolds = 3
  for (seed in 1:2) {
    for (family in c("gaussian", "binomial")) {
      for (type_measure in c("loss", "auc")) {
        if (type_measure == "auc" & family != "binomial") {
          next
        }
        for (alpha in c(-1, 0.5)) {
          data = data.gen(seed=seed, sample_size=sample_size,
                          family=family, normalize=TRUE)
          
          cv = gesso.cv(data$G_train, data$E_train, data$Y_train,
                        tolerance=tol, normalize=FALSE, grid_size=grid_size, alpha=alpha,
                        family=family, nfolds=nfolds, seed=seed, 
                        type_measure=type_measure,
                        verbose=FALSE)
          expect_equal(sum(cv$full_cv_result$has_converged != 1), 0)
          
          for (fold_id in 1:nfolds) {
            weights = rep(0, sample_size)
            weights[cv$full_cv_result$fold_ids != fold_id] = 1
            weights = weights / sum(weights)
            
            fit = gesso.fit(data$G_train, data$E_train, data$Y_train,
                                 tolerance=tol,
                                 grid=cv$grid,
                                 alpha=alpha,
                                 family=family,
                                 max_iterations=max_iterations,
                                 weights=weights,
                                 normalize=FALSE)
            expect_equal(sum(fit$has_converged != 1), 0)
            
            test_G = data$G_train[cv$full_cv_result$fold_ids == fold_id,]
            test_E = data$E_train[cv$full_cv_result$fold_ids == fold_id]
            test_Y = data$Y_train[cv$full_cv_result$fold_ids == fold_id]
            test_sample_size = sum(cv$full_cv_result$fold_ids == fold_id)
            test_loss = rep(0, length(fit$lambda_1))
            
            for (i in 1:length(fit$lambda_1)) {
              lambda_1 = fit$lambda_1[i]
              lambda_2 = fit$lambda_2[i]
              xbeta = gesso.predict(fit$beta_0[i], fit$beta_e[i], fit$beta_g[,i],
                                    fit$beta_gxe[,i], test_G, test_E, family="gaussian")
              expect_lt(max(abs(xbeta - cv$full_cv_result$xbeta[cv$full_cv_result$fold_ids == fold_id, i])), 1e-10)
              if (family == "gaussian") {
                test_loss[i] = sum((test_Y - xbeta)^2) / test_sample_size
              } else {
                test_loss[i] = sum(log(1 + exp(xbeta)) - test_Y * xbeta) / test_sample_size
              }
            }
            
            expect_lt(max(abs(cv$full_cv_result$test_loss[fold_id,] - test_loss)), 1e-10)
          }
        }
      }
    }
  }
})

test_that("cv and fit return same results on (custom) individual folds", {
  tol = 1e-3
  grid_size = 5
  max_iterations = 5000
  sample_size = 100
  n_g_non_zero = 10
  n_gxe_non_zero = 5
  p = 30
  nfolds = 3
  seed = 1
  #for (seed in 3:4) {
    for (family in c("gaussian", "binomial")) {
      data = data.gen(seed=seed, sample_size=sample_size,
                      family=family, normalize=TRUE)
      
      fold_ids = sample(seq(1, dim(data$G_train)[1]) %% nfolds, replace=FALSE)
      fold_ids = fold_ids + 1
      
      cv = gesso.cv(data$G_train, data$E_train, data$Y_train,
                    tolerance=tol, normalize=FALSE, grid_size=grid_size,
                    family=family, fold_ids=fold_ids, seed=seed,
                    verbose=FALSE)
      expect_equal(sum(cv$full_cv_result$has_converged != 1), 0)
      expect_equal(sum(fold_ids != cv$full_cv_result$fold_ids), 0)
      
      for (fold_id in 1:nfolds) {
        weights = rep(0, sample_size)
        weights[cv$full_cv_result$fold_ids != fold_id] = 1
        weights = weights / sum(weights)
        
        fit = gesso.fit(data$G_train, data$E_train, data$Y_train,
                        tolerance=tol,
                        grid=cv$grid,
                        family=family,
                        max_iterations=max_iterations,
                        weights=weights,
                        normalize=FALSE)
        expect_equal(sum(fit$has_converged != 1), 0)
        
        test_G = data$G_train[cv$full_cv_result$fold_ids == fold_id,]
        test_E = data$E_train[cv$full_cv_result$fold_ids == fold_id]
        test_Y = data$Y_train[cv$full_cv_result$fold_ids == fold_id]
        test_sample_size = sum(cv$full_cv_result$fold_ids == fold_id)
        test_loss = rep(0, length(fit$lambda_1))
        
        for (i in 1:length(fit$lambda_1)) {
          lambda_1 = fit$lambda_1[i]
          lambda_2 = fit$lambda_2[i]
          xbeta = gesso.predict(fit$beta_0[i], fit$beta_e[i], fit$beta_g[,i],
                                fit$beta_gxe[,i], test_G, test_E, family="gaussian")
          if (family == "gaussian") {
            test_loss[i] = sum((test_Y - xbeta)^2) / test_sample_size
          } else {
            test_loss[i] = sum(log(1 + exp(xbeta)) - test_Y * xbeta) / test_sample_size
          }
        }
        
        expect_lt(max(abs(cv$full_cv_result$test_loss[fold_id,] - test_loss)), 1e-10)
      }
    }
  #}
})

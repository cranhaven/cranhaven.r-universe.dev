context("compare with glmnet")

test_that("training loss is similar to the fit by glmnet when E=0", {
  grid_size = 10
  grid = 10^seq(-4, log10(1), length.out=grid_size) 
  grid = rev(grid)
  tols = c(1e-4)
  
  max_iterations = 2000
  
  for (family in c("gaussian", "binomial")){
    #for (seed in 1:20) {
    for (seed in 1:5) {
      for (tol in tols) {
        file_name = paste0("testdata/compare_with_glmnet/", seed, "_", family, "_data.rds")
        data = readRDS(file_name)
        
        file_name = paste0("testdata/compare_with_glmnet/", seed, "_", family, "_glmnet_results.rds")
        glmnet_fit = readRDS(file_name)
        
        sample_size = length(data$Y_train)
        
        fit = gesso.fit(G=data$G_train, E=rep(0, sample_size),
                             Y=data$Y_train, C=data$C_train,
                             tolerance=tol, grid=grid, family=family, 
                             normalize=FALSE, max_iterations=max_iterations)
        expect_equal(sum(fit$has_converged != 1), 0)
        expect_lt(max(fit$objective_value - rep(glmnet_fit$objective_value, rep(grid_size, grid_size))), tol)
      }
    }
  }
})

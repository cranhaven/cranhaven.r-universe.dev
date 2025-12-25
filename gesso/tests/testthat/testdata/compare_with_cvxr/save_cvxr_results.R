
grid = 10^seq(-4, log10(1), length.out=10) 
tol = 1e-6
max_iterations = 10000

for (family in c("gaussian", "binomial")){
  for (seed in 2:2) {
    cat("---", seed, family, "\n")
    data = data.gen(sample_size=200, p=50, 
                    n_g_non_zero=10, n_gxe_non_zero=4, 
                    seed=seed,
                    family=family,
                    normalize=TRUE)

    file_name = paste0("tests/testthat/testdata/", seed, "_", family, "_data.rds")
    saveRDS(data, file_name)
    
    start = Sys.time()
    fit = gesso.fit(data$G_train, data$E_train, data$Y_train,
                         tolerance=tol, grid=grid, family=family, 
                         normalize=FALSE,
                         max_iterations=max_iterations)
    print(Sys.time() - start)
    
    cat("-- gesso.fit done in ", Sys.time() - start, " seconds. num not converged ", sum(1 - fit$has_converged), "\n")    

    cvxr_fit = hierNetGxE.cvxr(data$G_train, data$E_train, data$GxE_train, data$Y_train,
                               grid=grid, tol=tol, family=family)
    
    print(max(fit$objective_value - cvxr_fit$objective_value) < tol)
    
    file_name = paste0("tests/testthat/testdata/", seed, "_", family, "_cvxr_results.rds")
    saveRDS(cvxr_fit$objective_value, file_name)
  }
}

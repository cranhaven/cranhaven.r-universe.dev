test_that("CytOpt", {
  
  skip_if_notall_pythondeps()
  
  #load data
  data("HIPC_Stanford")
  
  #run CytOpt
  res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
         Lab_source = HIPC_Stanford_1228_1A_labels, method='both') # Comparison two methods
  expect_length(res, 2)
  res2 <- cytopt_desasc_r(HIPC_Stanford_1228_1A, HIPC_Stanford_1369_1A, 
                          as.numeric(HIPC_Stanford_1228_1A_labels)) # Use Desasc method
  expect_length(res2, 2)
  res3 <- cytopt_minmax_r(HIPC_Stanford_1228_1A, HIPC_Stanford_1369_1A, 
                          as.numeric(HIPC_Stanford_1228_1A_labels)) # Use Minmax method
  expect_length(res3, 2)
  
})
test_that("Correlation", {
  
  # Very small fit to run super fast
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)
  
  test_that("corrMatrix",{
    matrix_MGS <- corrMatrix(fit_MGS)
    matrix_MGSG <- corrMatrix(fit_MGSG)
    
    # Check class
    expect_true(all(class(matrix_MGS) == c("gg", "ggplot")))
    expect_true(all(class(matrix_MGSG) == c("gg", "ggplot")))
  })
  
  test_that("corrPlot",{
    plt_MGS <- corrPlot(fit_MGS)
    plt_MGS_det <- corrPlot(fit_MGS,"deterministic")
    plt_MGSG <- corrPlot(fit_MGSG)
    plt_MGSG_det <- corrPlot(fit_MGSG,"deterministic")
    plt_MGSG_sto <- corrPlot(fit_MGSG,"stochastic")
    
    # Check class
    expect_true(all(class(plt_MGS) == c("gg", "ggmatrix")))
    expect_true(all(class(plt_MGS_det) == c("gg", "ggmatrix")))
    expect_true(all(class(plt_MGSG) == c("gg", "ggmatrix")))
    expect_true(all(class(plt_MGSG_det) == c("gg", "ggmatrix")))
    expect_true(all(class(plt_MGSG_sto) == c("gg", "ggmatrix")))
  })
})

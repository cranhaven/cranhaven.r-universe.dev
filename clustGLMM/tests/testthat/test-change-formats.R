test_that("clustglmm object can be flawlessly changed to the different howsave format", {
  
  expect_equal({
    set.seed(123456789)
    suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE,
      howsave = "list")
    )}, {
    set.seed(123456789)
    mcmc <- suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE,
      howsave = "data.frame"))
    from_matrix_to_list(mcmc)  
  })
  
  expect_equal({
    set.seed(123456789)
    suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE,
      howsave = "data.frame")
    )}, {
      set.seed(123456789)
      mcmc <- suppressMessages(clustGLMM(
        formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
        id = "id", family = c(y = "num"), 
        data = data.frame(id = 1:10,
                          y = 1:10,
                          x = rep(c(0, 1), each = 5)),
        G = 1, nchains = 1, iter = 10, standardize = TRUE,
        howsave = "list"))
      from_list_to_matrix(mcmc)  
    })
})

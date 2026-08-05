test_that("A zero offset is automatically added to formula if missing", {

  expect_equal({
    set.seed(123456789)
    form <- list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1))
    mcmc <- suppressMessages(clustGLMM(
      formula = form, id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5),
                        o = rep(c(0, -10), each = 5)),
      G = 2, nchains = 1, iter = 10, standardize = TRUE))
    mcmc$formula
  }, 
  {
    list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1, offset = ""))
  })
})

test_that("If standardize=FALSE and intercept in fixed and group, the intercept is group-specific", {
  expect_equal({
    set.seed(123456789)
    form <- list(y = list(fixed = ~ 1, group = ~ x, random = ~ 1))
    mcmc <- suppressMessages(clustGLMM(
      formula = form, id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5),
                        o = rep(c(0, -10), each = 5)),
      G = 2, nchains = 1, iter = 10, standardize = FALSE))
    list(mcmc$lfixnames,
         mcmc$lgrpnames)
  }, 
  {
    list(list(y = character()),
         list(y = c("(Intercept)", "x")))
  })
  
})

test_that("If standardize=FALSE and no intercept in group, the intercept is not group-specific", {
  
  expect_equal({
    set.seed(123456789)
    form <- list(y = list(fixed = ~ 1, group = ~ x+0, random = ~ 1))
    mcmc <- suppressMessages(clustGLMM(
      formula = form, id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5),
                        o = rep(c(0, -10), each = 5)),
      G = 2, nchains = 1, iter = 10, standardize = FALSE))
    list(mcmc$lfixnames,
         mcmc$lgrpnames)
  }, 
  {
    list(list(y = "(Intercept)"),
         list(y = "x"))
  })
  
})

test_that("If standardize=TRUE, the intercept is group-specific", {
  
  expect_warning(
    expect_equal({
      set.seed(123456789)
      form <- list(y = list(fixed = ~ 1, group = ~ x-1, random = ~ 1))
      mcmc <- suppressMessages(clustGLMM(
        formula = form, id = "id", family = c(y = "num"), 
        data = data.frame(id = 1:10,
                          y = 1:10,
                          x = rep(c(0, 1), each = 5),
                          o = rep(c(0, -10), each = 5)),
        G = 2, nchains = 1, iter = 10, standardize = TRUE))
      list(mcmc$lfixnames,
           mcmc$lgrpnames)
    }, 
    {
      list(list(y = character()),
           list(y = c("(Intercept)", "x")))
    })
  )
  
})

test_that("If standardize=TRUE and no group terms, the intercept is not group-specific", {

    expect_equal({
      set.seed(123456789)
      form <- list(y = list(fixed = ~ x, group = ~ 0, random = ~ 1))
      mcmc <- suppressMessages(clustGLMM(
        formula = form, id = "id", family = c(y = "num"), 
        data = data.frame(id = 1:10,
                          y = 1:10,
                          x = rep(c(0, 1), each = 5),
                          o = rep(c(0, -10), each = 5)),
        G = 2, nchains = 1, iter = 10, standardize = TRUE))
      list(mcmc$lfixnames,
           mcmc$lgrpnames)
    }, 
    {
      list(list(y = c("(Intercept)", "x")),
           list())
    })
  
})

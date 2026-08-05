test_that("clustGLMM returns clustglmm class", {
  expect_s3_class({
    set.seed(123456789)
    suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE)
    )}, "clustglmm")
})

test_that("clustGLMM returns the same output with the same seed", {
  expect_equal({
    set.seed(123456789)
    suppressMessages(clustGLMM(
    formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
    id = "id", family = c(y = "num"), 
    data = data.frame(id = 1:10,
                      y = 1:10,
                      x = rep(c(0, 1), each = 5)),
    G = 1, nchains = 1, iter = 10, standardize = TRUE))
    }, {
    set.seed(123456789)
    suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 1)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE))
    })
})

test_that("clustGLMM when no parameter is group-specific", {
  expect_no_error({
    set.seed(123456789)
    mcmc <- suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ x, group = ~ 0, random = ~ 1)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      varying = default_varying(prec_num = FALSE),
      G = 2, nchains = 1, iter = 10, standardize = TRUE))
  })
})

test_that("clustGLMM when no random effects", {
  expect_length({
    set.seed(123456789)
    mcmc <- suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 1, group = ~ x, random = ~ 0),
                     z = list(fixed = ~ 1, group = ~ x, random = ~ 0)), 
      id = "id", family = c(y = "num", z = "poi"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        z = c(1, 2, 3, 3, 2, 1, 1, 2, 3, 2),
                        x = rep(c(0, 1), each = 5)),
      G = 2, nchains = 1, iter = 10, standardize = TRUE))
    intersect(names(mcmc$param_names[[1]]),
              c("Sigma", "InvSigma", "sdSigma", "corSigma", "detInvSigma"))
  }, 0)
})

test_that("clustGLMM when no random effects despite longitudinal structure", {
  expect_warning(
    expect_length({
      set.seed(123456789)
      mcmc <- suppressMessages(clustGLMM(
        formula = list(y = list(fixed = ~ 1, group = ~ x, random = ~ 0),
                       z = list(fixed = ~ 1, group = ~ x, random = ~ 0)), 
        id = "id", family = c(y = "num", z = "poi"), 
        data = data.frame(id = rep(1:5, each = 2),
                          y = 1:10,
                          z = c(1, 2, 3, 3, 2, 1, 1, 2, 3, 2),
                          x = rep(c(0, 1), each = 5)),
        G = 1, nchains = 1, iter = 10, standardize = TRUE))
      intersect(names(mcmc$param_names[[1]]),
                c("Sigma", "InvSigma", "sdSigma", "corSigma", "detInvSigma"))
    }, 0)
  )
})

test_that("clustGLMM with single outcome when no random effects despite longitudinal structure", {
  expect_no_warning(
    expect_length({
      set.seed(123456789)
      mcmc <- suppressMessages(clustGLMM(
        formula = list(y = list(fixed = ~ 1, group = ~ x, random = ~ 0)), 
        id = "id", family = c(y = "num"), 
        data = data.frame(id = rep(1:5, each = 2),
                          y = 1:10,
                          x = rep(c(0, 1), each = 5)),
        G = 1, nchains = 1, iter = 10, standardize = TRUE))
      intersect(names(mcmc$param_names[[1]]),
                c("Sigma", "InvSigma", "sdSigma", "corSigma", "detInvSigma"))
    }, 0)
  )
})

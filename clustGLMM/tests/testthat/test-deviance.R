test_that("deviance is computed correctly", {
  
  expect_equal({
    set.seed(123456789)
    mcmc <- suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 0)), 
      id = "id", family = c(y = "num"), 
      data = data.frame(id = 1:10,
                        y = 1:10,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE))
    pmcmc <- suppressMessages(clustering_probabilities_and_deviance(
      mcmc, id = "id", 
      data = data.frame(id = 11:12, y = c(4.5, 5.5), x = c(0, 1))
    ))
    # mcmc$draws[[1]]
    c(pmcmc$draws[[1]]$`dev_i[1]`, pmcmc$draws[[1]]$`dev_i[2]`)
  }, {
    c(-2*dnorm(4.5,
             mean = mcmc$draws[[1]]$`beta_num_y(1)[1]`,
             sd = mcmc$draws[[1]]$`sd_num_y(1)`, log = TRUE),
      -2*dnorm(5.5,
               mean = mcmc$draws[[1]]$`beta_num_y(1)[1]` + mcmc$draws[[1]]$`beta_num_y(1)[2]`,
               sd = mcmc$draws[[1]]$`sd_num_y(1)`, log = TRUE)
    )
  })
  
  expect_equal({
    set.seed(123456789)
    mcmc <- suppressMessages(clustGLMM(
      formula = list(y = list(fixed = ~ 0, group = ~ x, random = ~ 0)), 
      id = "id", family = c(y = "poi"), 
      data = data.frame(id = 1:10,
                        y = 0:9,
                        x = rep(c(0, 1), each = 5)),
      G = 1, nchains = 1, iter = 10, standardize = TRUE))
    pmcmc <- suppressMessages(clustering_probabilities_and_deviance(
      mcmc, id = "id", 
      data = data.frame(id = 11:12, y = c(4, 5), x = c(0, 1))
    ))
    # mcmc$draws[[1]]
    c(pmcmc$draws[[1]]$`dev_i[1]`, pmcmc$draws[[1]]$`dev_i[2]`)
  }, {
    c(-2*dpois(4,
               lambda = exp(mcmc$draws[[1]]$`beta_poi_y(1)[1]`),
               log = TRUE),
      -2*dpois(5,
               lambda = exp(mcmc$draws[[1]]$`beta_poi_y(1)[1]` + mcmc$draws[[1]]$`beta_poi_y(1)[2]`),
               log = TRUE)
    )
  })
})

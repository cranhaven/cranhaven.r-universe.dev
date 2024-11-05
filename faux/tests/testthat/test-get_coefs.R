test_that("errors", {
  data <- sim_design(within = 2, between = 2, 
                     mu = c(1, 0, 1, 1), 
                     long = TRUE, empirical = TRUE, plot = FALSE) %>%
    add_contrast("W1", "anova") %>%
    add_contrast("B1", "anova")
  
  coef <- get_coefs(data)
  expect_equal(coef, c("(Intercept)" = 0.75, W1 = -0.5, B1 = 0.5,  "W1:B1" = 1.0))
  
  coef <- get_coefs(data, y ~ W1 * B1)
  expect_equal(coef, c("(Intercept)" = 0.75, W1 = -0.5, B1 = 0.5, "W1:B1" = 1.0))
  
  coef <- get_coefs(data, y ~ B1 * W1)
  expect_equal(coef, c("(Intercept)" = 0.75, B1 = 0.5, W1 = -0.5, "B1:W1" = 1.0))
  
  coef <- get_coefs(data, y ~ W1 + B1)
  expect_equal(coef, c("(Intercept)" = 0.75, W1 = -0.5, B1 = 0.5))
  
  coef <- get_coefs(data, y ~ B1)
  expect_equal(coef, c("(Intercept)" = 0.75, B1 = 0.5))
  
  data$y <- norm2binom(data$y)
  mod <- glm(y~ W1*B1, data, family = binomial)
  coef <- get_coefs(data, fun = glm, family = binomial)
  expect_equivalent(coef, mod$coefficients)
  
})

context("Model plots")

dat <- data.frame(x = runif(100, 0, 1), stringsAsFactors = TRUE)
dat$y <- rnorm(100, 20 + dat$x * 5, 5)


test_that("Plot works for basic lm", {
    lm0 <- lm(y ~ 1, data = dat)
    expect_silent(plotlm6(lm0, which = 1))
    expect_silent(plotlm6(lm0, which = 3))
    expect_silent(plotlm6(lm0, which = 4))
    expect_silent(plotlm6(lm0, which = 4))
    expect_silent(plotlm6(lm0, which = 5))
    expect_silent(plotlm6(lm0, which = 6))

    lm1 <- lm(y ~ x, data = dat)
    expect_silent(plotlm6(lm1, which = 2))
    expect_silent(plotlm6(lm1, which = 3))
    expect_silent(plotlm6(lm1, which = 3))
    expect_silent(plotlm6(lm1, which = 4))
    expect_silent(plotlm6(lm1, which = 5))
    expect_silent(plotlm6(lm1, which = 6))
})

set.seed(100)
dat <- data.frame(x = runif(100, 0, 1), stringsAsFactors = TRUE)
dat$y <- rbinom(100, 1, log(dat$x*2 + 1) / log(3))
test_that("Plot works for basic glm", {
    glm0 <- glm(y ~ 1, data = dat)
    expect_silent(plotlm6(glm0, which = 1))
    expect_silent(plotlm6(glm0, which = 2))
    expect_silent(plotlm6(glm0, which = 3))
    expect_silent(plotlm6(glm0, which = 4))
    expect_silent(plotlm6(glm0, which = 5))
    expect_silent(plotlm6(glm0, which = 6))

    glm1 <- glm(y ~ x, data = dat)
    expect_silent(plotlm6(glm1, which = 1))
    expect_silent(plotlm6(glm1, which = 2))
    expect_silent(plotlm6(glm1, which = 3))
    expect_silent(plotlm6(glm1, which = 4))
    expect_silent(plotlm6(glm1, which = 5))
    expect_silent(plotlm6(glm1, which = 6))
})


# data(leuk, package = "MASS")
# glm1 <- glm(time ~ ag-1+log10(wbc), family = Gamma(log), data = leuk)

# glm1bs <- lapply(1:7, function(i) {
#     yhat <- rgamma(nrow(glm1$data), 1, predict(glm1))
#     .data <- glm1$data
# })

# fits <- c(list(glm1), glm1bs)

# par(mfrow = c(2, 4))
# lapply(sample(fits), plot, which = 1)

# plotlm6(glm1, which = 1, showBootstraps = FALSE)
# plotlm6(glm1bs, which = 1, showBootstraps = FALSE)


unlink("Rplots.pdf")

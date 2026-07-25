context("Factor output")

fit <- lm(Sepal.Length ~ Species, data = iris)
fit2 <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

test_that("Factor comparisons computed correctly", {
    set.seed(10)
    x <- factorComp(fit, "Species")
    expect_is(x, "inzfactorcomp")

    set.seed(10)
    z <- summary(multcomp::glht(fit, linfct = multcomp::mcp(Species = "Tukey")))
    expect_equal(x$estimate[c(1, 3:4)], as.numeric(z$test$coefficients))
    expect_equal(x$p.value[c(1, 3:4)], as.numeric(z$test$pvalues))
    expect_equal(
        round(x$ci[c(1, 5, 7)], 3),
        round(as.numeric(confint(z)$confint[, "lwr"]), 3)
    )
    expect_equal(
        round(x$ci[1 + c(1, 5, 7)], 3),
        round(as.numeric(confint(z)$confint[, "upr"]), 3)
    )

    expect_is(factorComp(fit2, "Species"), "inzfactorcomp")

    d <- data.frame(
        x = rnorm(1000),
        g1 = sample(c("A", "B"), 1000, replace = TRUE),
        g2 = sample(c("first", "second"), 1000, replace = TRUE),
        stringsAsFactors = TRUE
    )
    expect_is(factorComp(lm(x ~ g1 + g2, data = d), "g1"), "inzfactorcomp")
    expect_is(factorComp(lm(x ~ g1 + g2, data = d), "g2"), "inzfactorcomp")
})

test_that("Factor comparison matrix prints OK", {
    expect_output(print(factorComp(fit, "Species")))
})

test_that("Factor comparison for glm", {
    dat <- data.frame(x = runif(1000, 0, 1), stringsAsFactors = TRUE)
    dat$y <- rbinom(1000, 1, log(dat$x*2 + 1) / log(3))
    dat$g <- factor(sample(c("A", "B", "C"), 1000, replace = TRUE))
    glm1 <- glm(y ~ x + g, data = dat)
    expect_is(factorComp(glm1, "g"), "inzfactorcomp")
})

require(survey)
test_that("Factor comparison for svyglm", {
    data(api)
    dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
    s <- svyglm(api00~ell+meals+mobility+awards, design=dstrat)
    expect_is(factorComp(s, "awards"), "inzfactorcomp")

    expect_match(
        capture.output(factorComp(s, "awards")),
        "all coefficients associated with 'awards' are zero",
        all = FALSE
    )
})

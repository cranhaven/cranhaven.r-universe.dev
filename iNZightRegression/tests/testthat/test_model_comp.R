context("Model comparison")

f1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
f2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

test_that("Single model output is correct", {
    expect_is(compare_models(f1), "inzmodelcomp")
    expect_equal(compare_models(f1)[, "df"], 3)
    expect_equal(compare_models(f1)[, "AIC"], AIC(f1))
    expect_equal(compare_models(f1)[, "BIC"], BIC(f1))
})

test_that("Multiple model comparisons are correct", {
    expect_is(compare_models(f1, f2), "inzmodelcomp")
    expect_equal(compare_models(f1, f2)[, "df"], c(f1 = 3, f2 = 5))
    expect_equal(compare_models(f1, f2)[, "AIC"], c(f1 = AIC(f1), f2 = AIC(f2)))
    expect_equal(compare_models(f1, f2)[, "BIC"], c(f1 = BIC(f1), f2 = BIC(f2)))
})

iris$big <- as.integer(iris$Sepal.Length > median(iris$Sepal.Length))
g1 <- glm(big ~ Sepal.Width, data = iris, family = binomial)
g2 <- glm(big ~ Sepal.Width + Species, data = iris, family = binomial)

test_that("GLM models work", {
    expect_equal(
        compare_models(g1, g2),
        structure(
            cbind(
                c(2, 4),
                c(AIC(g1), AIC(g2)),
                c(BIC(g1), BIC(g2))
            ),
            .Dimnames = list(Model = c("g1", "g2"), c("df", "AIC", "BIC")),
            class = "inzmodelcomp"
        )
    )
})

library(survey)
data(api)
dclus2 <- svydesign(id = ~dnum + snum, weights = ~pw, data = apiclus2)

s1 <- svyglm(api00 ~ ell, design = dclus2)
s2 <- svyglm(api00 ~ ell + meals, design = dclus2)
s3 <- svyglm(api00 ~ ell + meals + mobility, design = dclus2)
s4 <- svyglm(api00 ~ meals, design = dclus2)

test_that("Survey models work", {
    skip("Test doesn't work within 'testthat' due to namespacing issues.")
    expect_is(compare_models(s1), "inzmodelcomp")
    expect_equivalent(compare_models(s1)[, "AIC"], AIC(s1)["AIC"])
    expect_equivalent(
        compare_models(s1)[, "BIC"],
        BIC(s1, maximal = s1)["BIC"]
    )

    expect_is(compare_models(s1, s2, s3), "inzmodelcomp")
    expect_is(compare_models(s3, s2, s1), "inzmodelcomp")
    expect_is(compare_models(s1, s4), "inzmodelcomp")

    expect_equal(
        compare_models(s1, s2, s3, s4),
        structure(
            cbind(
                AIC(s1, s2, s3, s4)[, "AIC"],
                BIC(s1, s2, s3, s4, maximal = s3)[, "BIC"],
                c(0, 0, 1, 0)
            ),
            .Dimnames = list(Model = paste0("s", 1:4), c("AIC", "BIC", "maximal")),
            class = "inzmodelcomp"
        )
    )
})

test_that("Mixing model types returns an error", {
    expect_error(compare_models(f1, g1))
    expect_error(compare_models(s1, f1))
})

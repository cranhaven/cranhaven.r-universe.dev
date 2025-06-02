regr <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
regr$call$formula
# UNSTANDARDIZED REGRESSION USING AN LM OBJECT
set.seed(100)
z_gor<-gorica(regr,"pre=fu=pea;pea > fu > pre; pre>fu>pea", standardize = FALSE, iterations = 1000)

test_that("gorica and bain give similar results for regression", {
  expect_equivalent(z_gor$fit$gorica_weights, c(4.2092165627915e-07, 1.66151509444716e-10, 0.790004046827182, 0.20999553208501), tolerance = .07)
})

# STANDARDIZED REGRESSION USING AN LM OBJECT


regr <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
set.seed(100)
#sz<-bain(regr,"pre=fu=pea;pea > fu > pre; pre>fu>pea", standardize = TRUE)
test_that("Warning when lm has argument standardize", {
  expect_warning(gorica(regr,"pre=fu=pea;pea > fu > pre; pre>fu>pea", standardize = TRUE, iterations = 10))})


# REGRESSION WITH THE INTERCEPT INCLUDED IN THE RESTRICTIONS



regr <- lm(postnumb ~ prenumb + peabody, sesamesim)
set.seed(100)

sz_gor<-gorica(regr,"Int>5 & pre > pea", standardize = FALSE, iterations = 1000)

test_that("gorica and bain give similar results for regression", {
  expect_equivalent(sz_gor$fit$gorica_weights, c(0.775134354794349, 0.224865645205651), tolerance = .07)
})

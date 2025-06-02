library(lavaan)

# read in the simulated sesamestreet data
sesamedata <- sesamesim

# use lavaan syntax to specify the confirmatory factor model
model1 <- 'Ab ~ Bb + Bl + 1'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- lavaan::sem(model1, data = sesamedata)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION WITH UNSTANDARDIZED PARAMETERS

hypotheses1 <-" Ab~Bb = Ab~Bl; Ab~Bb > Ab~Bl"
set.seed(100)
y1 <- gorica(fit1,hypotheses1,standardize = FALSE)

tab <- parTable(fit1)
tab$label <- paste0(tab$lhs, tab$op, tab$rhs)

test_that("gorica uses unstandardized estimates when standardize=FALSE", {
  expect_equivalent(y1$estimates, tab$est[match(names(y1$estimates), tab$label)])
})

y2 <- gorica(fit1,hypotheses1,standardize = TRUE)

tab <- standardizedSolution(fit1)
tab$label <- paste0(tab$lhs, tab$op, tab$rhs)

test_that("gorica uses standardized estimates when standardize=TRUE", {
  expect_equivalent(y2$estimates, tab$est[match(names(y1$estimates), tab$label)])
})

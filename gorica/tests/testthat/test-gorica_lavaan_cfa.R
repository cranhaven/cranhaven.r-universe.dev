library(lavaan)
data(sesamesim)
sesameCFA <- sesamesim
names(sesameCFA)[6] <- "pea"
model1 <- '
A =~ Ab + Al + Af + An + Ar + Ac
B =~ Bb + Bl + Bf + Bn + Br + Bc
'

# use the lavaan sem function to execute the confirmatory factor analysis
fit1 <- sem(model1, data = sesameCFA, std.lv = TRUE)

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION:

hypotheses1 <- "A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac >.6 &
B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc >.6"

set.seed(100)

y <- bain(fit1,hypotheses1,standardize = TRUE)
y_gor <- gorica(fit1,hypotheses1, standardize = TRUE)

test_that("bain and gorica give similar results", {
  expect_equivalent(y$fit$PMPb[1:2], y_gor$fit$gorica_weights, tolerance = .1)
})

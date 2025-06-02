#Example 3: Structural Equation Modeling
library(lavaan)


#Estimation of Structural Parameters and Their Covariance Matrix


########################################################################
#Maximum Likelihood Estimation

#Specification of the data set
data("wechsler")

#Specification of the variables and the model
SEM.model <- '
Cry = ~ y1 + y2 + y3 + y4
Fld = ~ y2 + y3 + y5 + y6 + y7 + y8
Cry ~ edc + age
Fld ~ edc + age
'


#Model fitting
fit <- cfa(SEM.model, data = wechsler, std.lv = TRUE)


##############################################################

strest <- standardizedSolution(fit)[1:10,4]
strcovmtrx1 <- as.matrix(lavInspect(fit, "vcov.std.all")[1:10,1:10])
names(strest)<-colnames(strcovmtrx1)

#Obtaining order-restricted estimates using ormle

#Specification of the hypotheses containing factor loadings

# Hypothesis 1
constr <- matrix(c(0, 1, 0, 0, -1, 0, 0, 0, 0, 0,
0, 0, 1, 0, 0, -1, 0, 0, 0, 0,
1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 1, 0, 0, 0, 0, 0, 0), nrow = 4, ncol = 10, byrow = TRUE)
rhs <- rep(0, 4)
nec <- 0
H1 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx1, constr = constr, nec = nec, rhs = rhs)

H1_char <- "Cry=~y2 > Fld=~y2 & Cry=~y3 > Fld=~y3 & Cry=~y1>0 & Cry=~y4 > 0"
parsed_hyp1 <- bain:::parse_hypothesis(names(strest), H1_char)

test_that("Parsed H1 correct", {
  expect_equivalent(constr, parsed_hyp1$hyp_mat[[1]][, 1:(ncol(parsed_hyp1$hyp_mat[[1]])-1)])
  expect_equivalent(rhs, parsed_hyp1$hyp_mat[[1]][, ncol(parsed_hyp1$hyp_mat[[1]])])
  expect_equivalent(nec, parsed_hyp1$n_ec)
})

# Hypothesis 2
constr <- matrix(c(0, 0, 0, 0, 0, 0, 1, -1, 0, 0,
0, 0, 0, 0, 0, 0, 0, 1, -1, 0,
0, 0, 0, 0, 0, 0, 0, 0, 1, -1), nrow = 3, ncol = 10, byrow = TRUE)
rhs <- rep(0, 3)
nec <- 0
H2 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx1, constr = constr, nec = nec, rhs = rhs)

H2_char <- "Fld=~y5 > Fld=~y6 & Fld=~y6 > Fld=~y7 & Fld=~y7 > Fld=~y8"
parsed_hyp2 <- bain:::parse_hypothesis(names(strest), H2_char)

test_that("Parsed H2 correct", {
  expect_equivalent(constr, parsed_hyp2$hyp_mat[[1]][, 1:(ncol(parsed_hyp2$hyp_mat[[1]])-1)])
  expect_equivalent(rhs, parsed_hyp2$hyp_mat[[1]][, ncol(parsed_hyp2$hyp_mat[[1]])])
  expect_equivalent(nec, parsed_hyp2$n_ec)
})

# Hypothesis 3
constr <- matrix(c(0, 1, 0, 0, -1, 0, 0, 0, 0, 0,
0, 0, 1, 0, 0, -1, 0, 0, 0, 0,
1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 1, -1, 0, 0,
0, 0, 0, 0, 0, 0, 0, 1, -1, 0,
0, 0, 0, 0, 0, 0, 0, 0, 1, -1), nrow = 7, ncol = 10, byrow = TRUE)
rhs <- rep(0, 7)
nec <- 0
H3 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx1, constr = constr, nec = nec, rhs = rhs)

H3_char <- "Cry=~y2 > Fld=~y2 & Cry=~y3 > Fld=~y3 & Cry=~y1 > 0 & Cry=~y4 > 0 & Fld=~y5 > Fld=~y6 & Fld=~y6 > Fld=~y7 & Fld=~y7 > Fld=~y8"
parsed_hyp3 <- bain:::parse_hypothesis(names(strest), H3_char)

test_that("Parsed H3 correct", {
  expect_equivalent(constr, parsed_hyp3$hyp_mat[[1]][, 1:(ncol(parsed_hyp3$hyp_mat[[1]])-1)])
  expect_equivalent(rhs, parsed_hyp3$hyp_mat[[1]][, ncol(parsed_hyp3$hyp_mat[[1]])])
  expect_equivalent(nec, parsed_hyp3$n_ec)
})


# The unconstrained hypothesis
constr <- matrix(c(rep(0, 10)), nrow = 1, ncol = 10, byrow = TRUE)
rhs <- rep(0, 1)
nec <- 0
Hu <- gorica:::ormle(est = strest, covmtrx = strcovmtrx1, constr = constr, nec = nec, rhs = rhs)

# Evaluation of hypotheses containing factor loadings
set.seed(111)


#Performing gorica to obtain the values of misfit, complexity, GORICA, and GORICA weights
man_gorica <- gorica:::compare_hypotheses.ormle(H1, H2, H3, Hu, iter = 1000)
set.seed(111)

res_gorica <- gorica(fit, "Cry=~y2 > Fld=~y2 & Cry=~y3 > Fld=~y3 & Cry=~y1>0 & Cry=~y4 > 0; Fld=~y5 > Fld=~y6 & Fld=~y6 > Fld=~y7 & Fld=~y7 > Fld=~y8; Cry=~y2 > Fld=~y2 & Cry=~y3 > Fld=~y3 & Cry=~y1 > 0 & Cry=~y4 > 0 & Fld=~y5 > Fld=~y6 & Fld=~y6 > Fld=~y7 & Fld=~y7 > Fld=~y8", iter = 1000, standardize = TRUE)

test_that("Manual and package version yield same loglik", {
  expect_equivalent(man_gorica$comparisons$loglik, res_gorica$fit$loglik)
})

test_that("Manual and package version yield same penalty", {
  expect_equivalent(man_gorica$comparisons$penalty, res_gorica$fit$penalty, tolerance = .05)
})

test_that("Manual and package version yield same gorica", {
  expect_equivalent(man_gorica$comparisons$gorica, res_gorica$fit$gorica, tolerance = .2)
})

test_that("Manual and package version yield same weights", {
  expect_equivalent(gorica:::compute_weights(man_gorica$comparisons$gorica),
                    res_gorica$fit$gorica_weights, tolerance = .02)
})


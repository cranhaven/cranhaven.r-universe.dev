#Example 2: Multilevel Regression Modeling

########################################################################
#Maximum Likelihood Estimation
set.seed(111)
library(lme4)
#Specification of the data set
data("reading_ach")
reading_ach$zgeread <- scale(reading_ach$geread)
reading_ach$zage <- scale(reading_ach$age)
reading_ach$zgevocab <- scale(reading_ach$gevocab)


#Model fitting
model <- lmer(zgeread ~ zage + zgevocab + gender + zage * zgevocab + zage * gender +
zgevocab * gender + (1 + zgevocab | school), data = reading_ach, REML = FALSE)


strest <- summary(model)$coefficients[c(2,3,5,6,7),1]
strcovmtrx <- vcov(model)[c(2,3,5,6,7), c(2,3,5,6,7)]

#Specification of the hypotheses under evaluation

# Hypothesis 1
constr <- matrix(c(1, 0, 0, 0, 0,
1, 0, 0, 1, 0,
0, 1, 0, 0, 0,
0, 0, 1, 0, 0,
0, 1, 0, 0, 1), nrow = 5, ncol = 5, byrow = TRUE)
rhs <- rep(0, 5)
nec <- 2
H1 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

H1_char <- "zage = 0 & zage + zage:genderMale = 0 & zgevocab > 0 & zage:zgevocab > 0 & zgevocab+zgevocab:genderMale > 0"
parsed_hyp1 <- bain:::parse_hypothesis(names(strest), H1_char)

test_that("Parsed H1 correct", {
  expect_equivalent(constr, parsed_hyp1$hyp_mat[[1]][, 1:(ncol(parsed_hyp1$hyp_mat[[1]])-1)])
  expect_equivalent(rhs, parsed_hyp1$hyp_mat[[1]][, ncol(parsed_hyp1$hyp_mat[[1]])])
  expect_equivalent(nec, parsed_hyp1$n_ec)
})

# Hypothesis 2
constr <- matrix(c(-1, 0, 0, 0, 0,
0, 1, 0, 0, 0,
0, 0, 1, 0, 0,
-1, 0, 0, -1, 0,
0, 1, 0, 0, 1), nrow = 5, ncol = 5, byrow = TRUE)
rhs <- rep(0, 5)
nec <- 0
H2 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

H2_char <- "zage < 0 & zgevocab > 0 & zage:zgevocab > 0 & zage+zage:genderMale < 0&zgevocab+zgevocab:genderMale > 0"

parsed_hyp2 <- bain:::parse_hypothesis(names(strest), H2_char)

test_that("Parsed H2 correct", {
  expect_equivalent(constr, parsed_hyp2$hyp_mat[[1]][, 1:(ncol(parsed_hyp2$hyp_mat[[1]])-1)])
  expect_equivalent(rhs, parsed_hyp2$hyp_mat[[1]][, ncol(parsed_hyp2$hyp_mat[[1]])])
  expect_equivalent(nec, parsed_hyp2$n_ec)
})

# Hypothesis 3
constr <- matrix(c(0, 0, 1, 0, 0,
1, 0, 0, 0, 0,
0, 1, 0, 0, 0,
-1, 0, 0, -1, 0,
0, -1, 0, 0, -1), nrow = 5, ncol = 5, byrow = TRUE)
rhs <- rep(0,5)
nec <- 1
H3 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

H3_char <- "zage:zgevocab = 0 & zage > 0 & zgevocab > 0 & zage + zage:genderMale < 0 & zgevocab + zgevocab:genderMale < 0 "

parsed_hyp3 <- bain:::parse_hypothesis(names(strest), H3_char)

test_that("Parsed H3 correct", {
  expect_equivalent(constr, parsed_hyp3$hyp_mat[[1]][, 1:(ncol(parsed_hyp3$hyp_mat[[1]])-1)])
  expect_equivalent(rhs, parsed_hyp3$hyp_mat[[1]][, ncol(parsed_hyp3$hyp_mat[[1]])])
  expect_equivalent(nec, parsed_hyp3$n_ec)
})

# The unconstrained hypothesis
constr <- matrix(c(rep(0, 5)), nrow = 1, ncol = 5, byrow = TRUE)
rhs <- rep(0, 1)
nec <- 0
Hu <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)


set.seed(111)
#Performing gorica to obtain the values of misfit, complexity, GORICA, and GORICA weights
man_gorica <- gorica:::compare_hypotheses.ormle(H1, H2, H3, Hu, iter = 1000)

set.seed(111)
res_gorica <- gorica(model, "zage = 0 & zage + zage:genderMale = 0 & zgevocab > 0 & zage:zgevocab > 0 & zgevocab+zgevocab:genderMale > 0; zage < 0 & zgevocab > 0 & zage:zgevocab > 0 & zage+zage:genderMale < 0&zgevocab+zgevocab:genderMale > 0; zage:zgevocab = 0 & zage > 0 & zgevocab > 0 & zage + zage:genderMale < 0 & zgevocab + zgevocab:genderMale < 0 ", iter = 1000)

test_that("Manual and package version yield same loglik", {
  expect_equivalent(man_gorica$comparisons$loglik, res_gorica$fit$loglik)
})

test_that("Manual and package version yield same penalty", {
  expect_equivalent(man_gorica$comparisons$penalty, res_gorica$fit$penalty, tolerance = .05)
})

test_that("Manual and package version yield same gorica", {
  expect_equivalent(man_gorica$comparisons$gorica, res_gorica$fit$gorica, tolerance = .1)
})

test_that("Manual and package version yield same weights", {
  expect_equivalent(gorica:::compute_weights(man_gorica$comparisons$gorica),
                    gorica:::compute_weights(res_gorica$fit$gorica), tolerance = .01)
})


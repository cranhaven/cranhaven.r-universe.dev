#Example 1: Poisson Regression Modeling

########################################################################
#Maximum Likelihood Estimation


#Specification of the data set
data("academic_awards")
academic_awards$zmath <- scale(academic_awards$math)
#Model fitting
model <- glm(num_awards ~ prog + zmath + prog * zmath, family = "poisson",
data = academic_awards)


strest <- model$coefficients[c(4,5,6)]
strcovmtrx <- vcov(model)[c(4,5,6), c(4,5,6)]

#Obtaining order-restricted estimates using ormle

#Specification of the hypotheses under evaluation

# Hypothesis 1
constr <- matrix(c(1, 0, 0,
1, 1, 0,
1, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
rhs <- rep(0, 3)
nec <- 3
H1 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

H1_char <- "zmath = 0 & zmath + progGeneral:zmath = 0 & zmath + progVocational:zmath =0"
parsed_hyp1 <- bain:::parse_hypothesis(names(strest), H1_char)

test_that("Parsed H1 correct", {
  expect_equivalent(constr, parsed_hyp1$hyp_mat[[1]][1:3, 1:3])
  expect_equivalent(rhs, parsed_hyp1$hyp_mat[[1]][1:3, 4])
  expect_equivalent(nec, parsed_hyp1$n_ec)
})

# Hypothesis 2
constr <- matrix(c(0, 1, -1,
0, 0, 1), nrow = 2, ncol = 3, byrow = TRUE)
rhs <- rep(0, 2)
nec <- 0
H2 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)
H2_char <- "progGeneral:zmath > progVocational:zmath & progVocational:zmath > 0"

parsed_hyp2 <- bain:::parse_hypothesis(names(strest), H2_char)

test_that("Parsed H2 correct", {
  expect_equivalent(constr, parsed_hyp2$hyp_mat[[1]][1:2, 1:3])
  expect_equivalent(rhs, parsed_hyp2$hyp_mat[[1]][1:2, 4])
  expect_equivalent(nec, parsed_hyp2$n_ec)
})


# Hypothesis 3
constr <- matrix(c(0, 0, -1), nrow = 1, ncol = 3, byrow = TRUE)
rhs <- rep(0, 1)
nec <- 0
H3 <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)
H3_char <- "progVocational:zmath < 0"

parsed_hyp3 <- bain:::parse_hypothesis(names(strest), H3_char)

test_that("Parsed H3 correct", {
  expect_equivalent(constr, parsed_hyp3$hyp_mat[[1]][1, 1:3])
  expect_equivalent(rhs, parsed_hyp3$hyp_mat[[1]][1, 4])
  expect_equivalent(nec, parsed_hyp3$n_ec)
})


# The unconstrained hypothesis
constr <- matrix(c(rep(0, 3)), nrow = 1, ncol = 3, byrow = TRUE)
rhs <- rep(0, 1)
nec <- 0
Hu <- gorica:::ormle(est = strest, covmtrx = strcovmtrx, constr = constr, nec = nec, rhs = rhs)

set.seed(111)

#Performing gorica to obtain the values of misfit, complexity, GORICA, and GORICA weights
man_gorica <- gorica:::compare_hypotheses.ormle(H1, H2, H3, Hu, iter = 100000)

res_gorica <- gorica(model, "zmath = 0 & zmath + progGeneral:zmath = 0 & zmath + progVocational:zmath =0; progGeneral:zmath > progVocational:zmath & progVocational:zmath > 0; progVocational:zmath < 0", iter = 100000)

test_that("Manual and package version yield same loglik", {
  expect_equivalent(man_gorica$comparisons$loglik, res_gorica$fit$loglik, tolerance = .01)
})

test_that("Manual and package version yield same penalty", {
  expect_equivalent(man_gorica$comparisons$penalty, res_gorica$fit$penalty, tolerance = .01)
})

test_that("Manual and package version yield same gorica", {
  expect_equivalent(man_gorica$comparisons$gorica, res_gorica$fit$gorica, tolerance = .01)
})

test_that("Manual and package version yield same weights", {
  expect_equivalent(gorica:::compute_weights(man_gorica$comparisons$gorica),
                    gorica:::compute_weights(res_gorica$fit$gorica), tolerance = .01)
})

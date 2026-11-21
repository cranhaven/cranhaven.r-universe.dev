regtab_fixture <- function() {
  set.seed(606)
  n <- 180

  age <- round(stats::rnorm(n, mean = 52, sd = 11), 1)
  sex <- factor(sample(c("Female", "Male"), n, replace = TRUE))
  smoking <- factor(
    sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.45, 0.3, 0.25)),
    levels = c("Never", "Former", "Current")
  )

  male <- as.integer(sex == "Male")
  former <- as.integer(smoking == "Former")
  current <- as.integer(smoking == "Current")

  eta_bin1 <- -5.6 + 0.075 * age + 0.9 * male + 0.35 * former + 0.8 * current - 0.02 * age * male
  eta_bin2 <- -4.9 + 0.06 * age + 0.5 * male + 0.6 * former + 0.45 * current + 0.015 * age * male
  eta_pois1 <- 0.2 + 0.015 * age + 0.3 * male + 0.18 * former + 0.28 * current + 0.01 * age * male
  eta_pois2 <- -0.05 + 0.012 * age + 0.22 * male + 0.1 * former + 0.35 * current - 0.008 * age * male
  mu_gauss <- 9 + 0.32 * age - 1.4 * male + 0.6 * former + 1.3 * current + 0.05 * age * male

  dat <- data.frame(
    age = age,
    sex = sex,
    smoking = smoking,
    disease_a = stats::rbinom(n, 1, stats::plogis(eta_bin1)),
    disease_b = stats::rbinom(n, 1, stats::plogis(eta_bin2)),
    visits_a = stats::rpois(n, lambda = exp(eta_pois1)),
    visits_b = stats::rpois(n, lambda = exp(eta_pois2)),
    score_a = mu_gauss + stats::rnorm(n, sd = 1.8),
    score_b = mu_gauss + 1.5 + stats::rnorm(n, sd = 2.1),
    stringsAsFactors = FALSE
  )

  dat$age[c(2, 17, 44, 91)] <- NA_real_
  dat$smoking[c(8, 29, 73)] <- NA
  dat$visits_b[c(11, 32, 120)] <- NA_integer_
  dat$score_b[c(6, 41)] <- NA_real_
  dat$bad_outcome <- "not-a-valid-glm-response"

  dat
}

manual_regtab_reference <- function(data,
                                    outcome,
                                    predictors,
                                    family,
                                    robust = TRUE,
                                    conf.level = 0.95,
                                    exponentiate = NULL,
                                    include_intercept = FALSE,
                                    offset = NULL) {
  full_formula <- stats::update(predictors, stats::as.formula(paste(outcome, "~ .")))
  if (!is.null(offset)) {
    full_formula <- stats::update(
      full_formula,
      stats::as.formula(paste0(". ~ . + offset(log(", offset, "))"))
    )
  }
  fit <- stats::glm(full_formula, data = data, family = family)
  fam_name <- family$family
  if (is.null(exponentiate)) {
    exponentiate <- fam_name %in% c("poisson", "binomial", "quasipoisson", "quasibinomial")
  }

  coef_vec <- stats::coef(fit)
  vcov_mat <- if (isTRUE(robust)) {
    sandwich::vcovHC(fit, type = "HC0")
  } else {
    stats::vcov(fit)
  }
  se <- sqrt(diag(vcov_mat))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  terms <- names(coef_vec)
  if (!isTRUE(include_intercept)) {
    terms <- setdiff(terms, "(Intercept)")
  }

  estimate <- coef_vec[terms]
  lower <- estimate - z * se[terms]
  upper <- estimate + z * se[terms]
  if (isTRUE(exponentiate)) {
    estimate <- exp(estimate)
    lower <- exp(lower)
    upper <- exp(upper)
  }

  data.frame(
    outcome = outcome,
    term = terms,
    estimate = as.numeric(estimate),
    conf.low = as.numeric(lower),
    conf.high = as.numeric(upper),
    p.value = as.numeric(2 * stats::pnorm(-abs(stats::coef(fit)[terms] / se[terms]))),
    n = stats::nobs(fit),
    stringsAsFactors = FALSE
  )
}

glm_vif_fixture <- function() {
  set.seed(2302)
  n <- 90
  x1 <- seq(-1.5, 1.5, length.out = n)
  x2 <- sin(seq(0, 2 * pi, length.out = n))
  x3 <- rep(c(-0.5, 0.5), length.out = n)
  f <- factor(rep(c("A", "B", "C"), length.out = n))
  near <- x1 + stats::rnorm(n, sd = 0.03)

  y_cont <- 1 + 0.4 * x1 - 0.2 * x2 + 0.1 * x3 + stats::rnorm(n, sd = 0.7)
  y_factor <- 0.5 + 0.25 * x1 +
    ifelse(f == "B", 0.4, ifelse(f == "C", -0.3, 0)) +
    stats::rnorm(n, sd = 0.8)
  y_near <- 2 + 0.3 * x1 - 0.25 * near + stats::rnorm(n, sd = 0.5)

  data.frame(y_cont, y_factor, y_near, x1, x2, x3, f, near)
}

glm_vif_moderate_fixture <- function() {
  set.seed(2303)
  n <- 100
  x1 <- stats::rnorm(n)
  x2 <- x1 + stats::rnorm(n, sd = 0.45)
  y <- 1 + 0.2 * x1 - 0.1 * x2 + stats::rnorm(n)
  data.frame(y, x1, x2)
}

glm_vif_orthogonal_fixture <- function() {
  n <- 80
  x1 <- rep(c(-1, 1), each = n / 2)
  x2 <- rep(c(-1, 1), times = n / 2)
  y <- 1 + 0.2 * x1 - 0.1 * x2 + rep(c(-0.2, 0.2), length.out = n)
  data.frame(y, x1, x2)
}

regtab_many_predictors_fixture <- function(p = 11, design = FALSE) {
  n <- 140
  x <- as.data.frame(replicate(p, stats::rnorm(n), simplify = FALSE))
  names(x) <- paste0("x", seq_len(p))
  y <- 1 + 0.08 * rowSums(x) + stats::rnorm(n)
  dat <- cbind(data.frame(y = y), x)
  if (isTRUE(design)) {
    dat <- set_design(dat, "cross_sectional")
  }
  dat
}

regtab_many_predictors_formula <- function(p = 11) {
  stats::as.formula(paste("~", paste(paste0("x", seq_len(p)), collapse = " + ")))
}

# Two-group aggregated person-time fixture: one row per group with a total
# event count and total person-years, for closed-form incidence-rate-ratio
# reference checks against a saturated 2-parameter Poisson-log model.
person_time_fixture <- function() {
  data.frame(
    group = factor(c("ref", "exp"), levels = c("ref", "exp")),
    mace_count = c(25L, 40L),
    person_years = c(480, 500),
    stringsAsFactors = FALSE
  )
}

# Closed-form Wald-log IRR reference for a two-group rate comparison:
# IRR = (count_exp / pt_exp) / (count_ref / pt_ref); SE(log IRR) is exact
# (not approximate) for a saturated two-cell Poisson model under model-based
# (non-robust) standard errors.
manual_two_group_irr_reference <- function(count_ref, pt_ref, count_exp, pt_exp, conf.level = 0.95) {
  rate_ref <- count_ref / pt_ref
  rate_exp <- count_exp / pt_exp
  irr <- rate_exp / rate_ref
  se_log <- sqrt(1 / count_exp + 1 / count_ref)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  list(
    estimate = irr,
    lower = exp(log(irr) - z * se_log),
    upper = exp(log(irr) + z * se_log)
  )
}

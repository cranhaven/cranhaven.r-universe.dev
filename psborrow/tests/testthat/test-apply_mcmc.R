

suppressPackageStartupMessages({
    library(dplyr)
    library(flexsurv)
})


test_that("apply_mcmc can recover known parameters and catches errors", {
    set.seed(13465)

    n <- 4000

    grp_lvl <- c("CC", "TRT", "EC")
    sex_lvl <- c("Male", "Female")

    log_hr_grp <- c(
        "TRT" = log(1 / 400),
        "CC" = log(1 / 200),
        "EC" = log(1 / 200)
    )

    log_hr_sex <- c(
        "Male" = 0,
        "Female" = -0.1
    )

    log_hr_age <- 0.2

    Shape <- 0.95

    dat <- tibble(
        pt = 1:n,
        grp = factor(
            sample(grp_lvl, size = n, replace = TRUE, prob = c(0.3, 0.3, 0.4)),
            levels = grp_lvl
        ),
        sex = factor(
            sample(sex_lvl, size = n, replace = TRUE, prob = c(0.4, 0.6)),
            levels = sex_lvl
        ),
        age = rnorm(n, mean = 0, sd = 1),
        lambda = exp(
            log_hr_age * age +
            log_hr_sex[as.character(sex)] +
            log_hr_grp[as.character(grp)]
        ),
        time = rweibullPH(n, scale = lambda, shape = Shape),
        centime = rexp(n, 1 / 400)
    ) %>%
        mutate(event = ifelse(time <= centime, 1, 0)) %>%
        mutate(time = ifelse(time <= centime, time, centime)) %>%
        select(pt, grp, age, sex, time, event) %>%
        mutate(trt = ifelse(grp == "TRT", 1, 0)) %>%
        mutate(ext = ifelse(grp == "EC", 1, 0)) %>%
        mutate(cnsr = 1 - event) %>%
        select(pt, trt, ext, time, cnsr, sex, age)

    options("psborrow.quiet" = TRUE)

    expect_error(
      apply_mcmc(
        dt = dat,
        formula_cov = ~ 1,
        priorObj = set_prior(pred = "all", prior = "gamma", r0 = 0.85, alpha = c(0, 0)),
        n.chains = 1,
        n.adapt = 200,
        n.burn = 300,
        n.iter = 700,
        seed = 47
      ),
      "`formula_cov = ~ 1 ` is not supported"
    )

    x <- apply_mcmc(
        dt = dat,
        formula_cov = ~ 1 + age + sex,
        priorObj = set_prior(pred = "all", prior = "gamma", r0 = 0.85, alpha = c(0, 0)),
        n.chains = 2,
        n.adapt = 100,
        n.burn = 100,
        n.iter = 200,
        seed = 47
    )

    options("psborrow.quiet" = FALSE)

    ss <- summary(x)

    rownames(ss) <- ss$parameter

    expect_between <- function(x, par) {
        m <- ss[par, "mean"]
        sd <- ss[par, "sd"]
        ui <- m + qnorm(0.999) * sd
        li <- m - qnorm(0.999) * sd
        expect_lte(x, ui)
        expect_gte(x, li)
    }

    real <- exp(log_hr_grp["CC"] - log_hr_grp["EC"])
    expect_between(real, "HR_cc_hc")

    real <- exp(log_hr_grp["TRT"] - log_hr_grp["CC"])
    expect_between(real, "HR_trt_cc")

    real <- log_hr_grp["CC"]
    expect_between(real, "alpha[1]")

    real <- log_hr_grp["EC"]
    expect_between(real, "alpha[2]")

    real <- log_hr_grp["TRT"] - log_hr_grp["CC"]
    expect_between(real, "beta_trt")

    real <- log_hr_age
    expect_between(real, "beta_age")

    real <- log_hr_sex["Female"]
    expect_between(real, "beta_sexFemale")

    real <- Shape
    expect_between(real, "r0")
})

test_that("apply_mcmc works with one covariate", {
  set.seed(401)
  n <- 500

  grp_lvl <- c("CC", "TRT", "EC")
  sex_lvl <- c("Male", "Female")

  log_hr_grp <- c(
    "TRT" = log(1 / 400),
    "CC" = log(1 / 200),
    "EC" = log(1 / 150)
  )
  log_hr_sex <- c(
    "Male" = 0,
    "Female" = -0.1
  )
  log_hr_age <- 0.2


  dat <- tibble(
    pt = 1:n,
    grp = factor(
      sample(grp_lvl, size = n, replace = TRUE, prob = c(0.3, 0.3, 0.4)),
      levels = grp_lvl
    ),
    sex = factor(
      sample(sex_lvl, size = n, replace = TRUE, prob = c(0.4, 0.6)),
      levels = sex_lvl
    ),
    age = rnorm(n, mean = 0, sd = 1),
    lambda = exp(
      log_hr_age * age +
        log_hr_sex[as.character(sex)] +
        log_hr_grp[as.character(grp)]
    ),
    time = rexp(n, rate = lambda),
    centime = rexp(n, 1 / 400)
  ) %>%
    mutate(event = ifelse(time <= centime, 1, 0)) %>%
    mutate(time = ifelse(time <= centime, time, centime)) %>%
    select(pt, grp, age, sex, time, event)


  dat2 <- dat %>%
    mutate(trt = ifelse(grp == "TRT", 1, 0)) %>%
    mutate(ext = ifelse(grp == "EC", 1, 0)) %>%
    mutate(cnsr = 1 - event) %>%
    select(pt, trt, ext, time, cnsr, sex, age)

  result <- apply_mcmc(
    dt = dat2,
    formula_cov = ~ 1 + age,
    priorObj = set_prior(pred = "all", prior = "gamma", r0 = 0.85, alpha = c(0, 0)),
    n.chains = 1,
    n.adapt = 100,
    n.burn = 100,
    n.iter = 200,
    seed = 47
  )

  expect_s3_class(result, "apply_mcmc")
  expect_s3_class(summary(result), "data.frame")
})



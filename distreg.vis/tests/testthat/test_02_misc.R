## Misc Testing ##

# Remove everything
rm(list = ls())

## Context
testthat::context("Miscellaneous")

# Libraries
library("gamlss")
library("distreg.vis")
library("bamlss")
library("testthat")

### --- Mult_trans --- ###

# Transform predictions of multinomial data
# multinomial_p1 <- multinomial_p %>%
#   sample_n(1)
#
# m_one <- distreg.vis:::mult_trans(multinomial_p1, multinomial_model)
# m_two <- distreg.vis:::mult_trans(multinomial_p, multinomial_model)
# expect_equal(class(m_one), "data.frame")
# expect_equal(class(m_two), "data.frame")

### -- Shiny & Javascript/CSS --- ###
cssfile <-
  system.file("srcjs/solarized-dark.css", package = "distreg.vis")
jsfile <-
  system.file("srcjs/highlight.pack.js", package = "distreg.vis")
expect_true(file.exists(cssfile))
expect_true(file.exists(jsfile))

### -- fac_check -- ###
DF <-
  structure(
    list(
      race = structure(
        c(1L, 1L, 1L, 1L, 1L),
        .Label = c("1. White",
                   "2. Black", "3. Asian", "4. Other"),
        class = c("ordered", "factor")
      ),
      year = c(2006L, 2006L, 2006L, 2006L, 2006L),
      education = structure(
        1:5,
        .Label = c(
          "1. < HS Grad",
          "2. HS Grad",
          "3. Some College",
          "4. College Grad",
          "5. Advanced Degree"
        ),
        class = c("ordered", "factor")
      ),
      health = structure(
        c(1L,
          1L, 1L, 1L, 1L),
        .Label = c("1. <=Good", "2. >=Very Good"),
        class = c("ordered",
                  "factor")
      ),
      age = c(42L, 42L, 42L, 42L, 42L),
      intercept = c(TRUE,
                    TRUE, TRUE, TRUE, TRUE)
    ),
    .Names = c("race", "year", "education",
               "health", "age", "intercept"),
    row.names = c("P1", "P2", "P3",
                  "P4", "P5"),
    class = "data.frame"
)
DF <- distreg.vis:::fac_check(DF)
expect_false("ordered" %in% unlist(sapply(DF, class)))

## --- Test for correct calculation of preds() function --- ##

fam_name <<- "GA"

# Data
art_data <- model_fam_data(fam_name = fam_name)
ndata <- art_data[sample(seq_len(nrow(art_data)), 5),
                  !colnames(art_data) %in% fam_name]

# gamlss model fitting
form <- as.formula(paste0(fam_name, "~ norm2 + binomial1"))
ga <- gamlss(form, sigma.formula = ~ .,
             data = art_data, family = fam_name, trace = FALSE)

# bamlss model fitting
ba <- bamlss(list(GA ~ s(norm2) + binomial1,
                  sigma ~ s(norm2) + binomial1),
             data = art_data, family = gamma_bamlss(),
             verbose = FALSE)

# Preds
ba_pred_samples_5 <- preds(ba, newdata = ndata, what = "samples")
ba_pred_samples_1 <- preds(ba, newdata = ndata[1, , drop = FALSE], what = "samples")
ba_pred_mean_5 <- preds(ba, newdata = ndata, what = "mean")
ba_pred_mean_1 <- preds(ba, newdata = ndata[1, , drop = FALSE], what = "mean")
ga_pred_mean_5 <- preds(ga, newdata = ndata, what = "mean")

# Moms
ba_mom_samples_5_mean <-
  moments(ba_pred_samples_5, fam_name = "gaussian", what = "mean")
ba_mom_samples_5_ll <-
  moments(ba_pred_samples_5, fam_name = "gaussian", what = "lowerlimit")
ba_mom_samples_5_ul <-
  moments(ba_pred_samples_5, fam_name = "gaussian", what = "upperlimit")
ba_mom_samples_1_ul <-
  moments(ba_pred_samples_1, fam_name = "gaussian", what = "upperlimit")
ba_mom_mean_5_ul <-
  moments(ba_pred_mean_5, fam_name = "gaussian", what = "mean")

## --- Test for error message when supplied with transformed variables --- ##
dat <- model_fam_data(fam_name = "GA")
dat$norma <- dat$norm2 - min(dat$norm2) + 0.01
dat$lognorma <- log(dat$norma)
m1 <-
  gamlss(
    GA ~ factor(binomial1) + log(norma),
    sigma.fo =  ~ factor(binomial1),
    nu.fo =  ~ log(norma),
    tau.fo =  ~ factor(binomial1) + log(norma),
    data = dat,
    family = GA
  )
m2 <-
  gamlss(
    GA ~ factor(binomial1) + lognorma,
    sigma.fo =  ~ factor(binomial1),
    nu.fo =  ~ lognorma,
    tau.fo =  ~ factor(binomial1) + lognorma,
    data = dat,
    family = GA
  )
expect_error(preds(m1, newdata = data.frame(binomial1 = "yes", norma = 50)))
expect_error(preds(m2, newdata = data.frame(binomial1 = "yes", lognorma = log(50))))


# mkuhn, 2021-11-15
# extrapolation to get P-values from reported critical values

library("rmetalog")
library("tibble")
library("tidyr")
library("purrr")
library("rlang")
library("ggplot2")


# exponential distribution ----

# P-value for A2_mod based on upper tail percentage points (table 4.14, p.138)
# use linear extrapolation on log-scaled sig_level to get P-values
A2_mod_crit_exp <- tibble::tribble(
  ~statist,
  ~p_val,
  .736,
  .25,
  .916,
  .15,
  1.062,
  .10,
  1.321,
  .05,
  1.591,
  .025,
  1.959,
  .01
) %>%
  dplyr::mutate(prob = 1 - p_val)

ggplot(A2_mod_crit_exp, mapping = aes(x = statist, y = p_val)) +
  scale_y_log10() +
  geom_point() +
  geom_line(size = .4, linetype = 'dotted') +
  geom_smooth(
    method = lm,
    formula = y ~ x + log(x),
    se = FALSE,
    col = 'red',
    size = .2,
    linetype = 'dashed'
  )

#model with log(pval) as response and critical value as predictor
#coef(lm(log(p_val) ~ statist, data = A2_mod_crit))
#min(1L, exp(.51014 - 2.62843 * A2_mod))

#linear model with logits as response and sqrt(crit) as predictor
#plot(lm(log(p_val / (1-p_val)) ~ sqrt(statist), data = A2_mod_crit))
#stats::plogis(4.42397965 - 6.42703744 * sqrt(A2_mod))

# P-value comes from back-transformed linear model with logits as response and crit + sqrt(crit) as predictors
# summary(lm(log(p_val / (1-p_val)) ~ statist + sqrt(statist), data = A2_mod_crit))
fm_exp1 <- lm(
  log(p_val / (1 - p_val)) ~ statist + log(statist),
  data = A2_mod_crit_exp
)
fm_exp2 <- lm(log(p_val) ~ 0 + statist + log(statist), data = A2_mod_crit_exp)

ml_exp <- rmetalog::metalog(
  x = A2_mod_crit_exp$statist,
  bounds = 0,
  boundedness = 'sl',
  term_limit = 4,
  probs = A2_mod_crit_exp$prob
)
#pmetalog(ml_exp, q = A2_mod_crit_exp$statist, term = 4) - A2_mod_crit_exp$prob
#pmetalog(ml_exp, q = c(.5, 1, 2, 5), term = 4)

qq_exp <- seq.int(from = .32, to = 4, by = .01)
pp_exp <- rmetalog::pmetalog(ml_exp, q = qq_exp, term = 4)

pfun_exp <- stats::approxfun(
  x = c(0, qq_exp, +Inf),
  y = 1 - c(0, pp_exp, 1),
  method = 'linear'
)

# fm_exp2 has better fit than fm_exp1 for high statist on training data
#+ exp(fitted(fm_exp2)) - A2_mod_crit_exp$p_val
#+ stats::plogis(fitted(fm_exp1)) - A2_mod_crit_exp$p_val
#plot(p_val ~ statist, data = A2_mod_crit_exp, log = 'y')
#points(exp(fitted(fm2)) ~ statist, data = A2_mod_crit, col = "blue", pch = 9, cex = 1)
#points(stats::plogis(fitted(fm)) ~ statist, data = A2_mod_crit, col = "red", pch = 16, cex = .6)
# plot(lm(log(p_val / (1-p_val)) ~ statist + sqrt(statist) + log(statist), data = A2_mod_crit))

# weibull distribution -----

# P-value for Weibull based on Lockhart, 1994 (Table 1)
# interpolation model on logits using critical value and inverse of shape parameter
#
# the P-value for the AD-test depends on the shape parameter estimate
#+cf the MC-study of Lockhart for finite samples but there it assumes a single shape parameter estimate,
#+XXX we might have two! for the time being we use the harmonic mean (the smallest mean)
#+which might overstate shape_inverse and is hence conservative for the P-value

# c (in Lockhart) is inverse of shape parameter
A2_crit_weib <- tibble::tribble(
  ~shape_inv,
  ~`a=0.5`,
  ~`a=0.25`,
  ~`a=0.15`,
  ~`a=0.1`,
  ~`a=0.05`,
  ~`a=0.025`,
  ~`a=0.01`,
  ~`a=0.005`,
  0.00,
  0.292,
  0.395,
  0.467,
  0.522,
  0.617,
  0.711,
  0.836,
  0.931,
  0.05,
  0.295,
  0.399,
  0.471,
  0.527,
  0.623,
  0.719,
  0.845,
  0.941,
  0.10,
  0.298,
  0.403,
  0.476,
  0.534,
  0.631,
  0.728,
  0.856,
  0.954,
  0.15,
  0.301,
  0.408,
  0.483,
  0.541,
  0.640,
  0.738,
  0.869,
  0.969,
  0.20,
  0.305,
  0.414,
  0.490,
  0.549,
  0.650,
  0.751,
  0.885,
  0.986,
  0.25,
  0.309,
  0.421,
  0.498,
  0.559,
  0.662,
  0.765,
  0.902,
  1.007,
  0.30,
  0.314,
  0.429,
  0.508,
  0.570,
  0.676,
  0.782,
  0.923,
  1.030,
  0.35,
  0.320,
  0.438,
  0.519,
  0.583,
  0.692,
  0.802,
  0.947,
  1.057,
  0.40,
  0.327,
  0.448,
  0.532,
  0.598,
  0.711,
  0.824,
  0.974,
  1.089,
  0.45,
  0.334,
  0.469,
  0.547,
  0.615,
  0.732,
  0.850,
  1.006,
  1.125,
  0.50,
  0.342,
  0.472,
  0.563,
  0.636,
  0.757,
  0.879,
  1.043,
  1.167
) %>%
  # scaling for regression models
  dplyr::mutate(shape_inv2 = as.numeric(scale(shape_inv))) %>% #center: 0.25, scale: 0.166
  tidyr::pivot_longer(
    cols = starts_with('a='),
    names_to = 'p_val',
    names_prefix = 'a=',
    names_transform = list(p_val = as.numeric),
    values_to = 'statist'
  ) %>%
  dplyr::mutate(prob = 1L - p_val)

ggplot(
  data = A2_crit_weib,
  aes(x = statist, y = p_val, col = as.character(shape_inv))
) +
  scale_y_log10() +
  geom_point() +
  #geom_line() +
  geom_smooth(method = lm, formula = y ~ x + sqrt(x) + log(x), se = FALSE) +
  labs(col = expression('Shape ' * m^-1))


qq_weib <- seq.int(from = .24, to = 4, by = .01)
# approximation for fixed shape parameter to get p_value (1-CDF) according to given statistic
pfun_weib_fixedShape <- purrr::map(
  .x = unique(A2_crit_weib$shape_inv),
  .f = ~ {
    dfsub <- dplyr::filter(A2_crit_weib, near(shape_inv, .))
    ml <- rmetalog::metalog(
      x = dfsub$statist,
      bounds = 0,
      boundedness = 'sl',
      term_limit = 4,
      term_lower_bound = 4,
      probs = dfsub$prob
    )
    stats::approxfun(
      x = c(0, qq_weib, +Inf),
      y = c(0, 1 - rmetalog::pmetalog(ml, q = qq_weib, term = 4), 1),
      method = 'linear'
    )
  }
)
names(pfun_weib_fixedShape) <- unique(A2_crit_weib$shape_inv)

# approximate for inverse shape parameter
#+use approx-funs for different fixed shapes at specified quantile, so that I can evaluate any shape value
# is vectorized for shape, but not for quantile!!
# pfun_weib0a <- function(q, shape){
#   shape_inv <- pmin.int(0.5, 1/shape) # maximal value of 0.5, see Lockhart, 1994
#   af <- approxfun(x=as.numeric(names(pfun_weib_fixedShape)),
#                   y=purrr::map_dbl(.x = pfun_weib_fixedShape, .f = ~ .(q)), method = 'linear')
#   af(shape_inv)
# }

# vectorized, short code but less efficient because we use approxfun for each row in data.frame again (even if q is identical)
# pfun_weib0b <- function(q, shape){
#   shape_inv <- pmin.int(0.5, 1/shape) # maximal value of 0.5, see Lockhart, 1994
#   # data.frame does the vector recycling
#   data.frame(shape_inv, q) %>%
#     rowwise() %>%
#     mutate(
#       # store approxfuns for shape
#       af = list(approxfun(x=as.numeric(names(pfun_weib_fixedShape)),
#                            y=purrr::map_dbl(.x = pfun_weib_fixedShape, .f = ~ .(q)), method = 'linear')),
#       ##af_lab = env_label(environment(af)), #memory address
#       # value of function
#       afval = af(shape_inv))
#   af(shape_inv)
# }

# approximate for inverse shape parameter
#+use approx-funs for different fixed shapes at specified quantile, so that I can evaluate any shape value
#+vectorized for quantile q and shape parameter
#+uses approxfun for shape only for unique values of q
pfun_weib <- function(q, shape) {
  shape_inv <- pmin.int(0.5, 1 / shape) # maximal value of 0.5, see Lockhart, 1994
  # data.frame does the vector recycling
  combiDF <- data.frame(shape_inv, q)

  purrr::map(.x = unique(q), .f = function(qu) {
    af = stats::approxfun(
      x = as.numeric(names(pfun_weib_fixedShape)),
      y = purrr::map_dbl(.x = pfun_weib_fixedShape, .f = ~ .(qu)),
      method = 'linear'
    )
  }) %>%
    tibble::tibble(q = unique(q), af = .) %>%
    dplyr::inner_join(y = combiDF, by = 'q') %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ### confirm we have a single approxfun per quantile value
      ##aflab = env_label(environment(af)),
      # value of function
      afval = af(shape_inv)
    ) %>%
    dplyr::pull(afval)
}

# make list-object pfun_weib_fixedShape accessible to the function
environment(pfun_weib) <- rlang::env(
  pfun_weib_fixedShape = pfun_weib_fixedShape
)

fm_wei1 <- lm(
  log(p_val / (1 - p_val)) ~ (statist + sqrt(statist)) * shape_inv,
  data = A2_crit_weib
)
summary(fm_wei1)

fm_wei2 <- lm(
  log(p_val) ~ (statist + sqrt(statist)) * shape_inv,
  data = A2_crit_weib
)
summary(fm_wei2)

fm_wei3 <- lm(
  log(p_val) ~ (statist + log(statist)) * shape_inv,
  data = A2_crit_weib
)
summary(fm_wei3)

fm_wei4 <- lm(
  log(p_val) ~ 0 + (statist + sqrt(statist) + log(statist)) * shape_inv2,
  data = A2_crit_weib
)
summary(fm_wei4)

fm_wei5 <- lm(
  log(p_val) ~
    0 +
      statist +
      sqrt(statist) +
      log(statist) +
      statist:shape_inv2 +
      sqrt(statist):shape_inv2 +
      log(statist):shape_inv2,
  data = A2_crit_weib
)
summary(fm_wei5)


sum((exp(fitted(fm_wei2)) - A2_crit_weib$p_val)^2)
sum((exp(fitted(fm_wei3)) - A2_crit_weib$p_val)^2)
sum((exp(fitted(fm_wei4)) - A2_crit_weib$p_val)^2)
sum((exp(fitted(fm_wei5)) - A2_crit_weib$p_val)^2)


# save -----

.ad_pval <- list(
  exponential = pfun_exp,
  weibull = pfun_weib
)

usethis::use_data(.ad_pval, internal = TRUE, overwrite = TRUE)

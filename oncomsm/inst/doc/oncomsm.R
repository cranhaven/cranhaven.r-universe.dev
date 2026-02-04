## ----knitr-options, include=FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7 / 1.61,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(oncomsm)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

## ----msm-1, eval=TRUE, echo=FALSE---------------------------------------------
DiagrammeR::mermaid("
graph LR
  stable -- 1 --> response
  response -- 3 --> progression[progression or death]
  stable -- 2 --> progression[progression or death]
", height = 200)

## ----specify-model------------------------------------------------------------
mdl <- create_srpmodel(
  A = define_srp_prior(
    p_mean = 0.4, p_n = 10,
    median_t_q05 = c(3, 2, 6) - 1, # s->r s->p r->p
    median_t_q95 = c(3, 2, 6) + 1
  ),
  B = define_srp_prior(
    p_mean = 0.6, p_n = 10,
    median_t_q05 = c(2, 8, 3) - 1, # s->r s->p r->p
    median_t_q95 = c(2, 8, 12) + 1,
    shape_q05 = c(2, 2, 0.75),
    shape_q95 = c(2.1, 2.1, 0.76)
  )
)

print(mdl)

## ----plotting-the-prior-------------------------------------------------------
smpl_prior <- sample_prior(mdl, seed = 36L)

# plot(mdl) also works but need to resample prior further below
plot(mdl, parameter_sample = smpl_prior, confidence = 0.75)

## ----prior-predictive---------------------------------------------------------
tbl_prior_predictive <- sample_predictive(
  mdl,
  sample = smpl_prior,
  n_per_group = c(30L, 30L),
  nsim = 100,
  seed = 342
)

print(tbl_prior_predictive, n = 25)

## -----------------------------------------------------------------------------
tbl_prior_predictive %>%
  group_by(group_id, iter, subject_id) %>%
  summarize(
    responder = any(state == "response"),
    .groups = "drop"
  ) %>%
  group_by(group_id) %>%
  summarize(
    p_response = mean(responder),
    se = sd(responder) / sqrt(n())
  )

## -----------------------------------------------------------------------------
tbl_prior_predictive %>%
  distinct(subject_id, iter, state, .keep_all = TRUE) %>%
  group_by(iter, group_id, subject_id) %>%
  summarize(
    dt = t - lag(t),
    from = lag(state),
    to = state,
    .groups = "drop"
  ) %>%
  filter(to != "stable") %>%
  group_by(group_id, from, to) %>%
  summarize(
    `median transition time` = median(dt),
    .groups = "drop"
  )

## ----convert-to-mstate--------------------------------------------------------
tbl_mstate <- tbl_prior_predictive %>%
  filter(iter == 1) %>%
  visits_to_mstate(mdl)

tbl_mstate

## ----plot-preior-predictive, fig.height=6-------------------------------------
plot_mstate(tbl_mstate, mdl)

## ----prior-predictive-fixed---------------------------------------------------
sample_predictive(
    mdl,
    sample = smpl_prior,
    p = c(0.1, 0.9),
    n_per_group = c(30L, 30L),
    nsim = 100,
    seed = 3423423
  ) %>%
  group_by(group_id, iter, subject_id) %>%
  summarize(
    responder = any(state == "response"),
    .groups = "drop"
  ) %>%
  group_by(group_id) %>%
  summarize(
    p_response = mean(responder),
    se = sd(responder) / sqrt(n())
  )

## -----------------------------------------------------------------------------
tbl_data_interim <- sample_predictive(
    mdl,
    sample = smpl_prior,
    p = c(0.2, 0.8),
    n_per_group = c(30L, 30L),
    nsim = 1,
    seed = 42L
  ) %>%
  filter(
    t <= 15
  )

## ---- fig.height=6------------------------------------------------------------
tbl_data_interim %>%
  visits_to_mstate(mdl, now = 15) %>%
  plot_mstate(mdl, relative_to_sot = FALSE, now = 15)

## -----------------------------------------------------------------------------
tbl_data_interim %>%
  group_by(group_id, iter, subject_id) %>%
  summarize(
    responder = any(state == "response"),
    .groups = "drop"
  ) %>%
  group_by(group_id) %>%
  summarize(
    p_response = mean(responder),
    se = sd(responder) / sqrt(n())
  )

## -----------------------------------------------------------------------------
smpl_posterior <- sample_posterior(mdl, tbl_data_interim, seed = 43L)
# plot under posterior
plot(mdl, parameter_sample = smpl_posterior, confidence = 0.75)
# calculate posterior quantiles of response probability
smpl_posterior %>%
  parameter_sample_to_tibble(mdl, .) %>%
  filter(parameter == "p") %>%
  group_by(group_id) %>%
  summarize(
    p_posterior_mean = median(value),
    q25 = quantile(value, probs = .25),
    q75 = quantile(value, probs = .75)
  )

## -----------------------------------------------------------------------------
mdl2 <- create_srpmodel(
  A = define_srp_prior(),
  B = define_srp_prior()
)
smpl_posterior2 <- sample_posterior(mdl2, tbl_data_interim, seed = 43L)
# plot under posterior
plot(mdl2, parameter_sample = smpl_posterior2, confidence = 0.75)
# calculate posterior quantiles of response probability
smpl_posterior2 %>%
  parameter_sample_to_tibble(mdl2, .) %>%
  filter(parameter == "p") %>%
  group_by(group_id) %>%
  summarize(
    p_posterior_mean = median(value),
    q25 = quantile(value, probs = .25),
    q75 = quantile(value, probs = .75)
  )

## ----session-info-------------------------------------------------------------
sessionInfo()


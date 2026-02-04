## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7 / 1.61,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(oncomsm)

## -----------------------------------------------------------------------------
mdl <- create_srpmodel(
  A = define_srp_prior(
    median_t_q05 = c(1, 4, 12), # shorter time to response than progression
    median_t_q95 = c(6, 8, 36),
    # essentially fixed shape:
    shape_q05 = c(0.99, 0.99, 0.99),
    shape_q95 = c(1.00, 1.00, 1.00)
  )
)

plot(mdl, confidence = 0.9)

## -----------------------------------------------------------------------------
tbl_data_interim <- tibble::tribble(
    ~subject_id, ~t,   ~state,
        "subj1",  1, "stable",
        "subj1",  5, "stable",
        "subj2",  1, "stable",
        "subj2",  5, "stable",
        "subj3",  1, "stable",
        "subj3",  5, "stable",
        "subj4",  1, "stable",
        "subj4",  5, "stable",
        "subj5",  0, "stable",
        "subj5",  1, "response",
        "subj5",  2, "EOF",
        "subj6",  0, "stable",
        "subj6",  6, "progression"
  ) %>%
  mutate(group_id = "A")
plot_mstate(
  visits_to_mstate(tbl_data_interim, mdl),
  mdl,
  relative_to_sot = FALSE
)

## -----------------------------------------------------------------------------
p_posterior <- parameter_sample_to_tibble(
    mdl,
    sample_posterior(mdl, tbl_data_interim)
  ) %>%
  filter(parameter == "p") %>%
  pull(value)

hist(p_posterior)

summary(p_posterior)


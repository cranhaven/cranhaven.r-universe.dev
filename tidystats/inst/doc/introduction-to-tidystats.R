## -----------------------------------------------------------------------------
model <- lm(extra ~ group, data = sleep)

## ----setup, message = FALSE---------------------------------------------------
library(tidystats)

data <- quote_source

## ----t-test-------------------------------------------------------------------
t_test <- t.test(response ~ source, data = data)
t_test

## ----regression1--------------------------------------------------------------
lm_us_or_not <- lm(response ~ source * us_or_international, data = data)
summary(lm_us_or_not)

## ----regression2--------------------------------------------------------------
lm_age <- lm(response ~ source * age, data = data)
summary(lm_age)

## ----tidystats-example--------------------------------------------------------
# Create an empty list to store the analyses in
statistics <- list()

# Add the analyses
statistics <- statistics |>
  add_stats(
    t_test,
    preregistered = TRUE, type = "primary",
    notes = "A t-test comparing the effect of source on the quote rating."
  ) |>
  add_stats(
    lm_us_or_not,
    preregistered = FALSE, type = "exploratory",
    notes = "Interaction effect with being from the U.S. or not."
  ) |>
  add_stats(lm_age)

## ----saving, eval = FALSE-----------------------------------------------------
# write_stats(statistics, "lorge-curtiss-1936-replication.json")


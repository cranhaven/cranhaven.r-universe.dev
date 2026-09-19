## ----setup, include=FALSE-----------------------------------------------------
use_saved_results <- TRUE

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  eval = !use_saved_results,
  message = FALSE,
  warning = FALSE
)

if (use_saved_results) {
  results <- readRDS("vignette_fs.rds")
  model_df <- results$model_df
  boot_df <- results$boot_df
}

## ----eval=TRUE, class.source = "fold-hide"------------------------------------
library(dplyr); library(tidyr); library(purrr) # Data wrangling
library(ggplot2); library(stringr) # Plotting
library(tidyfit) # Model fitting

# Max model size
MODEL_SIZE <- 10

## ----eval=TRUE----------------------------------------------------------------
# Load the data
data <- readRDS("FRED-MD.rds")

## ----eval=TRUE----------------------------------------------------------------
data <- data |>
  arrange(date) |>
  # Shift the target by 1 month
  mutate(Target = lead(INDPRO)) |>
  drop_na() |>
  select(-date)

data

## -----------------------------------------------------------------------------
# # Correlation
# algorithms_df <- data |>
#   regress(Target ~ ., Correlation = m("cor"))

## -----------------------------------------------------------------------------
# # RReliefF
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       regress(Target ~ ., RReliefF = m("relief"))
#   )

## -----------------------------------------------------------------------------
# # Information Gain
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       # Split target into buckets
#       mutate(Target = as.factor(ntile(Target, 10))) |>
#       regress(Target ~ .,
#               `Information Gain` = m("relief", estimator = "InfGain"))
#   )

## -----------------------------------------------------------------------------
# # Forward Selection
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       regress(Target ~ .,
#               `Forward Selection` = m("subset", method = "forward", nvmax = MODEL_SIZE))
#   )

## -----------------------------------------------------------------------------
# # Backward Elimination
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       regress(Target ~ .,
#               `Backward Elimination` = m("subset", method = "backward", nvmax = MODEL_SIZE))
#   )

## -----------------------------------------------------------------------------
# # MRMR
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       regress(Target ~ ., MRMR = m("mrmr", feature_count = MODEL_SIZE))
#   )

## -----------------------------------------------------------------------------
# # LASSO
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       regress(Target ~ .,
#               `LASSO` = m("lasso", pmax = MODEL_SIZE + 1),
#               .cv = "rolling_origin",
#               .cv_args = list(initial = 120, assess = 24, skip = 23)
#               )
#   )

## -----------------------------------------------------------------------------
# # BMA
# algorithms_df <- algorithms_df |>
#   bind_rows(
#     data |>
#       regress(Target ~ .,
#               BMA = m("bma", burn = 10000, iter = 100000,
#                       mprior.size = MODEL_SIZE, mcmc = "rev.jump"))
#   )

## -----------------------------------------------------------------------------
# # Random Forest Importance
# nonlinear_algorithms_df <- data |>
#       regress(Target ~ ., `RF Importance` = m("rf"))

## -----------------------------------------------------------------------------
# coef_df <- coef(algorithms_df) |>
#   unnest(model_info) |>
#   bind_rows(explain(nonlinear_algorithms_df))

## -----------------------------------------------------------------------------
# model_df <- coef_df |>
#   # Always remove the intercept
#   filter(term != "(Intercept)") |>
# 
#   mutate(selected = case_when(
#     # Extract top 10 largest scores
#     model %in% c("Correlation", "RReliefF", "Information Gain") ~
#       rank(-abs(estimate)) <= MODEL_SIZE,
#     # BMA features are selected using the posterior inclusion probability
#     model == "BMA" ~ rank(-pip) <= MODEL_SIZE,
#     # The RF importance is stored in a separate column (%IncMSE)
#     model == "RF Importance" ~ rank(-importance) <= MODEL_SIZE,
#     # For all other methods keep all features
#     TRUE ~ TRUE
#   )) |>
# 
#   # Keep only included terms
#   filter(selected) |>
#   select(model, term)

## -----------------------------------------------------------------------------
# model_df <- model_df |>
#   bind_rows(tibble(
#     model = "Domain Expert",
#     term = c("NAPMNOI", "ANDENOx", "CLAIMSx", "ACOGNO",
#              "S&P 500", "T10YFFM", "PERMIT", "AWHMAN")
#   ))

## ----eval=T, class.source = 'fold-hide', fig.width=7, fig.height=2.5, fig.align='center'----
model_df |>
  # Add 'FALSE' entries, when a feature is not selected
  mutate(selected = TRUE) |>
  spread(term, selected) |>
  gather("term", "selected", -model) |>
  # Plotting color
  mutate(selected = ifelse(is.na(selected), "white", "darkblue")) |>
  # Fix plotting order
  group_by(term) |>
  mutate(selected_sum = sum(selected=="darkblue")) |>
  ungroup() |>
  arrange(desc(selected_sum)) |>
  mutate(term = factor(term, levels = unique(term))) |>
  mutate(model = factor(model, levels = unique(model_df$model))) |>
  ggplot(aes(term, model)) +
  geom_tile(aes(fill = selected)) +
  theme_bw(8) +
  scale_fill_identity() +
  xlab(element_blank()) + ylab(element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## -----------------------------------------------------------------------------
# model_names <- unique(model_df$model)
# 
# # Retrieve selected variables
# selected_vars_list <- model_names |>
#   map(function(mod) {
#     model_df |>
#       filter(model == mod) |>
#       pull(term)
#   })
# names(selected_vars_list) <- model_names
# 
# # Bootstrap resampling & regression
# boot_models_df <- selected_vars_list |>
#   map_dfr(function(selected_vars) {
#     data |>
#       select(all_of(c("Target", selected_vars))) |>
# 
#       regress(Target ~ .,
#               # Use linear regression
#               m("lm"),
#               # Bootstrap settings (see ?rsample::bootstraps)
#               .cv = "bootstraps", .cv_args = list(times = 100),
#               # Make sure the results for each slice are returned
#               .force_cv = T, .return_slices = T)
#   }, .id = "model")
# 
# # Finally, extract R2 from the model results
# boot_df <- boot_models_df |>
#   mutate(R2 = map_dbl(model_object, function(obj) summary(obj)$r.squared)) |>
#   select(model, R2)

## ----eval=T, class.source = 'fold-hide', fig.width=7, fig.height=2.5, fig.align='center'----
boot_df |>
  group_by(model) |>
  mutate(upper = mean(R2) + 2 * sd(R2) / sqrt(n()),
         lower = mean(R2) - 2 * sd(R2) / sqrt(n())) |>
  mutate(model = str_wrap(model, 10)) |>
  mutate(model = factor(model, levels = str_wrap(unique(model_df$model), 10))) |>
  ggplot(aes(model)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 0.25, width = 0.25) +
  theme_bw(8) +
  xlab(element_blank()) + ylab("R2 statistic")


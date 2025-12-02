## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-package-------------------------------------------------------------
library(riemstats)
library(riemtan) # Required for CSuperSample and CSample classes
data("airm")  # Load the AIRM metric

# Create example SPD matrices for demonstration
test_pd_mats <- list(
  Matrix::Matrix(c(2.0, 0.5, 0.5, 3.0), nrow = 2) |>
    Matrix::nearPD() |> _$mat |> Matrix::pack(),
  Matrix::Matrix(c(1.5, 0.3, 0.3, 2.5), nrow = 2) |>
    Matrix::nearPD() |> _$mat |> Matrix::pack()
)

# Create samples for two sites/groups
sample1 <- test_pd_mats |>
  purrr::map(\(x) (2 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
  CSample$new(metric_obj = airm)
sample2 <- test_pd_mats |> CSample$new(metric_obj = airm)

# Combine into a supersample for analysis
super_sample <- list(sample1, sample2) |> CSuperSample$new()

## ----harmonization------------------------------------------------------------
# Apply ComBat harmonization for SPD matrices
# Input must be a CSuperSample object
harmonized_super_sample <- combat_harmonization(super_sample)

# Alternative: Rigid geometric alignment
# Also works with CSuperSample objects
aligned_super_sample <- rigid_harmonization(super_sample)

## ----anova-analysis, eval=FALSE-----------------------------------------------
# # Fréchet ANOVA for overall group differences
# # Returns p-value directly
# frechet_p_value <- frechet_anova(super_sample)
# 
# # Riemannian ANOVA with permutation test inference
# # Can use log_wilks_lambda (default) or pillais_trace as stat_fun
# riemann_p_value_wilks <- riem_anova(
#   ss = super_sample,
#   stat_fun = log_wilks_lambda,
#   nperm = 100 # Number of permutations (use 1000+ in practice)
# )
# 
# # Using Pillai's trace
# riemann_p_value_pillai <- riem_anova(
#   ss = super_sample,
#   stat_fun = pillais_trace,
#   nperm = 100 # Number of permutations (use 1000+ in practice)
# )
# 
# # Display results
# cat("Fréchet ANOVA p-value:", frechet_p_value, "\n")
# cat("Riemannian ANOVA (Wilks) p-value:", riemann_p_value_wilks, "\n")
# cat("Riemannian ANOVA (Pillai) p-value:", riemann_p_value_pillai, "\n")


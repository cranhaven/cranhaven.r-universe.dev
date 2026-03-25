## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SQUIRE)

## ----example, eval=FALSE------------------------------------------------------
# # Generate example data
# set.seed(123)
# germination_data <- data.frame(
#   time = rep(0:7, times = 12),
#   treatment = rep(c("Control", "Inhibitor", "Promoter"), each = 32),
#   replicate = rep(rep(1:4, each = 8), times = 3),
#   response = c(
#     # Control: normal germination
#     rnorm(32, mean = rep(seq(0, 80, length.out = 8), 4), sd = 3),
#     # Inhibitor: reduced germination
#     rnorm(32, mean = rep(seq(0, 60, length.out = 8), 4), sd = 3),
#     # Promoter: enhanced germination
#     rnorm(32, mean = rep(seq(0, 95, length.out = 8), 4), sd = 3)
#   )
# )
# 
# # Run SQUIRE analysis
# results <- SQUIRE(
#   data = germination_data,
#   treatments = c("Control", "Inhibitor", "Promoter"),
#   control_treatment = "Control",
#   verbose = FALSE
# )
# 
# # Check results
# if (results$optimization_performed) {
#   print("Optimization was performed. Parameter estimates:")
#   print(results$parameters$parameter_matrix)
# } else {
#   print(paste("Optimization not performed:", results$validation_results$reason))
# }


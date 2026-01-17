

# Taken from Erlend Sandorf
# Example file for creating a simple MNL design
#
rm(list = ls(all = TRUE))
library(spdesign)

# Define the list of utility functions ----
#' Specifying a utility function with 3 attributes and a constant for the
#' SQ alternative. The design has 20 rows.
utility <- list(
  alt1 = "b_x1[0.1]  * x1[1:5] + b_x2[0.4] * x2[c(0, 1)] ",
  alt2 = "b_x1       * x1      + b_x2      * x2          "
)

# Generate designs ----
design <- generate_design(utility, rows = 20,
                          model = "mnl", efficiency_criteria = "d-error",
                          algorithm = "rsc", draws = "scrambled-sobol",
                          control = list(
                            max_iter = 2000,
                            max_no_improve = 5000
                          ))

saveRDS(design,"inst/extdata/spdesigns/designs/twoattr.RDS")

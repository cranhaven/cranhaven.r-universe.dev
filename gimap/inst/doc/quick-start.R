## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, results = 'hide'-------------------------------------------
library(gimap)

## ----echo = FALSE, results = 'hide'-------------------------------------------
library(dplyr)

## ----eval = FALSE-------------------------------------------------------------
# output_dir <- "output_timepoints"
# dir.create(output_dir, showWarnings = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# example_data <- get_example_data("count")

## ----eval = FALSE-------------------------------------------------------------
# counts <- example_data %>%
#   select(c("Day00_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC")) %>%
#   as.matrix()

## ----eval = FALSE-------------------------------------------------------------
# pg_ids <- example_data %>%
#   dplyr::select("id")

## ----eval = FALSE-------------------------------------------------------------
# sample_metadata <- data.frame(
#   col_names = c("Day00_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC"),
#   day = as.numeric(c("0", "22", "22", "22")),
#   rep = as.factor(c("RepA", "RepA", "RepB", "RepC"))
# )

## ----eval = FALSE-------------------------------------------------------------
# gimap_dataset <- setup_data(
#   counts = counts,
#   pg_ids = pg_ids,
#   sample_metadata = sample_metadata
# )

## ----eval = FALSE-------------------------------------------------------------
# run_qc(gimap_dataset,
#   output_file = file.path(output_dir, "example_qc_report.Rmd"),
#   overwrite = TRUE,
#   plots_dir = "plots",
#   quiet = TRUE
# )

## ----eval = FALSE-------------------------------------------------------------
# gimap_dataset <- gimap_dataset %>%
#   gimap_filter() %>%
#   gimap_annotate(cell_line = "HELA") %>%
#   gimap_normalize(
#     timepoints = "day"
#   ) %>%
#   calc_gi()

## ----eval = FALSE-------------------------------------------------------------
# gimap_dataset$gi_scores %>%
#   dplyr::arrange(fdr) %>%
#   head() %>%
#   knitr::kable(format = "html")

## ----eval = FALSE-------------------------------------------------------------
# plot_exp_v_obs_scatter(gimap_dataset)
# 
# # Save it to a file
# ggsave(file.path(output_dir, "exp_v_obs_scatter.png"))

## ----eval = FALSE-------------------------------------------------------------
# plot_rank_scatter(gimap_dataset)
# 
# # Save it to a file
# ggsave(file.path(output_dir, "plot_rank_scatter.png"))

## ----eval = FALSE-------------------------------------------------------------
# plot_volcano(gimap_dataset)
# 
# # Save it to a file
# ggsave(file.path(output_dir, "volcano_plot.png"))

## ----eval = FALSE-------------------------------------------------------------
# # "CNOT8_CNOT7" is top result so let's plot that
# plot_targets(gimap_dataset, target1 = "CNOT8", target2 = "CNOT7")
# 
# # Save it to a file
# ggsave(file.path(output_dir, "CNOT8_CNOT7.png"))

## -----------------------------------------------------------------------------
sessionInfo()


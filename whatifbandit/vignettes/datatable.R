library(data.table)
library(whatifbandit)

tanf_large <- tanf
setDT(tanf_large)
for (i in 1:9) {
  tanf_large <- rbindlist(list(tanf_large, tanf_large))
}

setorder(tanf_large, appt_date)
tanf_large[, id := .I]

set.seed(523432453)
dataframe_time <- system.time(single_mab_simulation(
  data = as.data.frame(tanf_large),
  assignment_method = "Batch",
  period_length = 3000,
  algorithm = "Thompson",
  whole_experiment = FALSE,
  perfect_assignment = TRUE,
  prior_periods = "All",
  blocking = FALSE,
  data_cols = c(
    id_col = "id",
    success_col = "success",
    condition_col = "condition"
  )
))

datatable_time <- system.time(single_mab_simulation(
  data = tanf_large,
  assignment_method = "Batch",
  period_length = 3000,
  algorithm = "UCB1",
  whole_experiment = FALSE,
  perfect_assignment = TRUE,
  prior_periods = "All",
  blocking = FALSE,
  data_cols = c(
    id_col = "id",
    success_col = "success",
    condition_col = "condition"
  )
))

datatable_time <- as.numeric(datatable_time["elapsed"])
dataframe_time <- as.numeric(dataframe_time["elapsed"])
save.image(file = "datatable.RData")

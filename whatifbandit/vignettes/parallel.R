library(future)
library(whatifbandit)
set.seed(532454)
seeds <- sample.int(1000000, 100, replace = FALSE)
plan("multisession", workers = 4)
parallel_time <- system.time(
  multiple_sims <- multiple_mab_simulation(
    data = tanf,
    assignment_method = "Date",
    time_unit = "Month",
    period_length = 1,
    algorithm = "Thompson",
    whole_experiment = FALSE,
    perfect_assignment = TRUE,
    prior_periods = "All",
    blocking = TRUE,
    block_cols = c("service_center"),
    data_cols = c(
      id_col = "ic_case_id",
      date_col = "appt_date",
      success_col = "success",
      condition_col = "condition",
      month_col = "recert_month",
      success_date_col = "date_of_recert",
      assignment_date_col = "letter_sent_date"
    ),
    keep_data = FALSE,
    times = 100,
    seeds = seeds
  )
)
plan("sequential")
save.image(file = "parallel.RData")

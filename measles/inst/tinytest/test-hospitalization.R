set.seed(312)

p_hosp <- 0.1
p_rec <- 1/3

# Adjusting hospitalization rate for testing purposes.
# p_hosp is the *target* probability that an infected individual is ever
# hospitalized over the course of their rash, whereas ModelMeaslesSchool
# expects a per-time-step hospitalization_rate that acts together with the
# per-time-step recovery probability p_rec.
#
# Because of this interaction, using p_hosp directly as hospitalization_rate
# would not yield an overall hospitalization proportion of p_hosp in the
# simulation. The expression below applies the algebraic adjustment implied
# by the model's transition structure so that, in expectation, the simulated
# proportion of hospitalized cases (ans$hosp / ans$outbreak_size) is close
# to p_hosp, as checked by the test at the end of this file.
hosp_rate <- p_hosp * (1-p_rec) / (1 - p_hosp)

m_model <- ModelMeaslesSchool(
  n = 500L,
  prevalence = 1,
  rash_period = as.integer(1/p_rec),
  hospitalization_rate = hosp_rate,
  prop_vaccinated = 0.5,
  quarantine_period = -1
)

run_multiple(
  m_model,
  ndays = 200,
  nsims = 1000,
  saver = make_saver(
    "transition", "outbreak_size", "transmission", "hospitalizations"
    ),
  nthreads = 2,
  seed = 1123
)

ans <- run_multiple_get_results(
  m_model, freader = data.table::fread, nThread = 4, nthreads = 1L)

library(data.table)
ans_transition <- ans$transition
ans_outbreak_size <- ans$outbreak_size
ans_transmission <- ans$transmission
ans_hospitalizations <- ans$hospitalizations

mean_o_s_transmission <- ans_transmission[, .N, by = "sim_num"]

mean_o_s <- ans_outbreak_size[date == max(date)][,
  outbreak_size, by = .(sim_num)]

expect_equal(mean_o_s_transmission$N, mean_o_s$outbreak_size)

to_hosp <- ans_transition[
  (from %in% c("Rash", "Isolated")) &
  (to %in% c("Hospitalized", "Detected Hospitalized"))][, .(hosp = sum(counts)), by = .(sim_num)]

to_hosp <- merge(
  to_hosp,
  data.table(sim_num = 1L:nrow(mean_o_s)),
  all = TRUE
  )

to_hosp[, hosp:= fcoalesce(hosp, 0L)]

ans <- merge(mean_o_s, to_hosp)

if (interactive()) {
  message(
    "Mean outbreak size      : ", round(mean(ans$outbreak_size), 2), "\n",
    "Mean hospitalizations   : ", round(mean(ans$hosp), 2), "\n",
    "Proportion hospitalized : ", round(mean(ans$hosp/ans$outbreak_size), 2), "\n",
    "Quantiles [.025, 0.5, .975]  : [", paste(
      ans[, quantile(hosp/outbreak_size, probs = c(.025, 0.5, .975))] |>
        round(2),
      collapse = "; "
    ), "]"
  )
}

# The empirical hospitalization rate comes from a stochastic simulation
# (finite population, 200 days, 1000 simulations), so it will not match
# the target p_hosp exactly. A 10 percentage point tolerance keeps the
# test robust to Monte Carlo variability while still detecting substantial
# deviations from the intended hospitalization rate.
expect_true(
  abs(mean(ans$hosp/ans$outbreak_size) - p_hosp) < .1
)

# Comparing the hospitalizations we retrieved from the 
# "hospitalizations" saver vs that from the transition matrix
# (should be the same)

# Computing from the data
mean_hosp <- ans_hospitalizations[, .(real_hosp=sum(count)), by = .(sim_num)]

ans <- merge(ans, mean_hosp, by = "sim_num")
expect_true(ans[, all(hosp == real_hosp)])
library(testthat)
library(future.apply)
library(SPARSEMODr)
# library(tidyverse)

future::plan("multisession")

context("model")

# Read in the example data:
ex_dir <- system.file(
  "extdata", "sparsemodr_example.Rdata", package="SPARSEMODr", mustWork=TRUE)
load(ex_dir)
n_pop <- length(dat_list[["pop_N"]])

# Set up realizations:
realz_seeds <- 1:2
n_realz <- length(realz_seeds)

# Data for testing
input_beta <-           c(   0.3,   0.3,  0.08,  0.08,  0.15)
input_dist_phi <-       c(   200,   200,    20,   150,   150)
input_m <-              c( 0.002, 0.002, 0.002,  0.02,  0.02)
input_imm_frac <-       c(  0.02,  0.02,  0.02,  0.02,  0.02)
input_window_length <-  c(     0,    36,    10,    35,   169)

# User creates the time_windows object here
tw <- time_windows(beta = input_beta,
                   dist_phi = input_dist_phi,
                   m = input_m,
                   imm_frac = input_imm_frac,
                   window_length = input_window_length)

# Randomly generate initial conditions for
# EXPOSED class:
E_pops <- vector("numeric", length = n_pop)
n_initial_E = 40
# (more exposed in larger populations)
these_E = sample.int(n_pop,
                     size = n_initial_E,
                     replace = TRUE,
                     prob = dat_list$pop_N)
for(i in 1:n_initial_E){
  E_pops[these_E[i]] = E_pops[these_E[i]] + 1
}

# Inputs for testing the models
N_pops <- as.integer(dat_list[["pop_N"]])
S_pops <- N_pops - E_pops
I_pops <- vector("integer", length = n_pop)
R_pops <- vector("integer", length = n_pop)
I_asym_pops <- vector("integer", length = n_pop)
I_presym_pops <- vector("integer", length = n_pop)
I_sym_pops <- vector("integer", length = n_pop)
I_home_pops <- vector("integer", length = n_pop)
I_hosp_pops <- vector("integer", length = n_pop)
I_icu1_pops <- vector("integer", length = n_pop)
I_icu2_pops <- vector("integer", length = n_pop)
D_pops <- vector("integer", length = n_pop)

get_result <- function(input_realz_seeds, control = NULL){

    with(dat_list, model_parallel(
        input_census_area = census_area,
        input_dist_mat = dist_vec,
        input_realz_seeds = input_realz_seeds,
        input_tw = tw,
        control = control)
    )

}


# User creates control list of parameters
covid19_control <- covid19_control(input_N_pops = N_pops,
                                   # input_S_pops = S_pops,
                                   input_E_pops = E_pops,
                                   input_I_asym_pops=I_asym_pops,
                                   input_I_presym_pops=I_presym_pops,
                                   input_I_sym_pops=I_sym_pops,
                                   input_I_home_pops=I_home_pops,
                                   input_I_hosp_pops=I_hosp_pops,
                                   input_I_icu1_pops=I_icu1_pops,
                                   input_I_icu2_pops=I_icu2_pops,
                                   input_R_pops=R_pops,
                                   input_D_pops=D_pops)
seir_control <- seir_control(input_N_pops = N_pops,
                             # input_S_pops = S_pops,
                             input_E_pops = E_pops,
                             input_I_pops = I_pops,
                             input_R_pops = R_pops)


result_fwd <- get_result(input_realz_seeds = realz_seeds,
                         control = covid19_control)

result_rev <- get_result(input_realz_seeds = rev(realz_seeds),
                         control = covid19_control)

# Shut down parallel workers
future::plan("sequential")

rows_per_realz <- n_pop * (sum(input_window_length))
expected_rows <- n_realz * rows_per_realz

test_that("model returns expected number of rows", {
    expect_equal(nrow(result_fwd), expected_rows)
    expect_equal(nrow(result_rev), expected_rows)
})

# Test fails if loop does not run n_times
expected_seed_fwd <- rep(realz_seeds, each=rows_per_realz)
test_that("fwd seed ok", {
    expect_equal(result_fwd$pops.seed, expected_seed_fwd)
})

expected_seed_rev <- rep(rev(realz_seeds), each=rows_per_realz)
test_that("rev seed ok", {
    expect_equal(result_rev$pops.seed, expected_seed_rev)
})

fwd1 <- result_fwd[pops.seed==1]
fwd2 <- result_fwd[pops.seed==2]
rev1 <- result_rev[pops.seed==1]
rev2 <- result_rev[pops.seed==2]

test_that("fwd and rev results identical for same seed", {
    expect_true(identical(fwd1, rev1))
    expect_true(identical(fwd2, rev2))
})

test_that("fwd results NOT identical for different seed", {
    expect_false(identical(fwd1, fwd2))
})

test_that("rev results NOT identical for different seed", {
    expect_false(identical(rev1, rev2))
})

#### QUICK FIGURES:

# test_df =
#   result_rev %>%
#   filter(pops.seed == 1, pops.pop == 10)
#
# plot(test_df$pops.E_pop ~ test_df$pops.time, type = "l")
# plot(test_df$pops.D_pop ~ test_df$pops.time, type = "l")
# plot(test_df$events.total_hosp ~ test_df$pops.time, type = "l")

message("COMPLETED successfully.")

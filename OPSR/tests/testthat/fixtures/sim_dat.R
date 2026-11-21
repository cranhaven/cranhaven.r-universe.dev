set.seed(123)
sim_dat <- opsr_simulate()
saveRDS(sim_dat, "./tests/testthat/fixtures/sim_dat.rds")

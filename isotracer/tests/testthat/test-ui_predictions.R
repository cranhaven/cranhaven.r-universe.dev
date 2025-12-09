### * Setup

n_cores <- min(2, parallel::detectCores())
n_chains <- max(n_cores, 2)

run_mcmc <- function(...) {
  isotracer:::run_mcmc(..., cores = n_cores, chains = n_chains)
}

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

### * predict() method

test_that("Basic prediction works", {
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(tibble::tibble(comps = c("NH4", "algae", "daphnia"),
                                sizes = c(0.2, 1, 2),
                                props = c(0.8, 0.004, 0.004)),
                 comp = "comps", size = "sizes", prop = "props") %>%
        set_params(c("eta" = 0.2, "lambda_algae" = 0, "lambda_daphnia" = 0, 
                     "lambda_NH4" = 0, "upsilon_algae_to_daphnia" = 0.15, 
                     "upsilon_NH4_to_algae" = 0.25, "upsilon_daphnia_to_NH4" = 0.04,
                     "zeta" = 0.1)) %>%
        project(end = 10) %>%
        set_priors(normal_p(0, 4), "", quiet = TRUE)
    z <- sample_from(x, at = c(0, 1, 1.5, 2, 2.5, 3))
    for (c in c("size", "prop")) {
        z[[c]] <- signif(z[[c]], 3)
    }
    z <- z %>% dplyr::rename(time.day = time, species = comp,
                      biomass = size, prop15N = prop)
    y <- x %>%
        set_obs(z, comp = "species", size = "biomass", prop = "prop15N",
                time = "time.day")
    capture_warnings(capture_output({ f <- run_mcmc(y, iter = 10) }))
    p <- predict(y, f, cores = n_cores)
    # Check output format
    expect_equal(nrow(p), 1)
    expect_true("prediction" %in% colnames(p))
    p <- p$prediction[[1]]
    expect_is(p, c("tbl_df", "tbl", "data.frame"))
    expect_true(nrow(p) > 0)
    expect_setequal(colnames(p), c("time", "size_low", "size_mean",
                                   "size_high", "prop_low", "prop_mean",
                                   "prop_high", "compartment"))
    expect_true(all(p >= 0))
})

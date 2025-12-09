### * Setup

n_cores <- min(2, parallel::detectCores())
n_chains <- max(n_cores, 2)

run_mcmc <- function(...) {
  isotracer:::run_mcmc(..., cores = n_cores, chains = n_chains)
}

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

no_w <- function(exp) {
    capture_warnings(capture_output(exp))
}

### * plot() methods

test_that("Basic plotting does not crash", {
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
        set_priors(normal_p(0, 4), "", quiet = TRUE) %>%
        project(end = 10)
    z <- sample_from(x, at = c(0, 1, 1.5, 2, 2.5, 3))
    for (c in c("size", "prop")) {
        z[[c]] <- signif(z[[c]], 3)
    }
    z <- z %>% dplyr::rename(time.day = time, species = comp,
                      biomass = size, prop15N = prop)
    y <- x %>%
        set_obs(z, comp = "species", size = "biomass", prop = "prop15N",
                time = "time.day")
    expect_error(plot(y), NA)
    capture_warnings(capture_output({ f <- run_mcmc(y, iter = 10) }))
    expect_error(plot(f), NA)
    expect_error(plot(f, hist = FALSE), NA)
    p <- predict(y, f, cores = n_cores)
    expect_error(plot(p), NA)
    expect_error(plot(p, facet_row = "type", facet_col = "compartment"), NA)
    expect_error(plot(p, facet_col = "compartment", facet_row = "type", log = TRUE, scale = "all"),
                 NA)
})

### * ggtopo()

test_that("ggtopo() does not crash", {
  if (requireNamespace("ggraph")) {
    # The tests below use a warning catcher at the moment (2023-09-20) because
    # of a warning due to a deprecated ggplot feature used by ggraph. They can
    # be used with the warning handler once ggraph is updated.
    expect_error(no_w(print(ggtopo(aquarium_mod, edge = "line"))), NA)
    expect_error(no_w(print(ggtopo(trini_mod))), NA)
    expect_error(no_w(print(ggtopo(trini_mod, layout = "sugiyama"))), NA)
    }
})

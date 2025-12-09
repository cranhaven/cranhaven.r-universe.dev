### * Setup

library(magrittr)

ITERS <- 20
DRAWS <- 10
FORCE_STAN_COVR <- TRUE

n_cores <- min(2, parallel::detectCores())
n_chains <- max(n_cores, 2)

run_mcmc <- function(...) {
    isotracer:::run_mcmc(..., cores = n_cores, chains = n_chains)
}

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

ci_overlap <- function(x, y) {
    f1 <- x
    f2 <- y
    q1 <- summary(f1)$quantiles[, c(1, 5)]
    q1 <- q1[order(rownames(q1)), ]
    q2 <- summary(f2)$quantiles[, c(1, 5)]
    q2 <- q2[order(rownames(q2)), ]
    stopifnot(nrow(q1) == nrow(q2))
    stopifnot(all(rownames(q1) == rownames(q2)))
    stopifnot(all(colnames(q1) == c("2.5%", "97.5%")))
    stopifnot(all(colnames(q2) == c("2.5%", "97.5%")))
    overlaps <- lapply(seq_len(nrow(q1)), function(i) {
        get_overlap(q1[i,], q2[i,])
    })
    q1_overlap_frac <- sapply(seq_len(nrow(q1)), function(i) {
        diff(overlaps[[i]]) / diff(q1[i,])
    })
    q2_overlap_frac <- sapply(seq_len(nrow(q2)), function(i) {
        diff(overlaps[[i]]) / diff(q2[i,])
    })
    o <- cbind(q1_overlap_frac, q2_overlap_frac)
    colnames(o) <- c("overlap_x", "overlap_y")
    rownames(o) <- rownames(q1)
    return(o)
}

get_overlap <- function(x, y) {
    x <- sort(x)
    y <- sort(y)
    left <- max(x[1], y[1])
    right <- min(x[2], y[2])
    if (left > right) return(c(0, 0))
    return(c(left, right))
}

### * Test both Euler and matrix exponential methods

for (solver in c("euler", "matrix_exp")) {

    ### ** Two compartments

    test_that("Model with two compartments works", {
        m <- new_networkModel() %>%
            set_topo("A -> B") %>%
            set_init(tibble::tibble(comp = c("A", "B"),
                                    size = c(100, 100),
                                    prop = c(0.5, 0)),
                     comp = "comp", size = "size", prop = "prop")
        expect_setequal(params(m, simplify = TRUE),
                        c("eta", "lambda_A", "lambda_B", "upsilon_A_to_B",
                          "zeta"))
        # Run model with set parameters
        m %<>% set_params(c("upsilon_A_to_B" = 0.1,
                            "eta" = 0.05, "zeta" = 0.05,
                            "lambda_A" = 0, "lambda_B" = 0)) %>%
            project(end = 50)
        traj <- m$trajectory[[1]]
        last <- length(traj$timepoints[[1]])
        expect_equal(traj$sizes[[1]][1, ], c("A" = 100, "B" = 100))
        x <- traj$sizes[[1]][last, "A"]
        expect_true(0 < x & x < 1)
        x <- traj$sizes[[1]][last, "B"]
        expect_true(199 < x & x < 200)
        expect_equal(traj$proportions[[1]][1, ], c("A" = 0.5, "B" = 0))
        x <- traj$proportions[[1]][last, "A"]
        expect_true(x == 0.5)
        x <- traj$proportions[[1]][last, "B"]
        expect_true(0.249 < x & x < 0.251)
        expect_error(plot(m), NA)
        # Run model fit
        obs <- m %>% sample_from(at = seq(1, 50, length.out = 5))
        m %<>% set_obs(obs, comp = "comp", size = "size", prop = "prop",
                       time = "time") %>%
            set_priors(hcauchy_p(0.1), "", quiet = TRUE)
        plot(m)
        if (!covr::in_covr() | FORCE_STAN_COVR) {
            expect_error(
                capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                               method = solver)})),
                NA)
            expect_error(plot(f), NA)
            # Posterior predictions
            expect_error({p <- predict(m, f, draws = DRAWS, cores = n_cores)}, NA)
            expect_error(plot(p), NA)
        }
    })

    ### * Two compartments, one split

    test_that("Model with two compartments, one split works", {
        m <- new_networkModel() %>%
            set_topo("A -> B") %>%
            set_split("B") %>%
            set_init(tibble::tibble(comp = c("A", "B"),
                                    size = c(100, 100),
                                    prop = c(0.5, 0)),
                     comp = "comp", size = "size", prop = "prop")
        expect_setequal(params(m, simplify = TRUE),
                        c("eta", "lambda_A", "lambda_B", "portion.act_B",
                          "upsilon_A_to_B", "zeta"))
        # Run model with set parameters
        m %<>% set_params(c("upsilon_A_to_B" = 0.1,
                            "eta" = 0.01, "zeta" = 0.01,
                            "lambda_A" = 0, "lambda_B" = 0,
                            "portion.act_B" = 0.25)) %>%
            project(end = 50)
        traj <- m$trajectory[[1]]
        last <- length(traj$timepoints[[1]])
        expect_equal(traj$sizes[[1]][1, ], c("A" = 100, "B" = 100))
        x <- traj$sizes[[1]][last, "A"]
        expect_true(0 < x & x < 1)
        x <- traj$sizes[[1]][last, "B"]
        expect_true(199 < x & x < 200)
        expect_equal(traj$proportions[[1]][1, ], c("A" = 0.5, "B" = 0))
        x <- traj$proportions[[1]][last, "A"]
        expect_true(x == 0.5)
        x <- traj$proportions[[1]][last, "B"]
        expect_true(0.249 < x & x < 0.251)
        expect_error(plot(m), NA)
        # Run model fit
        obs <- m %>% sample_from(at = seq(1, 50, length.out = 5))
        m %<>% set_obs(obs, comp = "comp", size = "size", prop = "prop",
                       time = "time") %>%
            set_priors(hcauchy_p(0.1), "", quiet = TRUE)
        plot(m)
        if (!covr::in_covr() | FORCE_STAN_COVR) {
            expect_error(
                capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                               method = solver)})),
                NA)
            expect_error(plot(f), NA)
            # Posterior predictions
            expect_error({p <- predict(m, f, draws = DRAWS, cores = n_cores)}, NA)
            expect_error(plot(p), NA)
        }
    })

    ### * Three compartments

    test_that("Model with three compartments work", {
        m <- new_networkModel() %>%
            set_topo("A -> B -> C") %>%
            set_init(tibble::tibble(comp = c("A", "B", "C"),
                                    size = c(100, 100, 100),
                                    prop = c(0.5, 0, 0)),
                     comp = "comp", size = "size", prop = "prop")
        expect_setequal(params(m, simplify = TRUE),
                        c("eta", "lambda_A", "lambda_B", "lambda_C",
                          "upsilon_A_to_B", "upsilon_B_to_C", "zeta"))
        # Run model with set parameters
        m %<>% set_params(c("upsilon_A_to_B" = 0.1,
                            "upsilon_B_to_C" = 0.1,
                            "eta" = 0.01, "zeta" = 0.01,
                            "lambda_A" = 0, "lambda_B" = 0,
                            "lambda_C" = 0)) %>%
            project(end = 50)
        traj <- m$trajectory[[1]]
        last <- length(traj$timepoints[[1]])
        expect_equal(traj$sizes[[1]][1, ], c("A" = 100, "B" = 100, "C" = 100))
        x <- traj$sizes[[1]][last, "A"]
        expect_true(0 < x & x < 1)
        x <- traj$sizes[[1]][last, "B"]
        expect_true(3 < x & x < 4)
        x <- traj$sizes[[1]][last, "C"]
        expect_true(295 < x & x < 296)
        expect_equal(traj$proportions[[1]][1, ], c("A" = 0.5, "B" = 0, "C" = 0))
        x <- traj$proportions[[1]][last, "A"]
        expect_true(x == 0.5)
        x <- traj$proportions[[1]][last, "B"]
        expect_true(0.4 < x & x < 0.42)
        x <- traj$proportions[[1]][last, "C"]
        expect_true(0.16 < x & x < 0.17)
        expect_error(plot(m), NA)
        # Run model fit
        obs <- m %>% sample_from(at = seq(1, 50, length.out = 5))
        m %<>% set_obs(obs, comp = "comp", size = "size", prop = "prop",
                       time = "time") %>%
            set_priors(hcauchy_p(0.1), "", quiet = TRUE)
        plot(m)
        if (!covr::in_covr() | FORCE_STAN_COVR) {
            expect_error(
                capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                               method = solver)})),
                NA)
            expect_error(plot(f), NA)
            # Posterior predictions
            expect_error({p <- predict(m, f, draws = DRAWS, cores = n_cores)}, NA)
            expect_error(plot(p), NA)
        }
    })

    ### * Three compartments, one split

    test_that("Model with three compartments, one split works", {
        m <- new_networkModel() %>%
            set_topo("A -> B -> C") %>%
            set_split("B") %>%
            set_init(tibble::tibble(comp = c("A", "B", "C"),
                                    size = c(100, 100, 100),
                                    prop = c(0.5, 0, 0)),
                     comp = "comp", size = "size", prop = "prop")
        expect_setequal(params(m, simplify = TRUE),
                        c("eta", "lambda_A", "lambda_B", "lambda_C",
                          "portion.act_B", "upsilon_A_to_B", "upsilon_B_to_C",
                          "zeta"))
        # Run model with set parameters
        m %<>% set_params(c("upsilon_A_to_B" = 0.1,
                            "upsilon_B_to_C" = 0.1,
                            "eta" = 0.01, "zeta" = 0.01,
                            "lambda_A" = 0, "lambda_B" = 0,
                            "lambda_C" = 0,
                            "portion.act_B" = 0.25)) %>%
            project(end = 50)
        traj <- m$trajectory[[1]]
        last <- length(traj$timepoints[[1]])
        expect_equal(traj$sizes[[1]][1, ], c("A" = 100, "B" = 100, "C" = 100))
        x <- traj$sizes[[1]][last, "A"]
        expect_true(0 < x & x < 1)
        x <- traj$sizes[[1]][last, "B"]
        expect_true(78 < x & x < 79)
        x <- traj$sizes[[1]][last, "C"]
        expect_true(220 < x & x < 221)
        expect_equal(traj$proportions[[1]][1, ], c("A" = 0.5, "B" = 0, "C" = 0))
        x <- traj$proportions[[1]][last, "A"]
        expect_true(x == 0.5)
        x <- traj$proportions[[1]][last, "B"]
        expect_true(0.02 < x & x < 0.03)
        x <- traj$proportions[[1]][last, "C"]
        expect_true(0.21 < x & x < 0.22)
        expect_error(plot(m), NA)
        # Run model fit
        obs <- m %>% sample_from(at = seq(1, 50, length.out = 5))
        m %<>% set_obs(obs, comp = "comp", size = "size", prop = "prop",
                       time = "time") %>%
            set_priors(hcauchy_p(0.1), "", quiet = TRUE)
        plot(m)
        if (!covr::in_covr() | FORCE_STAN_COVR) {
            expect_error(
                capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                               method = solver)})),
                NA)
            expect_error(plot(f), NA)
            # Posterior predictions
            expect_error({p <- predict(m, f, draws = DRAWS, cores = n_cores)}, NA)
            expect_error(plot(p), NA)
        }
    })

}

### * Compare Euler and matrix exponential outputs

# During 102 test runs without setting a random seed, overlap fractions were:
# -  1 in (0.5, 0.6)
# -  7 in (0.6, 0.7)
# - 45 in (0.7, 0.8)
# - 49 in (0.8, 0.9)

test_that("Models using Euler and matrix exponential solvers give similar posteriors", {
    m <- aquarium_mod
    if (!covr::in_covr() | FORCE_STAN_COVR) {
        expect_error(
            capture_warnings(capture_output({f_euler <- run_mcmc(m, iter = 250,
                                                                 euler_control = list(grid_size = 128),
                                                                 method = "euler",
                                                                 seed = 4)})),
            NA)
        expect_error(
            capture_warnings(capture_output({f_matrix_exp <- run_mcmc(m, iter = 1000,
                                                                      method = "matrix_exp",
                                                                      seed = 4)})),
            NA)
    }
    # Check that the 95%CI overlaps generously between the two approaches
    overlaps <- ci_overlap(f_euler, f_matrix_exp)
    expect_true(min(overlaps) > 0.10)
    expect_true(min(overlaps) > 0.20)
    expect_true(min(overlaps) > 0.30)
    expect_true(min(overlaps) > 0.40)
    expect_true(min(overlaps) > 0.50)
    ## expect_true(min(overlaps) > 0.60)
    ## expect_true(min(overlaps) > 0.70)
    ## expect_true(min(overlaps) > 0.80)
    ## expect_true(min(overlaps) > 0.90)
})

### * Test priors

test_that("Different priors do not crash the model", {
    m <- aquarium_mod
    m <- m %>%
        set_priors(exponential_p(2), "eta", use_regexp = FALSE) %>%
        set_priors(gamma_p(9, 2), "zeta")
    expect_error(
        capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                       method = "matrix_exp") })),
        NA)
    expect_error(
        capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                       method = "euler") })),
        NA)
})

### * Test pulses

test_that("Multiple pulses work", {
    exp <- tibble::tribble(
                       ~time.day,    ~species, ~biomass, ~prop15N,    ~transect,
                       0,       "NH4",    0.313,   0.0038, "transect_1",
                       4,       "NH4",   0.2746,       NA, "transect_1",
                       8,       "NH4",   0.3629,   0.0295, "transect_1",
                       12,       "NH4",       NA,   0.0032, "transect_1",
                       16,       "NH4",       NA,   0.0036, "transect_1",
                       20,       "NH4",   0.3414,   0.0038, "transect_1",
                       0, "epilithon",  89.2501,   0.0022, "transect_1",
                       8, "epilithon", 123.1212,    0.024, "transect_1",
                       12, "epilithon",       NA,     0.02, "transect_1",
                       16, "epilithon",  90.5919,   0.0107, "transect_1",
                       20, "epilithon",  80.3261,   0.0062, "transect_1",
                       0,       "NH4",   0.3525,   0.0035, "transect_2",
                       4,       "NH4",   0.2958,   0.0362, "transect_2",
                       8,       "NH4",       NA,     0.03, "transect_2",
                       12,       "NH4",   0.3392,   0.0044, "transect_2",
                       16,       "NH4",    0.212,   0.0026, "transect_2",
                       20,       "NH4",   0.3818,   0.0046, "transect_2",
                       0, "epilithon",  127.873,   0.0029, "transect_2",
                       4, "epilithon",       NA,   0.0096, "transect_2",
                       8, "epilithon", 123.3216,       NA, "transect_2",
                       12, "epilithon",  89.8053,   0.0144, "transect_2",
                       16, "epilithon",  74.9105,   0.0098, "transect_2",
                       20, "epilithon",  88.0108,   0.0067, "transect_2"
                   )
    m <- new_networkModel() %>% set_topo("NH4 -> epilithon")
    inits <- exp %>% filter(time.day == 0)
    m <- m %>% set_init(inits, comp = "species", size = "biomass", prop = "prop15N",
                        group_by = "transect")
    obs <- exp %>% filter(time.day > 0)
    m <- m %>% set_obs(obs, time = "time.day")
    m <- m %>% set_steady(comps = "NH4")
    m <- m %>%
        add_pulse_event(time = 2, comp = "NH4", unmarked = 0, marked = 0.008)
    m <- m %>%
        add_pulse_event(time = 10, comp = "NH4", unmarked = 0, marked = -0.008)
    m <- m %>%
        set_priors(hcauchy_p(0.1), "", quiet = TRUE)
    expect_error(
        capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                       method = "matrix_exp") })),
        NA)
    expect_error(
        capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                       method = "euler") })),
        NA)
})

### * Covariates for eta and zeta

test_that("Model runs when eta and zeta have covariates", {
    exp <- tibble::tribble(
  ~time.day,  ~species, ~biomass, ~prop15N, ~aquariumID,
          0,   "algae",     1.03,        0,      "aq01",
          2,   "algae",       NA,     0.08,      "aq01",
          5,   "algae",     0.81,     0.08,      "aq01",
          8,   "algae",     0.82,       NA,      "aq01",
         10,   "algae",       NA,     0.11,      "aq01",
          0, "daphnia",     2.07,        0,      "aq01",
          2, "daphnia",     1.79,       NA,      "aq01",
          5, "daphnia",     2.24,     0.02,      "aq01",
          8, "daphnia",       NA,     0.02,      "aq01",
         10, "daphnia",     1.86,     0.04,      "aq01",
          0,     "NH4",     0.23,        1,      "aq01",
          2,     "NH4",     0.25,       NA,      "aq01",
          5,     "NH4",       NA,     0.14,      "aq01",
          8,     "NH4",      0.4,      0.1,      "aq01",
         10,     "NH4",     0.41,     0.07,      "aq01",
          0,   "algae",     1.48,        0,      "aq02",
          2,   "algae",     0.94,     0.09,      "aq02",
          5,   "algae",       NA,     0.21,      "aq02",
          8,   "algae",       NA,     0.19,      "aq02",
         10,   "algae",     0.67,       NA,      "aq02",
          0, "daphnia",     1.49,        0,      "aq02",
          2, "daphnia",      1.4,     0.01,      "aq02",
          5, "daphnia",     1.44,       NA,      "aq02",
          8, "daphnia",       NA,     0.08,      "aq02",
         10, "daphnia",     1.82,     0.08,      "aq02",
          0,     "NH4",     0.51,     0.87,      "aq02",
          2,     "NH4",     0.47,     0.48,      "aq02",
          5,     "NH4",     0.35,     0.37,      "aq02",
          8,     "NH4",       NA,     0.27,      "aq02",
         10,     "NH4",     0.39,       NA,      "aq02"
  )
    m <- new_networkModel() %>% set_topo("NH4 -> algae -> daphnia -> NH4")
    inits <- exp %>% filter(time.day == 0)
    m <- m %>% set_init(inits, comp = "species", size = "biomass", prop = "prop15N",
                        group_by = "aquariumID")
    obs <- exp %>% filter(time.day > 0)
    m <- m %>% set_obs(obs, time = "time.day")
    m <- add_covariates(m, eta + zeta ~ aquariumID)
    m <- set_priors(m, normal_p(0, 2), "lambda|upsilon")
    m <- set_priors(m, normal_p(0, 2), "eta")
    expect_error(
        capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                       method = "matrix_exp") })),
        NA)
    expect_error({p <- predict(m, f, draws = DRAWS, cores = n_cores)}, NA)
    expect_error(plot(p), NA)
    expect_error(
        capture_warnings(capture_output({f <- run_mcmc(m, iter = ITERS,
                                                       method = "euler") })),
        NA)
    expect_error({p <- predict(m, f, draws = DRAWS, cores = n_cores)}, NA)
    expect_error(plot(p), NA)
})

### * Test thinning

test_that("Thinning does not crash the MCMC run", {
  m <- aquarium_mod
  expect_error(capture_warnings(capture_output({
    r <- isotracer::run_mcmc(m, seed = 40, iter = 100, thin = 1, chains = 2,
                             cores = n_cores)
  })), NA)
  expect_equal(nrow(as.matrix(r)), 100)
  expect_error(capture_warnings(capture_output({
    r <- isotracer::run_mcmc(m, seed = 40, iter = 100, thin = 7, chains = 2,
                  cores = n_cores)
  })), NA)
  expect_equal(nrow(as.matrix(r)), 16)
})

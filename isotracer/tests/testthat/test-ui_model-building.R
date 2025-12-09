### * Setup

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

### * new_networkModel()

test_that("new_networkModel() produces the expected output", {
    x <- new_networkModel()
    # Is an empty model (no rows)
    expect_equal(nrow(x), 0)
    # Contains the appropriate columns
    expect_setequal(names(x), c("topology", "initial", "observations"))
    # Has the correct class
    expect_is(x, "tbl_df")
    expect_is(x, "networkModel")    
})

test_that("new_networkModel() fails if an argument other than \"quiet\" is provided", {
    expect_error(isotracer::new_networkModel(x = 1, y = 2))
    expect_error(isotracer::new_networkModel(quiet = TRUE), NA)
})

### * set_topo()

### ** set_topo() works

test_that("set_topo() works", {
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia")
    # Has only one row
    expect_equal(nrow(x), 1)
    # The topology is correct
    topo <- x[["topology"]][[1]]
    expect_is(topo, "matrix")
    expect_is(topo,  "topology")
    expect_setequal(colnames(topo), c("NH4", "algae", "daphnia"))
    expect_identical(colnames(topo), rownames(topo))
    # Parameters were automatically added
    expect_false(is.null(x$parameters[[1]]))
    expect_is(x$parameters[[1]], "tbl_df")
    expect_setequal(colnames(x$parameters[[1]]), c("in_replicate", "in_model"))
})

### ** Different topologies work with set_topo()

test_that("Different topologies work with set_topo()", {
    # Links defined by several strings, included one with "<-"
    x <- new_networkModel() %>%
        set_topo(links = c("NH4, NO3 -> epi -> pseph, tricor",
                           "NH4 -> FBOM, CBOM", "CBOM <- NO3"))
    topo <- x[["topology"]][[1]]
    expect_is(topo, "matrix")
    expect_is(topo,  "topology")
    expect_setequal(colnames(topo),
                    c("NH4", "NO3", "epi", "pseph", "tricor", "CBOM", "FBOM"))
    expect_identical(colnames(topo), rownames(topo))
    expect_equal(topo["pseph", "epi"], 1)
    expect_equal(topo["epi", "pseph"], 0)
    expect_equal(topo["CBOM", "NO3"], 1)
    expect_equal(topo["NO3", "CBOM"], 0)
    # Links defined by multiple strings provided as distrinct arguments
    x <- new_networkModel() %>%
        set_topo("NH4, NO3 -> epi -> pseph, tricor",
                 "NH4 -> FBOM, CBOM", "CBOM <- NO3")
    topo <- x[["topology"]][[1]]
    expect_is(topo, "matrix")
    expect_is(topo,  "topology")
    expect_setequal(colnames(topo),
                    c("NH4", "NO3", "epi", "pseph", "tricor", "CBOM", "FBOM"))
    expect_identical(colnames(topo), rownames(topo))
    expect_equal(topo["pseph", "epi"], 1)
    expect_equal(topo["epi", "pseph"], 0)
    expect_equal(topo["CBOM", "NO3"], 1)
    expect_equal(topo["NO3", "CBOM"], 0)
    # Links defined in a data frame
    links <- data.frame(source = c("NH4", "NO3", "epi"),
                        consumer = c("epi", "epi", "petro"))
    x <- new_networkModel() %>%
        set_topo(links = links, from = "source", to = "consumer")
    topo <- x[["topology"]][[1]]
    expect_is(topo, "matrix")
    expect_is(topo,  "topology")
    expect_setequal(colnames(topo),
                    c("NH4", "NO3", "epi", "petro"))
    expect_identical(colnames(topo), rownames(topo))
    expect_equal(topo["epi", "NH4"], 1)
    expect_equal(topo["NH4", "epi"], 0)
    expect_equal(topo["petro", "epi"], 1)
    expect_equal(topo["epi", "petro"], 0)
    # Error when mixing strings and df or providing multiple df
    expect_error({x <- new_networkModel() %>%
                      set_topo("NH4 -> epi", links, from = "source", to = "to")})
})

### ** set_topo() warns the user when the network model is not empty

test_that("set_topo() warns the user when the network model is not empty", {
    x <- new_networkModel() %>%
        set_topo(links = c("NH4, NO3 -> epi -> pseph, tricor",
                           "NH4 -> FBOM, CBOM", "CBOM <- NO3"))
    expect_message(set_topo(x, "a -> b"),
                   "`set_topo\\(\\)` was called on a non-empty networkModel object[.] As a result, the parameter mapping and the priors of the model were reset[.]")
})

### * set_init()

### ** set_init() works

test_that("set_init() works", {
    inits <- tibble::tibble(comps = c("NH4", "algae", "daphnia"),
                    sizes = c(0.2, 1, 2),
                    props = c(0.8, 0.004, 0.004))
    expected <- tibble::tibble(compartment = c("NH4", "algae", "daphnia"),
                       size = c(0.2, 1, 2),
                       proportion = c(0.8, 0.004, 0.004))
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props")
    # x has the right shape
    expect_equal(nrow(x), 1)
    expect_false(is.null(x[["initial"]][[1]]))
    # The initial component is correct
    obs <- x$initial[[1]]
    expect_is(obs, "tbl_df")
    expect_identical(obs, expected)
})

### ** set_init() can handle grouping

test_that("set_init() can handle grouping", {
    # nm not grouped, init grouped
    inits <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 2),
                    sizes = c(0.2, 1, 2, 1, 0.5, 6),
                    props = c(0.8, 0.004, 0.004, 0.2, 0.04, 0.5),
                    aquarium = rep(c("aq01", "aq02"),each = 3))
    exp1 <- inits[1:3, c("comps", "sizes", "props")]
    exp2 <- inits[4:6, c("comps", "sizes", "props")]
    colnames(exp1) <- c("compartment", "size", "proportion")
    colnames(exp2) <- colnames(exp1)
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props",
                 group_by = "aquarium")
    # x has the right shape
    expect_equal(nrow(x), 2)
    expect_null(x$observations[[1]])
    expect_null(x$observations[[2]])
    expect_identical(x$topology[[1]], x$topology[[2]])
    expect_identical(x$parameters[[1]], x$parameters[[2]])
    expect_identical(x$initial[[1]], exp1)
    expect_identical(x$initial[[2]], exp2)
    expect_identical(x$group, list(c(aquarium = "aq01"), c(aquarium = "aq02")))
})

### * set_obs()

### ** set_obs() works

test_that("set_obs() works", {
    inits <- tibble::tibble(comps = c("NH4", "algae", "daphnia"),
                    sizes = c(0.2, 1, 2),
                    props = c(0.8, 0.004, 0.004))
    obs <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 3),
                    sizes = runif(9, 10, 20),
                    props = runif(9, 0, 1),
                    timepoint = runif(9, 0, 10))
    exp <- obs
    colnames(exp) <- c("compartment", "size", "proportion", "time")
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props") %>%
        set_obs(obs, comp = "comps", size = "sizes", prop = "props",
                time = "timepoint")
    # x has the right shape
    expect_equal(nrow(x), 1)
    expect_false(is.null(x[["initial"]][[1]]))
    expect_false(is.null(x[["observations"]][[1]]))
    # The observation component is correct
    obs <- x$observations[[1]]
    expect_is(obs, "tbl_df")
    expect_identical(obs, exp)
})

### ** set_obs() can handle grouping

test_that("set_obs() can handle grouping (1)", {
    # nm not grouped, init not grouped, obs grouped
    inits <- tibble::tibble(comps = c("NH4", "algae", "daphnia"),
                    sizes = c(0.2, 1, 2),
                    props = c(0.8, 0.004, 0.004))
    obs <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 4),
                    sizes = runif(12, 5, 10),
                    props = runif(12, 0, 1),
                    timepoint = runif(12, 5, 20),
                    aquarium = rep(c("aq01", "aq02"),each = 6))
    exp1 <- obs[1:6, c("comps", "sizes", "props", "timepoint")]
    exp2 <- obs[7:12, c("comps", "sizes", "props", "timepoint")]
    colnames(exp1) <- c("compartment", "size", "proportion", "time")
    colnames(exp2) <- colnames(exp1)
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props") %>%
        set_obs(obs, comp = "comps", size = "sizes", prop = "props",
                time = "timepoint", group_by = "aquarium")
    # x has the right shape
    expect_equal(nrow(x), 2)
    expect_identical(x$topology[[1]], x$topology[[2]])
    expect_identical(x$parameters[[1]], x$parameters[[2]])
    expect_identical(x$initial[[1]], x$initial[[2]])
    expect_identical(x$observations[[1]], exp1)
    expect_identical(x$observations[[2]], exp2)
    expect_identical(x$group, list(c(aquarium = "aq01"), c(aquarium = "aq02")))
})

test_that("set_obs() can handle grouping (2)", {
    # nm not grouped, init grouped, obs grouped
    inits <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 2),
                    sizes = runif(6, 2, 8),
                    props = runif(6, 0, 1),
                    aquarium = rep(c("aq01", "aq02"),each = 3))
    expinit1 <- inits[1:3, c("comps", "sizes", "props")]
    expinit2 <- inits[4:6, c("comps", "sizes", "props")]
    colnames(expinit1) <- c("compartment", "size", "proportion")
    colnames(expinit2) <- colnames(expinit1)
    obs <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 4),
                    sizes = runif(12, 5, 10),
                    props = runif(12, 0, 1),
                    timepoint = runif(12, 5, 20),
                    aquarium = rep(c("aq01", "aq02"),each = 6))
    expobs1 <- obs[1:6, c("comps", "sizes", "props", "timepoint")]
    expobs2 <- obs[7:12, c("comps", "sizes", "props", "timepoint")]
    colnames(expobs1) <- c("compartment", "size", "proportion", "time")
    colnames(expobs2) <- colnames(expobs1)
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props",
                 group_by = "aquarium") %>%
        set_obs(obs, comp = "comps", size = "sizes", prop = "props",
                time = "timepoint", group_by = "aquarium")
    # x has the right shape
    expect_equal(nrow(x), 2)
    expect_identical(x$topology[[1]], x$topology[[2]])
    expect_identical(x$parameters[[1]], x$parameters[[2]])
    expect_identical(x$observations[[1]], expobs1)
    expect_identical(x$observations[[2]], expobs2)
    expect_identical(x$initial[[1]], expinit1)
    expect_identical(x$initial[[2]], expinit2)
    expect_identical(x$group, list(c(aquarium = "aq01"), c(aquarium = "aq02")))
    # Same again, but with a row swap before adding observations
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props",
                 group_by = "aquarium")
    x <- x[2:1, ] %>% set_obs(obs, comp = "comps", size = "sizes", prop = "props",
                              time = "timepoint", group_by = "aquarium")
    if (x$group[[1]] == "aq02") {
        x <- x[2:1, ]
    }
    # x has the right shape
    expect_equal(nrow(x), 2)
    expect_identical(x$topology[[1]], x$topology[[2]])
    expect_identical(x$parameters[[1]], x$parameters[[2]])
    expect_identical(x$observations[[1]], expobs1)
    expect_identical(x$observations[[2]], expobs2)
    expect_identical(x$initial[[1]], expinit1)
    expect_identical(x$initial[[2]], expinit2)
    expect_identical(x$group, list(c(aquarium = "aq01"), c(aquarium = "aq02")))
})

## ### * set_steady()

## test_that("set_steady() works", {
##     x <- new_NetworkModel() %>%
##         set_topo("NH4 -> algae -> daphnia") %>%
##         set_steady("NH4")
##     topo <- x[["topology"]][[1]]
##     expect_equal(ncol(topo), 3)
##     expect_false(is.null(attr(topo, "steadyState")))
##     expect_equal(attr(topo, "steadyState"), "NH4")
## })

## ### * set_split()

## test_that("set_split() works", {
##     x <- new_NetworkModel() %>%
##         set_topo("NH4 -> algae -> daphnia") %>%
##         set_split("algae")
##     topo <- x[["topology"]][[1]]
##     expect_equal(ncol(topo), 3)
##     expect_false(is.null(attr(topo, "split")))
##     expect_equal(attr(topo, "split"), "algae")
## })

### * set_params()

test_that("set_params() works", {
    # nm not grouped, init grouped, obs grouped
    inits <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 2),
                    sizes = runif(6, 2, 8),
                    props = runif(6, 0, 1),
                    aquarium = rep(c("aq01", "aq02"),each = 3))
    obs <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 4),
                    sizes = runif(12, 5, 10),
                    props = runif(12, 0, 1),
                    timepoint = runif(12, 5, 20),
                    aquarium = rep(c("aq01", "aq02"),each = 6))
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props",
                 group_by = "aquarium") %>%
        set_obs(obs, comp = "comps", size = "sizes", prop = "props",
                time = "timepoint", group_by = "aquarium")
    x <- x %>% set_params(c("eta" = 2, "lambda_daphnia" = 0.5,
                            "upsilon_NH4_to_algae" = 3))
    # Check parameters column
    expect_identical(x$parameters[[1]], x$parameters[[2]])
    p <- x$parameters[[1]]
    expect_identical(p$in_replicate, p$in_model)
    p <- na.omit(p)
    expect_equal(nrow(p), 3)
    p <- sort(tibble::deframe(p[, 2:3]))
    expect_identical(p, c("lambda_daphnia" = 0.5, "eta" = 2,
                          "upsilon_NH4_to_algae" = 3))
})

### * set_prior()

test_that("set_prior() works", {
    # nm not grouped, init grouped, obs grouped
    inits <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 2),
                    sizes = runif(6, 2, 8),
                    props = runif(6, 0, 1),
                    aquarium = rep(c("aq01", "aq02"),each = 3))
    obs <- tibble::tibble(comps = rep(c("NH4", "algae", "daphnia"), 4),
                    sizes = runif(12, 5, 10),
                    props = runif(12, 0, 1),
                    timepoint = runif(12, 5, 20),
                    aquarium = rep(c("aq01", "aq02"),each = 6))
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "comps", size = "sizes", prop = "props",
                 group_by = "aquarium") %>%
        set_obs(obs, comp = "comps", size = "sizes", prop = "props",
                time = "timepoint", group_by = "aquarium")
    x <- x %>% set_params(c("eta" = 2, "lambda_daphnia" = 0.5,
                            "upsilon_NH4_to_algae" = 3))
    # Get priors
    p <- priors(x)
    expect_equal(sort(p[["in_model"]]), sort(params(x, simplify = TRUE)))
    expect_is(p[["prior"]], "list")
    ## Check that default is no priors
    expect_true(all(sapply(p$prior, is.null)))
    ## Set priors
    x <- x %>%
        set_priors(normal_p(0, 5), "", quiet = TRUE)
    p <- priors(x)
    expect_equal(sort(p[["in_model"]]), sort(params(x, simplify = TRUE)))
    expect_is(p[["prior"]], "list")
    expect_true(all(sapply(p[["prior"]], class) == "prior"))
    z <- p$prior[[which(p$in_model == "eta")]]
    expect_true(z$type == "trun_normal")
    # Change priors
    capture_output({x <- set_prior(x, uniform_p(0, 1), "zeta")})
    p <- priors(x)
    z <- p$prior[[which(p$in_model == "zeta")]]
    expect_true(z$type == "uniform")
    z <- p$prior[[which(p$in_model == "eta")]]
    expect_true(z$type == "trun_normal")
    # Change priors
    capture_output({x <- set_prior(x, constant_p(1), "eta", use_regexp = TRUE)})
    p <- priors(x)
    z <- p$prior[[which(p$in_model == "zeta")]]
    expect_true(z$type == "constant")
    z <- p$prior[[which(p$in_model == "eta")]]
    expect_true(z$type == "constant")
})

### * add_covariates()

test_that("add_covariates() works", {
    exp <- tibble::tribble(
  ~time.day,  ~species, ~biomass, ~prop15N, ~treatment,
          0,     "NH4",    0.205,    0.739,    "light",
          2,     "NH4",    0.232,    0.403,    "light",
          4,     "NH4",       NA,    0.199,    "light",
          6,     "NH4",       NA,    0.136,    "light",
          8,     "NH4",    0.306,       NA,    "light",
         10,     "NH4",    0.323,   0.0506,    "light",
          0,   "algae",    0.869,  0.00305,    "light",
          2,   "algae",       NA,   0.0875,    "light",
          4,   "algae",     0.83,    0.131,    "light",
          6,   "algae",    0.706,       NA,    "light",
         10,   "algae",    0.666,   0.0991,    "light",
          0, "daphnia",     2.13,  0.00415,    "light",
          2, "daphnia",     1.99,       NA,    "light",
          4, "daphnia",     1.97,   0.0122,    "light",
          6, "daphnia",       NA,   0.0284,    "light",
          8, "daphnia",       NA,   0.0439,    "light",
         10, "daphnia",      1.9,   0.0368,    "light",
          0,     "NH4",    0.474,     0.98,     "dark",
          2,     "NH4",    0.455,     0.67,     "dark",
          4,     "NH4",    0.595,    0.405,     "dark",
          6,     "NH4",       NA,    0.422,     "dark",
         10,     "NH4",    0.682,    0.252,     "dark",
          0,   "algae",     1.06,  0.00455,     "dark",
          2,   "algae",        1,   0.0637,     "dark",
          4,   "algae",    0.862,   0.0964,     "dark",
          6,   "algae",       NA,    0.222,     "dark",
          8,   "algae",       NA,    0.171,     "dark",
         10,   "algae",    0.705,    0.182,     "dark",
          0, "daphnia",     1.19,  0.00315,     "dark",
          4, "daphnia",     1.73,   0.0204,     "dark",
          6, "daphnia",     1.75,       NA,     "dark",
          8, "daphnia",     1.54,   0.0598,     "dark",
         10, "daphnia",     1.65,   0.0824,     "dark"
  )
    inits <- exp %>% dplyr::filter(time.day == 0)
    obs <- exp %>% dplyr::filter(time.day > 0)
    m <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "species", size = "biomass", prop = "prop15N",
                 group_by = "treatment") %>%
        set_obs(obs, comp = "species", size = "biomass", prop = "prop15N",
                time = "time.day", group_by = "treatment")
    expect_error(m <- m %>% add_covariates(upsilon_NH4_to_algae ~ treatment), NA)
    p <- priors(m)
    expect_is(p, c("tbl_df", "tbl", "data.frame"))
    expect_equal(dim(p), c(9, 2))
    expect_true(all(sapply(p$prior, is.null)))
    expect_setequal(p$in_model, c("eta", "lambda_algae", "lambda_daphnia",
                                  "lambda_NH4", "upsilon_algae_to_daphnia",
                                  "upsilon_daphnia_to_NH4",
                                  "upsilon_NH4_to_algae|dark",
                                  "upsilon_NH4_to_algae|light", "zeta"))
})

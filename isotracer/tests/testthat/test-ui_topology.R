### * Setup

n_cores <- min(2, parallel::detectCores())
n_chains <- max(n_cores, 2)

run_mcmc <- function(...) {
    isotracer:::run_mcmc(..., cores = n_cores, chains = n_chains)
}

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

### * Ops.topology()

test_that("Ops.topology() works", {
    expect_error({topo1 <- make_topology(links = "NH4, NO3 -> epi -> pseph, tricor")},
                 NA)
    # A larger foodweb
    links <- c("NH4, NO3 -> seston, epi, CBOM, FBOM",
               "seston -> lepto", "epi -> petro, pseph",
               "CBOM, FBOM -> eudan", "CBOM -> phyllo",
               "FBOM -> tricor -> arg, euthy")
    expect_error({topo2 <- make_topology(links = links, split = "epi")},
                 NA)
    # Tests
    expect_false(topo1 == topo2)
    expect_true(topo1 != topo2)
    # Using a data frame to specify the links
    links <- data.frame(source = c("NH4", "NO3", "epi"),
                        consumer = c("epi", "epi", "petro"))
    expect_error({topo3 <- make_topology(links, from = "source", to = "consumer")},
                 NA)
    expect_false(topo1 == topo3)
    expect_true(topo3 == topo3)
    expect_length(unique(list(topo1, topo2, topo1, topo3, topo1)), 3)
    topo4 <- topo1
    attr(topo4, "split") <- "epi"
    expect_false(topo4 == topo1)
    expect_length(unique(list(topo1, topo2, topo1, topo3, topo1, topo4)), 4)
})

### * Setup

n_cores <- min(2, parallel::detectCores())
n_chains <- max(n_cores, 2)

run_mcmc <- function(...) {
  isotracer:::run_mcmc(..., cores = n_cores, chains = n_chains)
}

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

### * sankey()

# Basically only checking that the function does not crash.

test_that("left2right layout works", {
    t <- topo(trini_mod)
    nodes <- nodes_from_topo(t)
    nodes$label <- as.list(nodes$label)
    nodes$size <- runif(nrow(nodes), 1, 2)
    flows <- flows_from_topo(t)
    flows$width <- runif(nrow(flows), 0.2, 2)
    expect_error({z <- sankey(t, nodes = nodes, flows = flows, layout = "left2right",
                              debug = FALSE, node_f = 1, edge_f = 0.9, edge_n = 32,
                              cex_lab = 1.5)},
                 NA)
    # Debug
    expect_error({z <- sankey(t, nodes = nodes, flows = flows, layout = "left2right",
                              debug = TRUE, node_f = 1, edge_f = 0.9, edge_n = 32,
                              cex_lab = 1.5)},
                 NA)
})

test_that("stress layout works", {
    y <- new_networkModel() %>%
        set_topo(c("subs -> NH3 -> subs",
                   "NH3 -> Q, E", "E -> Q -> E",
                   "E -> D, M")) %>%
        set_steady("subs") %>%
        set_prop_family("normal_sd", quiet = TRUE)
    y <- topo(y)
    nodes <- nodes_from_topo(y)
    nodes$size <- runif(nrow(nodes), 1, 5)
    flows <- flows_from_topo(y)
    flows$width <- runif(nrow(flows), 0.2, 5)
    expect_error(sankey(y, flows = flows, debug = TRUE, edge_n = 32, edge_f = 0.2, new = FALSE),
                 NA)
    expect_error(sankey(y, nodes = nodes, flows = flows, debug = FALSE, edge_n = 32,
                        edge_f = 0.4, node_s = "prop"),
                 NA)
    expect_error(sankey(y, nodes = nodes, flows = flows, debug = FALSE, edge_n = 32,
                        edge_f = 0.4, node_s = "constant"),
                 NA)
})

test_that("stress layout works", {
    r <- new_networkModel() %>%
        set_topo("infusion -> plasma -> body -> plasma") %>%
        set_steady(c("infusion", "body"))
    r <- topo(r)
    expect_error(sankey(r, debug = TRUE, edge_f = 0.2), NA)
    expect_error(sankey(r, debug = TRUE, edge_f = 0.2, node_s = "constant"), NA)
    expect_error(sankey(r, debug = TRUE, edge_f = 0.2, node_s = "prop"), NA)
})


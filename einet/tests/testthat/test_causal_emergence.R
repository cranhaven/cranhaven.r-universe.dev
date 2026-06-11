context("Testing Causal Emergence")

MAX_ERROR <- 0.001

### Testing Markov Blanket ---------------------------------------------
test_that("Markov Blanket is calculated correctly", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  ) %>%
    igraph::graph.adjacency(mode = "directed")

  blanket <- mb(graph, 2)

  expect_true(1 %in% blanket[[1]])
  expect_true(3 %in% blanket[[1]])
})

### Testing Causal Emergence --------------------------------------------
test_that("Causal Emergence is calculated correctly", {
  graph <- matrix(
    cbind(
      c(0.0, 2.0, 1.0),
      c(0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0)
    ),
    nrow = 3
  ) %>%
    igraph::graph.adjacency(mode = "directed")

  ce <- causal_emergence(graph)

  expect_lte(ce$ei_macro - 0, MAX_ERROR)
  expect_lte(ce$ei_micro - 0, MAX_ERROR)
  expect_lte(ce$ce - 0, MAX_ERROR)
})

### Testing Causal Emergence --------------------------------------------
test_that("Causal Emergence is calculated with and_and", {
  and_and <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  ce <- causal_emergence(and_and)

  expect_true(length(ce) > 0)
})


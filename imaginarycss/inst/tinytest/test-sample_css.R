
set.seed(331)
x <- matrix(sample.int(2, 16, replace = TRUE), ncol = 4) - 1L
diag(x) <- 0

# Perfect accuracy -------------------------------------------------------------
graph <- new_barry_graph(list(x, x, x, x))
acc <- tie_level_accuracy(graph)
expect_true(all(unlist(
  acc[, -1],
  recursive = TRUE
  ) == 1))

# Sampling should always get the same ------------------------------------------
graphs0 <- sample_css_network(graph, acc, keep_baseline = FALSE)
graphs1 <- sample_css_network(graph, acc, keep_baseline = FALSE)
expect_equal(graphs0, graphs1)
expect_equal(graphs0[[1]], x, check.attributes = FALSE)
expect_equal(graphs0[[2]], x, check.attributes = FALSE)

# One of them is 3/4 -----------------------------------------------------------
x2_3 <- x
x2_3[1, 2] <- 0
graph <- new_barry_graph(list(x, x2_3, x, x))

x2_3 <- structure(list(p_0_ego = c(1, 1, 1), p_1_ego = c(3/4, 1, 1), p_0_alter = c(1, 
1, 1), p_1_alter = c(1, 1, 1)), class = c("data.frame", "tie_level_accuracy"
), row.names = c(NA, -3L))

expect_equal(
  tie_level_accuracy(graph)[,-1],
  x2_3)

# Empty are zero ---------------------------------------------------------------
x_zero <- x
x_zero[1,] <- 0
x_zero[,1] <- 0

graph <- new_barry_graph(list(x_zero, x, x, x))

x_zero <- structure(list(k = 1:3, p_0_ego = c(0.333333333333333, 0.75, 
  0.75), p_1_ego = c(NA, 1, 1), p_0_alter = c(1, 0.4, 0.4), p_1_alter = c(1, 
  1, 1)), row.names = c(NA, -3L), class = c("data.frame", "tie_level_accuracy"
))

acc <- tie_level_accuracy(graph)
expect_equal(acc, x_zero)

# And NAs shouldn't affect sampling
samp <- sample_css_network(graph, acc)
samp <- sapply(samp[-1], function(x) {
  x <- attr(x, "probs")
  diag(x) <- -1L
  x <- as.vector(x)
  all(is.finite(x[x!=-1]))
})

expect_true(all(samp))


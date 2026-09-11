## -----------------------------------------------------------------------------
#| label: setup
library(imaginarycss)


## -----------------------------------------------------------------------------
#| label: basic-setup
# Build a true network and one perceiver's view
source_ <- c(1, 2, 3, 1, c(1, 2, 3) + 4)
target_ <- c(2, 1, 4, 4, c(2, 1, 4) + 4)

adjmat <- matrix(0L, nrow = 8, ncol = 8)
adjmat[cbind(source_, target_)] <- 1L
graph <- new_barry_graph(adjmat, n = 4)


## -----------------------------------------------------------------------------
#| label: print-graph
print(graph)


## -----------------------------------------------------------------------------
#| label: recip-errors
#| fig.height: 4
#| fig.width: 6
recip_errors <- count_recip_errors(graph)
barplot(
  recip_errors$value,
  names.arg = recip_errors$name,
  horiz = TRUE, las = 1,
  col = "steelblue",
  xlab = "Count",
  main = "Reciprocity Errors by Type"
)


## -----------------------------------------------------------------------------
#| label: census
#| fig.height: 5
#| fig.width: 7
census <- count_imaginary_census(graph)

# Aggregate by motif type using the summary method
agg <- summary(census)
agg <- sort(agg)

par(mar = c(4, 12, 3, 1))
barplot(
  agg,
  horiz = TRUE, las = 1,
  col = "steelblue",
  xlab = "Count",
  main = "Imaginary Census Distribution"
)


## -----------------------------------------------------------------------------
#| label: accuracy
#| fig.height: 5
#| fig.width: 6
accuracy <- tie_level_accuracy(graph)
acc_mat <- as.matrix(accuracy[, c("p_0_ego", "p_1_ego", "p_0_alter", "p_1_alter")])

boxplot(
  acc_mat,
  names = c("TN (Ego)", "TP (Ego)", "TN (Alter)", "TP (Alter)"),
  ylab = "Probability",
  main = "Individual-Level Accuracy Rates",
  col = c("#3498db", "#2980b9", "#e74c3c", "#c0392b"),
  border = "gray30"
)


## -----------------------------------------------------------------------------
#| label: sampling
#| fig.height: 4
#| fig.width: 6
set.seed(123)
n_samples <- 100

null_densities <- replicate(n_samples, {
  nets <- sample_css_network(graph, keep_baseline = FALSE)
  sum(nets[[1]]) / (4 * 3)
})

sampled <- sample_css_network(graph, keep_baseline = TRUE)
observed_density <- sum(sampled[[2]]) / (4 * 3)

hist(
  null_densities,
  breaks = 15,
  col = "steelblue",
  border = "white",
  main = "Null Distribution of Network Density",
  xlab = "Density"
)
abline(v = observed_density, col = "red", lwd = 2, lty = 2)
legend("topright",
  legend = paste("Observed =", round(observed_density, 3)),
  col = "red", lty = 2, lwd = 2, bty = "n"
)


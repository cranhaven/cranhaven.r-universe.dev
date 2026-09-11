## -----------------------------------------------------------------------------
#| label: setup
#| message: false
library(imaginarycss)

# Load Krackhardt data
data(krackhardt_advice)
data(krackhardt_friendship)
data(krackhardt_friendship_perceptions)
data(krackhardt_advice_perceptions)

# Convert edge-list data frames to adjacency matrices
df_to_adjmat <- function(df) {
  n <- max(c(df$from, df$to))
  m <- matrix(0L, nrow = n, ncol = n)
  m[cbind(df$from, df$to)] <- df$value
  m
}

advice_matrix     <- df_to_adjmat(krackhardt_advice)
friendship_matrix <- df_to_adjmat(krackhardt_friendship)


## -----------------------------------------------------------------------------
#| label: data-overview
n <- nrow(advice_matrix)

# Network summary
data.frame(
  Network    = c("Friendship", "Advice"),
  Edges      = c(sum(friendship_matrix), sum(advice_matrix)),
  Density    = round(c(
    sum(friendship_matrix) / (n * (n - 1)),
    sum(advice_matrix)     / (n * (n - 1))
  ), 3)
)


## -----------------------------------------------------------------------------
#| label: graphs
friendship_graph <- new_barry_graph(
  c(list(friendship_matrix), krackhardt_friendship_perceptions)
)
advice_graph <- new_barry_graph(
  c(list(advice_matrix), krackhardt_advice_perceptions)
)


## -----------------------------------------------------------------------------
#| label: observed
friendship_observed <- count_imaginary_census(friendship_graph)
advice_observed     <- count_imaginary_census(advice_graph)

# Friendship motifs (top 6)
head(summary(friendship_observed))

# Advice motifs (top 6)
head(summary(advice_observed))


## -----------------------------------------------------------------------------
#| label: test
#| fig.height: 7
#| fig.width: 8
set.seed(331)
friendship_test <- test_imaginary_census(friendship_graph, n_sim = 100)
advice_test     <- test_imaginary_census(advice_graph, n_sim = 100)


## -----------------------------------------------------------------------------
#| label: results
#| fig.height: 7
#| fig.width: 8
# Print significant motifs
friendship_test
advice_test

# Visualise z-scores
plot(friendship_test, main = "Friendship: Motif Z-Scores vs Null")
plot(advice_test,     main = "Advice: Motif Z-Scores vs Null")


## -----------------------------------------------------------------------------
#| label: summary
summary(friendship_test)


## -----------------------------------------------------------------------------
#| label: accuracy
friendship_acc <- tie_level_accuracy(friendship_graph)
advice_acc     <- tie_level_accuracy(advice_graph)

# Mean individual accuracy
data.frame(
  Network = c("Friendship", "Advice"),
  TP      = round(c(
    mean(friendship_acc$p_1_ego, na.rm = TRUE),
    mean(advice_acc$p_1_ego,     na.rm = TRUE)
  ), 3),
  TN      = round(c(
    mean(friendship_acc$p_0_ego, na.rm = TRUE),
    mean(advice_acc$p_0_ego,     na.rm = TRUE)
  ), 3)
)


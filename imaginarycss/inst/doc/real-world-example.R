## -----------------------------------------------------------------------------
#| label: setup
library(imaginarycss)


## -----------------------------------------------------------------------------
#| label: data
data(krackhardt_advice)
data(krackhardt_advice_perceptions)

# Convert edge-list data frame to adjacency matrix
n_people <- max(c(krackhardt_advice$from, krackhardt_advice$to))
advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <- krackhardt_advice$value

krack_graph <- new_barry_graph(c(list(advice_matrix), krackhardt_advice_perceptions))


## -----------------------------------------------------------------------------
#| label: plot-network
print(krack_graph)


## -----------------------------------------------------------------------------
#| label: accuracy
acc <- tie_level_accuracy(krack_graph)
acc


## -----------------------------------------------------------------------------
#| label: accuracy-summ
#| fig.height: 5
#| fig.width: 6

# Mean accuracy rates as a data.frame
data.frame(
  Measure = c("TP (Ego)", "TN (Ego)", "TP (Alter)", "TN (Alter)"),
  Mean    = round(c(
    mean(acc$p_1_ego,   na.rm = TRUE),
    mean(acc$p_0_ego,   na.rm = TRUE),
    mean(acc$p_1_alter, na.rm = TRUE),
    mean(acc$p_0_alter, na.rm = TRUE)
  ), 3)
)

acc_mat <- as.matrix(acc[, c("p_0_ego", "p_1_ego", "p_0_alter", "p_1_alter")])
boxplot(
  acc_mat,
  names = c("TN (Ego)", "TP (Ego)", "TN (Alter)", "TP (Alter)"),
  ylab = "Probability",
  main = "Individual Accuracy Rates",
  col = c("#3498db", "#2980b9", "#e74c3c", "#c0392b"),
  border = "gray30"
)


## -----------------------------------------------------------------------------
#| label: structure
#| collapse: true
krack_census <- count_imaginary_census(krack_graph)
krack_recip  <- count_recip_errors(krack_graph)

# Top imaginary census motifs
head(krack_census, 5)

# Reciprocity errors
head(krack_recip, 5)


## -----------------------------------------------------------------------------
#| label: takeaways
#| collapse: true
density <- sum(advice_matrix) / (n_people * (n_people - 1))
avg_tp  <- mean(acc$p_1_ego, na.rm = TRUE)
best    <- which.max(acc$p_1_ego)
worst   <- which.min(acc$p_1_ego)

# Network summary
data.frame(
  Statistic = c("Employees", "Advice network density",
                "Avg TP (Ego)", "Most accurate perceiver",
                "Least accurate perceiver"),
  Value     = c(n_people, round(density, 3), round(avg_tp, 3),
                paste0(best, " (", round(acc$p_1_ego[best], 3), ")"),
                paste0(worst, " (", round(acc$p_1_ego[worst], 3), ")"))
)


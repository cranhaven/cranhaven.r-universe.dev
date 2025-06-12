## ---- include = FALSE---------------------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE
# )

## ----setup, echo = FALSE------------------------------------------------------
library(RJcluster)

## -----------------------------------------------------------------------------
high_balanced = simulate_HD_data()
low_balanced = simulate_HD_data(signal_variance = 2)
high_unbalanced = simulate_HD_data(size_vector = c(20, 20, 80, 80))

print(dim(high_balanced$X))
print(dim(low_balanced$X))
print(dim(high_unbalanced$X))


## -----------------------------------------------------------------------------
res_high_balanced = RJclust(data = high_balanced$X)
res_low_balanced = RJclust(data = low_balanced$X)
res_high_unbalanced = RJclust(data = high_unbalanced$X)

results = list(res_high_balanced, res_low_balanced, res_high_unbalanced)
data = list(high_balanced, low_balanced, high_unbalanced, high_balanced, low_balanced, high_unbalanced)

## -----------------------------------------------------------------------------
for (i in 1:length(results))
{
  temp_results = results[[i]]
  mi = Mutual_Information(temp_results$class, data[[i]]$Y)
  print(paste("Number of classes found:", temp_results$K, "NMI:", round(mi$nmi,2), "AMI", round(mi$ami,2)))
}


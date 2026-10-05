## ----setup--------------------------------------------------------------------
library(ISCA)

## -----------------------------------------------------------------------------
data("sim_data")
head(sim_data)

ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, majority_group=1, minority_group=c(0), fuzzifier = 1.5,
                                      n_clusters=3, draws=5, cluster_vars= c("female", "age", "education", "income"))

head(ISCA_step1)

## -----------------------------------------------------------------------------
tail(ISCA_step1, 6)

## -----------------------------------------------------------------------------
majority_only <- ISCA_step1[ISCA_step1["native"] == 1, ]

result_ISCA_clustertable <- ISCA_clustertable(data = majority_only, 
                                              cluster_vars = c("native", "female", "age", "education", "income"),
                                              draws = 5)

head(result_ISCA_clustertable, 50)

## -----------------------------------------------------------------------------
ISCA_modeling_results <- ISCA_modeling(data= ISCA_step1, 
                                   model_spec="religiosity ~ native + female + age + education + discrimination",
                                   draws = 5, n_clusters = 3, weights = NULL)

estimates <- as.data.frame(ISCA_modeling_results[1])
head(estimates, 30) # OLS estimates per cluster across iterations

rsquared <- as.data.frame(ISCA_modeling_results[2])
head(rsquared)  # Corresponding Adjusted R Squared values per cluster across iterations

## -----------------------------------------------------------------------------
cluster_as_columns <- estimates %>%
  tidyr::pivot_longer(cols = c(mean_coefficients, mean_std.error, mean_p_value),
               names_to = "Statistics",
               values_to = "value") %>%
  tidyr::pivot_wider(names_from = cluster, values_from = value)

print(cluster_as_columns)


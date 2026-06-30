library(mvalpha)

### replicate example from Table 3 in Krippendoff and Craggs (2016) with bootstrapped estimates

# View data
ex_table3

# # Estimate alpha
# x <- mvalpha(ex_table3, verbose = TRUE, n_boot = 500)
#
# # View result
# x
#
# # View the unique values observed in the data
# x$values
#
# # View the unique labels used to code the data
# x$labels
#
# # Histogram of bootstrapped estimates
# hist(x$bootstrap_mvalpha)

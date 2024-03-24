# Create empty data frame to store the p-values at the different iterations
opt_trace <- as.data.frame(matrix(data = NA, 100, 3,
                                  dimnames = list(NULL,c("ite","pval","max"))))

# Generate some random p-values
for (ite in 1:100){
    pval <- runif(1,0,.6)
    opt_trace[ite,] <- c(ite, pval, max(pval, opt_trace$pval, na.rm = TRUE))
}

# Plot p-values for every iterations
MaxMC:::monitor_mmc(opt_trace, alpha = 0.05, monitor = TRUE)


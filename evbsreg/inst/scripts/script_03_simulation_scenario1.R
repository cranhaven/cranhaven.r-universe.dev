################################################################
## script_03_simulation_scenario1.R
##
## Reproduces Tables 4 and 5 of the paper:
## Monte Carlo simulation - Scenario 1 (canonical setting).
##
## Configuration in the paper:
##   m (replicates)  = 5000
##   n (sample size) = 60, 120, 180
##   beta0 = 0.5, beta1 = 0.5, alpha = 0.5
##   gama in {-0.2, -0.1, 0.1, 0.2}
##
## Usage:
##   library(evbsreg)
##   source(system.file("scripts/script_03_simulation_scenario1.R",
##                       package = "evbsreg"))
################################################################

library(evbsreg)

m <- 5000               # change to 500 for a quick check
gama_vals <- c(-0.20, -0.10, 0.10, 0.20)
n_vals    <- c(60, 120, 180)

cat(sprintf("Scenario 1 (canonical): m=%d, n=%s, gama=%s\n",
            m, paste(n_vals, collapse=","), paste(gama_vals, collapse=",")))

results <- list()
for (g in gama_vals) {
  for (n in n_vals) {
    key <- sprintf("gama=%g_n=%d", g, n)
    cat(sprintf("\n--- %s ---\n", key))
    t0 <- Sys.time()
    res <- evbsreg.fit.mc(m = m, n = n,
                          beta0 = 0.5, beta1 = 0.5, alpha = 0.5, gama = g,
                          scenario = "canonical", semente = 2023)
    print(res)
    cat(sprintf("Elapsed: %.1f s\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    results[[key]] <- res
  }
}

save(results, file = file.path(tempdir(), "results_scenario1.RData"))
cat("\nResults saved to results_scenario1.RData\n")

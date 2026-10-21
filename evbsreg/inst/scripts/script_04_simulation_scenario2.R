################################################################
## script_04_simulation_scenario2.R
##
## Reproduces Tables 6 and 7 of the paper:
## Monte Carlo simulation - Scenario 2 (leverage / extreme covariate).
################################################################

library(evbsreg)

m <- 5000               # change to 500 for a quick check
gama_vals <- c(-0.20, -0.10, 0.10, 0.20)
n_vals    <- c(60, 120, 180)

cat(sprintf("Scenario 2 (leverage): m=%d, n=%s, gama=%s\n",
            m, paste(n_vals, collapse=","), paste(gama_vals, collapse=",")))

results <- list()
for (g in gama_vals) {
  for (n in n_vals) {
    key <- sprintf("gama=%g_n=%d", g, n)
    cat(sprintf("\n--- %s ---\n", key))
    t0 <- Sys.time()
    res <- evbsreg.fit.mc(m = m, n = n,
                          beta0 = 0.5, beta1 = 0.5, alpha = 0.5, gama = g,
                          scenario = "leverage", semente = 2023)
    print(res)
    cat(sprintf("Elapsed: %.1f s\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    results[[key]] <- res
  }
}

save(results, file = file.path(tempdir(), "results_scenario2.RData"))
cat("\nResults saved to results_scenario2.RData\n")

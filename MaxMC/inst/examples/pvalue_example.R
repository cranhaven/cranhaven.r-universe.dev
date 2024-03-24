# Generate sample S0 and simulate statistics
S0 = 0
S = rnorm(99)

# Compute p-value
pvalue(S0, S, type = "geq")

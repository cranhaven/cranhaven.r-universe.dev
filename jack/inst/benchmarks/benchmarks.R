library(jack)
library(microbenchmark)

microbenchmark(
  JackPol = JackPol(n = n, lambda = lambda, alpha = alpha),
  JackSymPol = JackSymPol(n = n, lambda = lambda),
  JackPol2 = evalSymbolicQspray(JackSymPol(n = n, lambda = lambda), a = alpha),
  setup = {
    n <- 5
    lambda <- c(4, 2, 2, 1)
    alpha <- 2
  },
  times = 5
)

# Haskell is faster:
#
# benchmarking Jack/jackPol with the given alpha
# mean                 33.76 ms
# std dev              3.004 ms
#
# benchmarking Jack/jackSymbolicPol
# mean                 543.8 ms
# std dev              44.58 ms
#
# benchmarking Jack/jackSymbolicPol evaluated at alpha
# mean                 621.9 ms
# std dev              50.79 ms

# Julia is faster:
#
# julia> @benchmark JackPolynomial(5, [4; 2; 2; 1])
# BenchmarkTools.Trial: 15 samples with 1 evaluation.
#  Range (min … max):  261.262 ms … 498.055 ms  ┊ GC (min … max): 21.28% … 27.22%
#  Time  (median):     342.574 ms               ┊ GC (median):    18.62%
#  Time  (mean ± σ):   349.626 ms ±  65.011 ms  ┊ GC (mean ± σ):  22.29% ±  5.76%


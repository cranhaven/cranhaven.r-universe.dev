# couplr

*matching two groups into pairs*

[![CRAN status](https://www.r-pkg.org/badges/version/couplr)](https://CRAN.R-project.org/package=couplr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/couplr)](https://cran.r-project.org/package=couplr)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/couplr)](https://cran.r-project.org/package=couplr)
[![R-CMD-check](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml)
[![Coverage](https://codecov.io/gh/gcol33/couplr/graph/badge.svg?flag=r)](https://app.codecov.io/gh/gcol33/couplr)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Optimal one-to-one matching by linear assignment, solved exactly in C++.**

Hand `couplr` two groups and it returns the pairing that minimizes total covariate
distance across the whole sample. The assignment is solved exactly, by
Jonker-Volgenant on ordinary inputs or by any of nineteen solvers named explicitly, so
the total distance is the global minimum and a solve can be certified optimal.

```r
library(couplr)

# match treatment and control on covariates, in a single call
result <- match_couples(treated, control, vars = c("age", "income"), auto_scale = TRUE)

# analysis-ready paired data
join_matched(result, treated, control)
```

## Optimal and deterministic

`match_couples()` evaluates the pairing as a whole and returns the assignment with the
lowest total covariate distance. Because the solve is exact, the total distance is the
global minimum. There is no random seed: the same input gives the same pairing on every
run. When several pairings tie at the minimum, which of them comes back can depend on
the order of the rows.

Large pools are solved exactly as well. `memory_mode = "lazy"` computes each distance
when the solver needs it, and `memory_mode = "implicit"` generates only the pairs the
optimum turns out to need, so neither builds the full distance matrix.
`method = "greedy"` is a speed trade: it gives up the optimality guarantee and keeps the
same scaling, constraints, and blocking.

```r
match_couples(treated, control, vars = c("age", "income"), auto_scale = TRUE)   # optimal

match_couples(treated, control, vars = c("age", "income"),
              memory_mode = "implicit")                                          # optimal, large pools

match_couples(treated, control, vars = c("age", "income"),
              method = "greedy", strategy = "pq")                               # approximate, fast
```

## Scaling and constraints

Covariates on different scales are put on common footing before distances are computed,
and pairs can be constrained or matched within strata:

```r
match_couples(
  treated, control,
  vars         = c("age", "income", "bmi"),
  scale        = "robust",             # or "standardize", "range"; auto_scale = TRUE picks one
  max_distance = 2,                    # forbid pairs beyond this covariate distance
  calipers     = list(age = 5),        # per-variable maximum absolute difference
  block_id     = "site",               # match only within site
  ratio        = 2                     # two controls per treated unit
)
```

## A family of matching designs

The same two-group input drives several established designs, each returning a result the
diagnostics and join helpers understand:

```r
# variable-ratio full matching
full_match(treated, control, vars = c("age", "income"), min_controls = 1, max_controls = 3)

# coarsened exact matching
cem_match(treated, control, vars = c("age", "income"))

# balance-constrained cardinality matching
cardinality_match(treated, control, vars = c("age", "income"), fine = "site")

# propensity subclassification and propensity matching, formula interface
subclass_match(treated ~ age + income, data = df, n_subclasses = 5)
ps_match(treated ~ age + income, data = df)
```

## Balance and sensitivity

Check covariate balance before and after matching, and probe how sensitive a conclusion is
to unmeasured confounding:

```r
bal <- balance_diagnostics(result, treated, control)   # standardized differences, variance ratios, KS tests
bal
autoplot(bal)                           # love plot of standardized differences

sensitivity_analysis(result, treated, control, outcome_var = "recovery_days")   # Rosenbaum bounds
```

## Works with the matching ecosystem

A `couplr` result converts to `matchit`-class with `as_matchit()`, so `cobalt` balance
tables and `marginaleffects` estimates run against it without rewiring the analysis.
`match_data()` returns treatment, weights, and subclass columns in one analysis-ready
frame, and `autoplot()` methods cover matching results, balance, and sensitivity.

```r
m  <- as_matchit(result, treated, control)
md <- match_data(result, treated, control)
```

## The assignment backend

`lap_solve()` exposes the solver layer directly. It takes a cost matrix, handles
rectangular shapes and forbidden edges (`NA` / `Inf`), and offers nineteen solvers by name.
`method = "auto"` enumerates problems up to 8 by 8, uses Hopcroft-Karp when the finite
costs are all equal or all 0 or 1, and otherwise Jonker-Volgenant:

```r
cost <- matrix(c(4, 2, 8, 4, 3, 7, 3, 1, 6), nrow = 3, byrow = TRUE)

lap_solve(cost)                      # auto-selected solver
lap_solve(cost, method = "hungarian")
lap_solve_kbest(cost, k = 3)         # the three best assignments (Murty's algorithm)
lap_solve_batch(problems)            # many independent problems in one call
```

The solvers span the classics and the scaling algorithms: Jonker-Volgenant, Hungarian,
Kuhn-Munkres, Bertsekas auction (with epsilon-scaling variants), Goldberg-Kennedy
cost-scaling, Gabow-Tarjan bit-scaling, push-relabel, network simplex, and Sinkhorn
entropy-regularized transport. `bottleneck_assignment()` minimizes the largest edge
instead of the total, and `sinkhorn()` returns a soft transport plan.

## Watching a solve

`pixel_morph()` treats two images as source and target pixel sets and morphs one into the
other along the optimal assignment, so a matching problem becomes something you can look at:

```r
pixel_morph(imgA, imgB, n_frames = 16)
```

## Installation

```r
install.packages("couplr")            # CRAN

install.packages("pak")               # development version
pak::pak("gcol33/couplr")
```

## Documentation

- [Getting Started](https://gillescolling.com/couplr/articles/getting-started.html)
- [Matching Workflows](https://gillescolling.com/couplr/articles/matching-workflows.html)
- [Algorithms](https://gillescolling.com/couplr/articles/algorithms.html)
- [Comparison](https://gillescolling.com/couplr/articles/comparison.html)
- [Pixel Morphing](https://gillescolling.com/couplr/articles/pixel-morphing.html)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good tools
should be free and open. I started these projects for my own work and figured others
might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say thanks.
It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE file)

## Citation

```bibtex
@software{couplr,
  author = {Colling, Gilles},
  title  = {couplr: Optimal Matching via Linear Assignment},
  year   = {2026},
  url    = {https://github.com/gcol33/couplr}
}
```

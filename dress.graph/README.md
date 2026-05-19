# dress.graph (R)

**A Continuous Framework for Structural Graph Refinement**

DRESS is a deterministic, parameter-free framework for continuous structural graph refinement. It iterates a nonlinear dynamical system on real-valued edge similarities and produces a graph fingerprint as a sorted edge-value vector once the iteration reaches a prescribed stopping criterion. The resulting fingerprint is self-contained, isomorphism-invariant by construction, reproducible across vertex labelings under the reference implementation, numerically robust in practice, and efficient to compute with straightforward parallelization and distribution.

## Install

CRAN hosts a stable release. For the latest version, install from GitHub.

```r
# From CRAN (stable)
install.packages("dress.graph")

# From GitHub (latest)
# install.packages("remotes")
remotes::install_github("velicast/dress-graph", subdir="r")
```

## Quick start

```r
library(dress.graph)

result <- fit(
  n_vertices = 4L,
  sources    = c(0L, 1L, 2L, 0L),
  targets    = c(1L, 2L, 3L, 3L)
)
print(result$edge_dress)
```

For the full API and documentation, see the [main repository](https://github.com/velicast/dress-graph).

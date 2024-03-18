
# Rtapas

<!-- badges: start -->
<!-- badges: end -->

We introduce Rtapas (v1.2), an R package to perform Random Tanglegram Partitions (Balbuena et al. 2020). Rtapas applies a given global-fit method to random partial tanglegrams of a fixed size to identify the associations, terminals, and nodes that maximize phylogenetic congruence.
Incorporates ParaFit (Legendre et al. 2002), geodesic distances (GD) (Schardl et al. 2008) and PACo (Balbuena et al. 2013) as global-fit methods to implement Random TaPas. Rtapas further enhances the usability and implementation of Random TaPas by including functions (i) to facilitate the prior processing of association data between taxa, (ii) to estimate, in a set of probability trees, the statistic of a given global-fit method, (iii) to estimate the (in)congruence metrics of the individual host-symbiont associations, and (iv) to compute either conventional (G) or normalized (G*) (Raffinetti et al. 2015) characterizing the distribution of such metrics.

## Installation
 
You can install the development version of Rtapas from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mllaberia/Rtapas")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Rtapas)
data(nuc_pc)
N = 1e+2  # we recommend using 1e+4.
n = n = round(sum(np_matrix)*0.2)
NPc <- max_cong(np_matrix, NUCtr, CPtr, n, N, method = "paco",
               symmetric = FALSE, ei.correct = "sqrt.D",
               percentile = 0.01, res.fq = FALSE,
               strat = "parallel", cl = 10)

tangle_gram(NUCtr, CPtr, np_matrix, NPc, colscale = "sequential", 
            colgrad = c("darkorchid4", "gold"), nbreaks = 50, node.tag = TRUE, 
            cexpt = 1.2, link.lwd = 1, link.lty = 1, fsize = 0.75, pts = FALSE,
            ftype ="i")

```


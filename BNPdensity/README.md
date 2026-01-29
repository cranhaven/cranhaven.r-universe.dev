
- [BNPdensity](#bnpdensity)
  - [Installation](#installation)
  - [Problem setting](#problem-setting)
  - [Obtaining a Bayesian Nonparametric density estimate in a few
    seconds](#obtaining-a-bayesian-nonparametric-density-estimate-in-a-few-seconds)
  - [How to select the parameters of the Normalized Generalized Gamma
    process](#how-to-select-the-parameters-of-the-normalized-generalized-gamma-process)
  - [How to use the convergence
    diagnostics](#how-to-use-the-convergence-diagnostics)
  - [How to use the Goodness of fit
    plots](#how-to-use-the-goodness-of-fit-plots)
    - [Non censored data](#non-censored-data)
    - [Censored data](#censored-data)
  - [Posterior analysis of the clustering
    structure](#posterior-analysis-of-the-clustering-structure)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# BNPdensity

Bayesian nonparametric density estimation modeling mixtures by a
Ferguson-Klass type algorithm for posterior normalized random measures.

## Installation

You can install BNPdensity from github with:

``` r
# install.packages("devtools")
devtools::install_github("konkam/BNPdensity", dependencies = TRUE, upgrade = TRUE)
```

You will need to have the CRAN package `devtools` installed.

## Problem setting

We consider a one-dimensional density estimation problem. As an example,
we pick the `acidity` dataset, which contains an acidity index measured
in a sample of 155 lakes in north-central Wisconsin (Crawford et
al. (1992)).

We illustrate the package by estimating the distribution of the
`acidity` dataset.

Let’s have a quick overview of the acidity dataset:

``` r
library(BNPdensity)
data(acidity)
str(acidity)
#>  num [1:155] 2.93 3.91 3.73 3.69 3.82 ...
```

``` r
hist(acidity)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This dataset shows clear signs of multimodality, and it might be
tempting to fit a bimodal normal distribution. However, some asymmetry
in the two apparent clusters, or the extreme point on the left suggest
that a mixture with more components might be more appropriate.

A Bayesian Nonparametric approach avoids the need to specify an
arbitrary number of clusters in advance, it rather aims at estimating an
optimal number of clusters from the dataset by means of an infinite
mixture model.

The most famous Bayesian Nonparametric model for infinite mixtures is
the [Dirichlet
process](https://en.wikipedia.org/wiki/Dirichlet_process). However, it
implies a fairly informative a priori distribution on the number of
components in the mixture: the mode of the prior distribution is
proportional to the logarithm of the number of data points, and the
distribution is fairly peaked. This might not accurately reflect the
prior information available on the number of components, or might induce
an unwanted bias in the case of prior mis-specification. Models more
general than the Dirichlet process, such as Normalized Random Measure
models (Barrios et al. (2013)) allow overcoming this limitation by
specifying a **less informative** prior. In what follows, we present how
to use Normalized Random Measure models for density estimation.

## Obtaining a Bayesian Nonparametric density estimate in a few seconds

``` r
library(BNPdensity)
data(acidity)
fit = MixNRMI1(acidity, Nit = 3000)
#> MCMC iteration 500 of 3000 
#> MCMC iteration 1000 of 3000 
#> MCMC iteration 1500 of 3000 
#> MCMC iteration 2000 of 3000 
#> MCMC iteration 2500 of 3000 
#> MCMC iteration 3000 of 3000 
#>  >>> Total processing time (sec.):
#>    user  system elapsed 
#> 136.064   0.144 136.288
```

`MixNRMI1()` creates an object of class `MixNRMI1`, for which we provide
common S3 methods.

``` r
print(fit)
#> Fit of a semiparametric normal mixture model on 155 data points.
#> The MCMC algorithm was run for 3000 iterations with 10 % discarded for burn-in.
```

``` r
plot(fit)
#> Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
#> ℹ Please use `after_stat(density)` instead.
#> ℹ The deprecated feature was likely used in the BNPdensity package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## How to select the parameters of the Normalized Generalized Gamma process

We suggest the Normalized stable process, which corresponds to setting
`Alpha = 1, Kappa = 0` in the `MixNRMIx` functions. The stable process
is a convenient model because its parameter γ has a convenient
interpretation: it can be used to tune how informative the prior on the
number of components is. Small values of `Gama` bring the process closer
to a Dirichlet process, where the prior on the number of components is a
relatively peaked distribution around $\alpha \log n$. Larger values of
`Gama` make this distribution flatter. More guidelines on how to choose
the parameters may be found in Lijoi et al. (2007b), notably by
considering the expected prior number of components.

We provide a function to compute the expected number of components for a
normalized stable process:

``` r
library(BNPdensity)
expected_number_of_components_stable(100, 0.8)
#> 1 'mpfr' number of precision  5300   bits 
#> [1] 42.7094763347433063870164430179377602285097873537020509591067085108693089953308485568922629628925609670388816355409267145879811608727979635644774974037316163731591853953068391985644986476120011372960450707841949357628205832352455972108307720257846739029935831367778087474278900096286104350854420468743750780953727437388381753822866058766720173377298935849197133078525375521665344039604097969307794640101701067874475659220445305472235893927481095361462556416422489918801010270531398200441645390722656975243565592756687766475481859394293400264017298519219232910313654037149553666679890694710843958583376865920388291893659788102528801718172192946031922288386233821810410501186383355395149325749466413257183113733305353511545188982744656555395398972901331812210878443370420188050079296961588354717569901177367135749962447447224416420675294481931830396470678197938068495726792098492301861867848615495349033316737274460428156601729995582227093984927573684202948081480326602431090471715657360243449939490449
```

This number may be compared to the prior number of components induced by
a Dirichlet process with `Alpha = 1`:

``` r
expected_number_of_components_Dirichlet(100, 1.)
#> [1] 5.187378
```

We also provide a way to visualise the prior distribution on the number
of components:

``` r
plot_prior_number_of_components(50, 0.4) 
#> Computing the prior probability on the number of clusters for the Dirichlet process
#> Computing the prior probability on the number of clusters for the Stable process
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> \## How to
fit a dataset

We illustrate the package by estimating the distribution of the
`acidity` dataset.

``` r
library(BNPdensity)
data(acidity)
str(acidity)
#>  num [1:155] 2.93 3.91 3.73 3.69 3.82 ...
```

``` r
hist(acidity)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
library(BNPdensity)
data(acidity)
fit = MixNRMI1(acidity, Nit = 3000)
#> MCMC iteration 500 of 3000 
#> MCMC iteration 1000 of 3000 
#> MCMC iteration 1500 of 3000 
#> MCMC iteration 2000 of 3000 
#> MCMC iteration 2500 of 3000 
#> MCMC iteration 3000 of 3000 
#>  >>> Total processing time (sec.):
#>    user  system elapsed 
#> 132.491   0.047 132.594
```

`MixNRMI1()` creates an object of class `MixNRMI1`, for which we provide
common S3 methods.

``` r
print(fit)
#> Fit of a semiparametric normal mixture model on 155 data points.
#> The MCMC algorithm was run for 3000 iterations with 10 % discarded for burn-in.
```

``` r
plot(fit)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
summary(fit)
#> Density estimation using a Normalized stable process,
#> with stability parameter Gamma = 0.4
#> 
#> A semiparametric normal mixture model was used.
#> 
#> There were 155 data points.
#> 
#> The MCMC algorithm was run for 3000 iterations with 10% discarded for burn-in.
#> 
#> To obtain information on the estimated number of clusters,
#>  please use summary(object, number_of_clusters = TRUE).
```

## How to use the convergence diagnostics

We also provide an interface to run several chains in parallel, using
the functions `multMixNRMI1()`. We interface our package with the `coda`
package by providing a conversion method for the output this function.
This allows for instance to compute the convergence diagnostics included
in `coda`.

One detail is that due to the Nonparametric nature of the model, the
number of parameters which could potentially be monitored for
convergence of the chains varies. The location parameter of the
clusters, for instance, vary at each iteration, and even the labels of
the clusters vary, which makes them tricky to follow. However, it is
possible to monitor the log-likelihood of the data along the iterations,
the value of the latent variable `u`, the number of components and for
the semi-parametric model, the value of the common scale parameter.

``` r
library(BNPdensity)
library(coda)
#> 
#> Attaching package: 'coda'
#> The following object is masked from 'package:BNPdensity':
#> 
#>     traceplot
data(acidity)
fitlist = multMixNRMI1(acidity, Nit = 5000)
mcmc_list = as.mcmc(fitlist)
coda::traceplot(mcmc_list)
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

``` r
coda::gelman.diag(mcmc_list)
```

## How to use the Goodness of fit plots

### Non censored data

``` r
library(BNPdensity)
data(acidity)
fit = MixNRMI1(acidity, extras = TRUE)
#> MCMC iteration 500 of 1500 
#> MCMC iteration 1000 of 1500 
#> MCMC iteration 1500 of 1500 
#>  >>> Total processing time (sec.):
#>    user  system elapsed 
#>  57.618   0.099  57.746
GOFplots(fit)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Censored data

``` r
library(BNPdensity)
data(salinity)
fit = MixNRMI1cens(salinity$left,salinity$right, extras = TRUE)
#> MCMC iteration 500 of 1500 
#> MCMC iteration 1000 of 1500 
#> MCMC iteration 1500 of 1500 
#>  >>> Total processing time (sec.):
#>    user  system elapsed 
#>  55.512   0.005  55.526
GOFplots(fit)
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Posterior analysis of the clustering structure

The MCMC algorithm provides a sample of the posterior distribution on
the space of all clusterings. This is a very large discrete space, which
is not ordered. This means that for any reasonably sized problem, each
configuration in the posterior will have been explored no more than once
or twice, and that many potentially good configurations will not be
present in the MCMC sample. Moreover, the lack of ordering makes it not
trivial to summarize the posterior by an optimal clustering and to
provide credible sets.

We suggest using the approach developed in S. Wade and Z. Ghahramani,
“Bayesian cluster analysis: Point estimation and credible balls (with
discussion),” Bayesian Anal., vol. 13, no. 2, pp. 559–626, 2018.

The main proposal from this paper is to summarize the posterior on all
possible clusterings by an optimal clustering where optimality is
defined as minimizing the posterior expectation of a specific loss
function, the Variation of Information. Credible sets are also
available.

We use the implementation described in R. Rastelli and N. Friel,
“Optimal Bayesian estimators for latent variable cluster models,” Stat.
Comput., vol. 28, no. 6, pp. 1169–1186, Nov. 2018, which is faster and
implemented in the CRAN package `GreedyEPL`.

Using this approach requires installing the R package `GreedyEPL`, which
can be achieved with the following command:

``` r
install.packages("GreedyEPL")
```

Note that investigating the clustering makes more sense for the fully
Nonparametric NRMI model than for the Semiparametric. This is because to
use a single scale parameters for all the clusters, the Semiparametric
model may favor numerous small clusters, for flexibility. The larger
number of clusters may render interpretation of the clusters more
challenging.

The clustering structure may be visualized as follows:

``` r
data(acidity)
out <- MixNRMI2(acidity,  extras = TRUE)
#> MCMC iteration 500 of 1500 
#> MCMC iteration 1000 of 1500 
#> MCMC iteration 1500 of 1500 
#>  >>> Total processing time (sec.):
#>    user  system elapsed 
#>  17.107   0.103  17.218
clustering = compute_optimal_clustering(out)
plot_clustering_and_CDF(out, clustering)
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

We now also offer the option of using the implementation described in:

> D. B. Dahl, D. J. Johnson, and P. Müller (2022),  
> *Search Algorithms and Loss Functions for Bayesian Clustering*,  
> *Journal of Computational and Graphical Statistics*, 31(4),
> 1189–1201.  
> <https://doi.org/10.1080/10618600.2022.2069779>

This approach is implemented in the R package `salso`, and can be used
as an alternative to `GreedyEPL` for computing the optimal clustering
under the same loss functions (e.g., Variation of Information). SALSO by
default performs multiple runs with different initialisations, which
improves the robustness of the optimisation results.

To use this alternative, simply specify the method in the
`compute_optimal_clustering` function:

``` r
#install.packages("salso")  # if not already installed

data(acidity)
out <- MixNRMI2(acidity, extras = TRUE)
#> MCMC iteration 500 of 1500 
#> MCMC iteration 1000 of 1500 
#> MCMC iteration 1500 of 1500 
#>  >>> Total processing time (sec.):
#>    user  system elapsed 
#>  20.550   0.046  20.626
clustering <- compute_optimal_clustering(out, method = "SALSO")
plot_clustering_and_CDF(out, clustering)
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

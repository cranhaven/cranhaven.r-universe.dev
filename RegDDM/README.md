
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegDDM

<!-- badges: start -->

<!-- badges: end -->

Build Regression models over Drift Diffusion Model parameters using
MCMC!

## Installation

You can install latest version of `RegDDM` from CRAN:

``` r
install.packages("RegDDM")
```

For RStudio users, you may need the following:

``` r
install.packages("rstudioapi")
```

## Example

First, load the package and the example dataset.

``` r
library(RegDDM)
data(regddm_data)
```

`data1` is the subject-level dataset:

``` r
head(regddm_data$data1)
#>   id     iq age gender  race education
#> 1  1 112.80  22      F White        14
#> 2  2 114.32  22      F White        16
#> 3  3 116.96  22      F Black        13
#> 4  4 111.68  31      F Black        16
#> 5  5 121.36  21      M Asian        16
#> 6  6 124.24  29      F White        18
```

`data2` is the subject-level dataset:

``` r
head(regddm_data$data2)
#>   id memload response        rt
#> 1  1       1        1 1.1772806
#> 2  1       1        1 2.2207544
#> 3  1       6        1 4.4166550
#> 4  1       6        1 0.8540982
#> 5  1       3        1 1.3794191
#> 6  1       3        0 0.8278006
```

Specify the model using a list. In this example, the drift rate `v` is
influenced by `memload`, which is the memory load of the trial. The
subject’s `iq` is predicted by baseline drift rate `v_0` (drift rate
when `memload` is 0), the influence of `memload` on drift rate
`v_memload` and covariates `age` and `education`:

``` r
model = list(
  v ~ memload,
  iq ~ v_memload + v_0 + age + education
)
```

Use the main function of `RegDDM` to automatically generate the `RStan`
model and summary the results. This could take ~20 minutes to run. The
rows starting with ‘beta\_’ are the posterior distributions of
regression parameters:

``` r
fit = regddm(
  regddm_data$data1,
  regddm_data$data2,
  model
)

print(fit)
#> RegDDM Model Summary
#> Number of subjects: 49
#> Number of trials: 6032
#> Model:
#>   v ~ memload
#>   iq ~ v_memload + v_0 + age + education
#> Family: gaussian
#> Sampling: 4 chains, 500 warmups and 1000 iterations were used. Longest elapsed time is 3218 s.
#> 
#> Regression coefficients:
#>         variable     mean     sd     2.5%   97.5% n_eff  Rhat
#> 1         beta_0 112.8863 12.308   87.834 135.485  1621 0.999
#> 2 beta_v_memload -54.3336 27.418 -110.982  -3.216   958 1.001
#> 3       beta_v_0  -3.6667  2.003   -7.883   0.156  2602 0.999
#> 4       beta_age   0.1293  0.329   -0.498   0.809  2541 0.998
#> 5 beta_education  -0.0422  0.601   -1.216   1.083  2652 0.999
#> 6          sigma   6.8007  0.826    5.350   8.599  2004 0.999
#> Maximum R-hat: 1.005
```

In this example, `iq` is negatively correlated with `v_memload`. The
higher the influence of `memload` on drift rate, the lower the `iq` of
the subject.

# Using your own data!

If you want to fit the model on your own data, you need to specify
`data1`, `data2` and `model`.

`data1` is subject-level data table. It should contain the following: \*
`id`: unique indexing column for each subject. \* other subject-level
variables that we want to include in the regression. Missing values are
supported

`data2` is trial-level data table. It should contain the following: \*
`id`: the subject of each trial using the same index in `data1`. \*
`rt`: response time of the trial in seconds. \* \`response\`\`: response
the trial. must be either 0 or 1. \* trial-level variables. These are
the variables that differ by trial, such as difficulty of the task or
different numbers on the screen. We assume that subjects’ behavior
changes according to these variables. These variables cannot contain
missing values.

`model` is the proposed dependency between these parameters. Default is
an empty list. It must be a list of 0 - 5 formulas. The outcome of these
formulas can be either: \* one of the four DDM parameters `a`, `t`, `z`,
`v`, modeling the relationship between DDM parameters and trial-level
variables. \* one formula for GLM regression, modeling the relationship
between estimated DDM parameters and other subject-level variables.

`family` is the family of distribution of GLM. It can be either
`"gaussian"`, `"bernoulli"` or `"poisson"`. Default is `"gaussian"`.

`init` is how we initialize the MCMC algorithm. The `"default"`
initialization should work in most conditions

`prior` determines whether to use the default prior for DDM parameters
or not. Default is `TRUE`

`stan_filename` is the file location for the automatically generated
stan model. If an empty string ’’ is provided, a temporary file will be
created and deleted after the model is fit. Default is
`"stan_model.stan"`

`gen_model` determines whether to generate the model or not. Default is
`TRUE`.

`fit_model` determines whether to fit the model or not. Default is
`TRUE`.

`...`: additional parameters used by `rstan`, including
`warmup`,`iter`,`chains`,`cores` etc.

# Citation

to be added

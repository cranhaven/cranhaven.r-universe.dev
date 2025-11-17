
<!-- README.md is generated from README.Rmd. Please edit that file -->

xtune: Tuning feature-specific shrinkage parameters of penalized
regression models based on external information

<!-- badges: start -->

<!-- badges: end -->

\=======

## 📗 Introduction

### Motivation

In standard regularized regression (Lasso, Ridge, and Elastic-net), a
single penalty parameter $\lambda$ applied equally to all regression
coefficients to control the amount of regularization in the model.

Better prediction accuracy may be achieved by allowing a **different amount of shrinkage**. Ideally, we want to give a small penalty to important features and a large penalty to unimportant features. We guide the penalized regression model with external data **$Z$**  that are potentially informative for the importance/effect size of coefficients and allow feature-specific shrinkage modeled as a log-linear function of the external data.

The objective function of feature-specific shrinkage integrating
external information is:



```math
\min_f \sum_{i = 1}^n V(f(x_i), y_i) + \textcolor{red}{\lambda} R(f)
```


```math
\textcolor{red}{\lambda = e^{Z \cdot \alpha}}
```

where $V$ represents loss function, $\lambda$ is the penalty/tuning
parameter, and $R(f)$ is the regularization/penalty term.
Specifically, we use Elastic-net type of penalty:

$$R(f) = \left[\sum_{k = 1}^K\bigg((1-c)||\beta_k||_2^2/2 + c||\beta_k||_1 \bigg) \right]$$

when $c = 1, 0$ or any value between 0 to 1, the model is equivalent
to LASSO, Ridge, and Elastic-net, respectively.

The idea of external data is that it provides us information on the
importance/effect size of regression coefficients. It could be any
nominal or quantitative feature-specific information, such as the
grouping of predictors, prior knowledge of biological importance,
external p-values, function annotations, etc. Each column of **$Z$** is a
variable for features in design matrix **$X$**. **$Z$** is of dimension
$p \times q$, where $p$ is the number of features and $q$ is the
number of variables in **$Z$**.

### Tuning multiple penalty parameters

Penalized regression fitting consists of two phases: (1) learning the
tuning parameter(s) (2) estimating the regression coefficients giving
the tuning parameter(s). Phase (1) is the key to achieve good
performance. Cross-validation is widely used to tune a single penalty
parameter, but it is computationally infeasible to tune more than three
penalty parameters. We propose an **Empirical Bayes** approach to
estimate the multiple tuning parameters. The individual penalties are
interpreted as variance terms of the priors (exponential prior for
Elastic-net) in a random effect formulation of penalized regressions. A
majorization-minimization algorithm is employed for implementation. Once
the tuning parameters $\lambda$ s are estimated, and therefore the
penalties are known, phase (2) - estimating the regression coefficients
is done using `glmnet`.

### Data structure examples

Suppose we want to predict a person’s weight loss using his/her weekly
dietary intake. Our external information Z could incorporate information
about the levels of relevant food constituents in the dietary items.

Primary data X and Y: predicting an individual’s weight loss by his/her
weekly dietary items intake

External information Z: the nutrition facts about each dietary item

## 📙 Installation

`xtune` can be installed from Github using the following command:

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("JingxuanH/xtune", 
                         build_vignettes = TRUE)

library(xtune)
```

## ✍ Citation

  - **xtune LASSO**: Zeng, Chubing, Duncan Campbell Thomas, and Juan
    Pablo Lewinger. “Incorporating prior knowledge into regularized
    regression.” Bioinformatics 37.4 (2021): 514-521.

  - **xtune classification with Elastic-net type of penalty**: paper
    coming soon

  - **xtune package**:

<!-- end list -->

``` r
citation("xtune")
#> Warning in citation("xtune"): no date field in DESCRIPTION file of package
#> 'xtune'
#> Warning in citation("xtune"): could not determine year for 'xtune' from package
#> DESCRIPTION file
#> 
#> To cite package 'xtune' in publications use:
#> 
#>   Jingxuan He and Chubing Zeng (NA). xtune: Regularized Regression with
#>   Feature-specific Penalties Integrating External Information. R
#>   package version 0.99.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {xtune: Regularized Regression with Feature-specific Penalties Integrating External Information},
#>     author = {Jingxuan He and Chubing Zeng},
#>     note = {R package version 0.99.0},
#>   }
```

Feel free to contact `hejingxu@usc.edu` if you have
any questions.

## 📘 Examples

To show some examples on how to use this package, we simulated an
example of data that contains 100 observations, 200 predictors, and a
continuous outcome. The external information Z contains 4 columns, each
column is indicator variable (can be viewed as the grouping of
predictors).

``` r
library(xtune)

## load the example data
data(example)
```

The data looks like:

``` r
example$X[1:3,1:5]
#>               Predictor_1 Predictor_2 Predictor_3 Predictor_4 Predictor_5
#> Observation_1  -0.7667960   0.9212806   2.0149030  0.79004563  -1.4244699
#> Observation_2  -0.8164583  -0.3144157  -0.2253684  0.08712746  -1.0296026
#> Observation_3  -0.1415352   0.6623149  -1.0398456  1.87611212   0.7340254
example$Z[1:5,]
#>             External_variable_1 External_variable_2 External_variable_3
#> Predictor_1                   1                   0                   0
#> Predictor_2                   1                   0                   0
#> Predictor_3                   0                   1                   0
#> Predictor_4                   0                   1                   0
#> Predictor_5                   0                   0                   1
#>             External_variable_4
#> Predictor_1                   0
#> Predictor_2                   0
#> Predictor_3                   0
#> Predictor_4                   0
#> Predictor_5                   0
```

`xtune()` is the core function to fit the integrated penalized
regression model. At a minimum, you need to specify the predictor matrix
`X`, outcome variable `Y`. If an external information matrix `Z` is
provided, the function will incorporate `Z` to allow differential
shrinkage based on Z. The estimated tuning parameters are returned in
`$penalty.vector`.

If you do not provide external information `Z`, the function will
perform empirical Bayes tuning to choose the single penalty parameter in
penalized regression, as an alternative to cross-validation. You could
compare the tuning parameter chosen by empirical Bayes tuning to that
choose by cross-validation (see also `cv.glmnet`). The default penalty
applied to the predictors is the Elastic-net penalty.

If you provide an identify matrix as external information Z to
`xtune()`, the function will estimate a separate tuning parameter
\(\lambda_j\) for each regression coefficient \(\beta_j\).

``` r
xtune.fit <- xtune(example$X,example$Y,example$Z, family = "linear")
#> Z provided, start estimating individual tuning parameters 
#> Start estimating alpha:
#> #-----------------Inner loop Iteration 1 Done-----------------#
#> #-----------------Inner loop Iteration 2 Done-----------------#
#> #-----------------Inner loop Iteration 3 Done-----------------#
#> #-----------------Inner loop Iteration 4 Done-----------------#
#> #-----------------Inner loop Iteration 5 Done-----------------#
#> #-----------------Inner loop Iteration 6 Done-----------------#
#> #-----------------Inner loop Iteration 7 Done-----------------#
#> Difference between alpha_old and alpha_new: 5.693111 
#> Start estimating alpha:
#> #-----------------Inner loop Iteration 1 Done-----------------#
#> #-----------------Inner loop Iteration 2 Done-----------------#
#> #-----------------Inner loop Iteration 3 Done-----------------#
#> #-----------------Inner loop Iteration 4 Done-----------------#
#> #-----------------Inner loop Iteration 5 Done-----------------#
#> #-----------------Inner loop Iteration 6 Done-----------------#
#> #-----------------Inner loop Iteration 7 Done-----------------#
#> Difference between alpha_old and alpha_new: 2.281027 
#> ...
#> Done!
```

To view the penalty parameters estimated by `xtune()`

``` r
xtune.fit$penalty.vector[1:5]
#> [1] 0.005381686 0.005381686 0.015186170 0.015186170 0.052245694
```

The `coef` and `predict` functions can be used to extract beta
coefficient estimates and predict response on new data.

``` r
coef_xtune(xtune.fit)[1:5]
#> [1]  0.07943206  2.08369420 -1.95701978  0.86767453 -1.31125776
predict_xtune(xtune.fit, example$X)[1:5]
#> Observation_1 Observation_2 Observation_3 Observation_4 Observation_5 
#>     -2.573221     -2.915913     -5.589512      2.196957      1.684783
```

More details and examples are also described in the vignettes to further
illustrate the usage and syntax of this package.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# mxsem

<!-- badges: start -->
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mxsem)](https://cranlogs.r-pkg.org/badges/grand-total/mxsem)
<!-- badges: end -->

**mxsem** provides a **lavaan**-like (Rosseel, 2012) syntax to implement
structural equation models (SEM) with **OpenMx** (Boker et al., 2011).
The objective is to simplify fitting basic SEM with **OpenMx**, while
also unlocking some very useful advanced features. For instance,
**mxsem** allows for parameter transformations and definition variables.
However, **mxsem** is intentionally incomplete in order to focus on
simplicity. The main function (`mxsem()`) is similar to **lavaan**’s
`sem()`-function in that it tries to set up parts of the model
automatically (e.g., adding variances automatically or scaling the
latent variables automatically).

> **Warning**: The syntax and settings of **mxsem** may differ from
> **lavaan** in some cases. See `vignette("Syntax", package = "mxsem")`
> for more details on the syntax and the default arguments.

## Alternatives

**mxsem** is not the first package providing a **lavaan**-like syntax
for **OpenMx**. You will find similar functions in the following
packages:

- [**metaSEM**](https://github.com/mikewlcheung/metasem) (Cheung, 2015)
  provides a `lavaan2RAM` function that can be combined with the
  `create.mxModel` function. This combination offers more features than
  **mxsem**. For instance, constraints of the form `a < b` are
  supported. In **mxsem** such constraints require algebras (e.g.,
  `!diff; a := b - exp(diff)`).
- [**umx**](https://github.com/tbates/umx) (Bates et al., 2019) provides
  the `umxRAM` and `umxLav2RAM` functions that can parse single
  **lavaan**-style statements (e.g., `eta =~ y1 + y2 + y3`) or an entire
  **lavaan** models to **OpenMx** models.
- [**tidySEM**](https://github.com/cjvanlissa/tidySEM) (van Lissa, 2023)
  provides the `as_ram` function to translate **lavaan** syntax to
  **OpenMx** and also implements a unified syntax to specify both,
  **lavaan** and **OpenMx** models. Additionally, it works well with the
  **tidyverse**.
- [**ezMx**](https://github.com/OpenMx/ezMx) (Bates, et al. 2014)
  simplifies fitting SEM with **OpenMx** and also provides a translation
  of **lavaan** models to **OpenMx** with the `lavaan.to.OpenMx`
  function.

Because **mxsem** implements the syntax parser from scratch, it can
extend the **lavaan** syntax to account for specific **OpenMx**
features. This enables [implicit transformations](#transformations) with
curly braces.

## Citation

Cite **OpenMx** (Boker et al., 2011) for the modeling and **lavaan** for
the syntax (Rosseel, 2012). To cite **mxsem**, check
`citation("mxsem")`.

## Installation

**mxsem** is available from CRAN:

``` r
install.packages("mxsem")
```

The newest version of the package can be installed from GitHub using the
following commands in R:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("jhorzek/mxsem", 
                         ref = "main")
```

Because **mxsem** uses Rcpp, you will need a compiler for C++ (e.g., by
installing Rtools on Windows, Xcode on Mac and build-essential on
linux).

## Example

The following example is directly adapted from **lavaan**:

``` r
library(mxsem)
model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

mxsem(model = model,
      data  = OpenMx::Bollen) |>
  mxTryHard() |>
  summary()
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled2 
    #>  
    #> free parameters:
    #>           name matrix   row   col   Estimate  Std.Error A lbound ubound
    #> 1     ind60→x2      A    x2 ind60 2.17951969 0.13890273                
    #> 2     ind60→x3      A    x3 ind60 1.81811337 0.15211751                
    #> 3  ind60→dem60      A dem60 ind60 1.44904273 0.38544855                
    #> 4  ind60→dem65      A dem65 ind60 0.60449854 0.24058577                
    #> 5           a1      A    y2 dem60 1.29147133 0.19273354                
    #> 6            b      A    y3 dem60 1.17388110 0.11991187                
    #> 7           c1      A    y4 dem60 1.30214922 0.15716825                
    #> 8  dem60→dem65      A dem65 dem60 0.89849281 0.09209863                
    #> 9           a2      A    y6 dem65 1.13247238 0.15405101                
    #> 10          c2      A    y8 dem65 1.20957807 0.14443543                
    #> 11       y1↔y1      S    y1    y1 1.91458549 0.46801012    1e-06       
    #> 12       y2↔y2      S    y2    y2 7.40452888 1.34562916    1e-06       
    #> 13       y3↔y3      S    y3    y3 4.99236808 0.96375021    1e-06       
    #> 14       y2↔y4      S    y2    y4 1.32053478 0.69918534                
    #> 15       y4↔y4      S    y4    y4 3.15117584 0.75521995    1e-06       
    #> 16       y2↔y6      S    y2    y6 2.17541773 0.72882998                
    #> 17       y6↔y6      S    y6    y6 5.01524082 0.89773033    1e-06       
    #> 18       x1↔x1      S    x1    x1 0.08135247 0.01970040    1e-06       
    #> 19       x2↔x2      S    x2    x2 0.12052866 0.06990806    1e-06       
    #> 20       x3↔x3      S    x3    x3 0.46670049 0.08911867    1e-06       
    #> 21       y1↔y5      S    y1    y5 0.59097044 0.36679629                
    #> 22       y5↔y5      S    y5    y5 2.30230244 0.48307628    1e-06       
    #> 23       y3↔y7      S    y3    y7 0.73134993 0.62154873                
    #> 24       y7↔y7      S    y7    y7 3.52500940 0.73477059    1e-06       
    #> 25       y4↔y8      S    y4    y8 0.35317926 0.45974116                
    #> 26       y6↔y8      S    y6    y8 1.41224936 0.57574745                
    #> 27       y8↔y8      S    y8    y8 3.32140113 0.71106484    1e-06       
    #> 28 ind60↔ind60      S ind60 ind60 0.44863429 0.08674943    1e-06       
    #> 29 dem60↔dem60      S dem60 dem60 3.71721943 0.89611392    1e-06       
    #> 30 dem65↔dem65      S dem65 dem65 0.16448130 0.23830932    1e-06       
    #> 31      one→y1      M     1    y1 5.46466715 0.29605013                
    #> 32      one→y2      M     1    y2 4.25644263 0.44981119                
    #> 33      one→y3      M     1    y3 6.56311026 0.39007812                
    #> 34      one→y4      M     1    y4 4.45253310 0.38385079                
    #> 35      one→y6      M     1    y6 2.97807431 0.38583489                
    #> 36      one→x1      M     1    x1 5.05438392 0.08406042                
    #> 37      one→x2      M     1    x2 4.79219470 0.17326513                
    #> 38      one→x3      M     1    x3 3.55768986 0.16122804                
    #> 39      one→y5      M     1    y5 5.13625262 0.30762959                
    #> 40      one→y7      M     1    y7 6.19626397 0.36757001                
    #> 41      one→y8      M     1    y8 4.04339020 0.37125831                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             41                    784              3096.945
    #>    Saturated:             77                    748                    NA
    #> Independence:             22                    803                    NA
    #> Number of observations/statistics: 75/825
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:      1528.9445               3178.945                 3283.308
    #> BIC:      -287.9662               3273.962                 3144.740
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:09 
    #> Wall clock time: 0.2771828 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)

</details>

## Adding bounds

Lower and upper bounds can be added to any of the parameters in the
model. The following demonstrates bounds on a loading:

``` r
library(mxsem)
model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8
     
  # lower bound on a1
     a1 > 0
  # upper bound on a2
     a2 < 10.123
'

mxsem(model = model,
      data  = OpenMx::Bollen, 
      # use latent variances to scale the model
      scale_loadings = FALSE, 
      scale_latent_variances = TRUE) |>
  mxTryHard() |>
  summary()
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled4 
    #>  
    #> free parameters:
    #>           name matrix   row   col    Estimate  Std.Error A lbound ubound
    #> 1     ind60→x1      A    x1 ind60  0.66666815 0.06314203                
    #> 2     ind60→x2      A    x2 ind60  1.45423326 0.12394721                
    #> 3     ind60→x3      A    x3 ind60  1.21231509 0.12551275                
    #> 4     dem60→y1      A    y1 dem60 -2.24692386 0.16292809                
    #> 5           a1      A    y2 dem60  0.00000000         NA !     0!       
    #> 6            b      A    y3 dem60 -2.55892738 0.17648475                
    #> 7           c1      A    y4 dem60 -2.80348655 0.23710803                
    #> 8     dem65→y5      A    y5 dem65 -2.10524882 0.19254708                
    #> 9           a2      A    y6 dem65 -2.55243469 0.27839775          10.123
    #> 10          c2      A    y8 dem65 -2.71463830 0.22625659                
    #> 11       x1↔x1      S    x1    x1  0.08172352 0.01983068    1e-06       
    #> 12       x2↔x2      S    x2    x2  0.11872315 0.07051504    1e-06       
    #> 13       x3↔x3      S    x3    x3  0.46734771 0.08933374    1e-06       
    #> 14       y1↔y1      S    y1    y1  1.76022749 0.40668814    1e-06       
    #> 15       y2↔y2      S    y2    y2 15.37208533         NA    1e-06       
    #> 16       y3↔y3      S    y3    y3  4.97654067 0.92946421    1e-06       
    #> 17       y4↔y4      S    y4    y4  3.24412436 0.68674912    1e-06       
    #> 18       y5↔y5      S    y5    y5  2.28325758 0.45047306    1e-06       
    #> 19       y6↔y6      S    y6    y6  4.68028674 0.88272249    1e-06       
    #> 20       y7↔y7      S    y7    y7  3.46858346 0.67373234    1e-06       
    #> 21       y8↔y8      S    y8    y8  2.99198432 0.64558867    1e-06       
    #> 22 ind60↔dem60      S ind60 dem60 -0.45702984 0.10093202                
    #> 23 ind60↔dem65      S ind60 dem65 -0.55364625 0.08756465                
    #> 24 dem60↔dem65      S dem60 dem65  0.97486174 0.03020539                
    #> 25      one→x1      M     1    x1  5.05438384 0.08376190                
    #> 26      one→x2      M     1    x2  4.79219463 0.17257565                
    #> 27      one→x3      M     1    x3  3.55768979 0.16071368                
    #> 28      one→y1      M     1    y1  5.46466667 0.30132622                
    #> 29      one→y2      M     1    y2  4.25644288 0.45272065                
    #> 30      one→y3      M     1    y3  6.56311025 0.39201794                
    #> 31      one→y4      M     1    y4  4.45253304 0.38479483                
    #> 32      one→y5      M     1    y5  5.13625192 0.29924780                
    #> 33      one→y6      M     1    y6  2.97807408 0.38637444                
    #> 34      one→y7      M     1    y7  6.19626389 0.36547691                
    #> 35      one→y8      M     1    y8  4.04338968 0.37171110                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             35                    790              3187.076
    #>    Saturated:             77                    748                    NA
    #> Independence:             22                    803                    NA
    #> Number of observations/statistics: 75/825
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:      1607.0759               3257.076                 3321.691
    #> BIC:      -223.7397               3338.188                 3227.877
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:11 
    #> Wall clock time: 0.0535531 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)

</details>

**mxsem** adds lower bounds to any of the variances by default. To
remove these lower bounds, set `lbound_variances = FALSE` when calling
`mxsem()`.

## Definition Variables

Definition variables are, for instance, used in latent growth curve
models when the time intervals between observations are different for
the subjects in the data set. Here is an example, where the variables
`t_1`-`t_5` indicate the person-specific times of observation:

``` r
library(mxsem)
set.seed(3489)
dataset <- simulate_latent_growth_curve(N = 100)
head(dataset)
#>             y1       y2       y3        y4         y5 t_1       t_2      t_3
#> [1,] 1.2817946 5.159870 7.178191  8.950046 11.4822306   0 1.5792322 2.304777
#> [2,] 1.1796379 3.588279 5.927219  8.381157 10.4640667   0 1.6701976 3.530621
#> [3,] 0.2196010 0.763441 2.499564  3.672995  4.4505868   0 0.6452145 2.512730
#> [4,] 0.5688185 1.440709 1.523483  1.416965  1.9674847   0 1.7171826 3.245522
#> [5,] 3.4928919 2.620657 1.753159  1.080701 -0.4436508   0 1.4055839 2.024568
#> [6,] 0.3520293 5.126854 7.390669 10.721785 12.6363472   0 1.5249299 2.400432
#>           t_4      t_5
#> [1,] 3.120797 4.217403
#> [2,] 5.004695 6.408367
#> [3,] 3.761189 4.729461
#> [4,] 4.331997 6.145424
#> [5,] 3.570780 5.517224
#> [6,] 3.654230 4.222212
```

In **OpenMx**, parameters can be set to the values found in the columns
of the data set with the `data.` prefix. This is used in the following
to fix the loadings of a latent slope variable on the observations to
the times recorded in `t_1`-`t_5`:

``` r
library(mxsem)
model <- "
  # specify latent intercept
     I =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  # specify latent slope
     S =~ data.t_1 * y1 + data.t_2 * y2 + data.t_3 * y3 + data.t_4 * y4 + data.t_5 * y5
    
  # specify means of latent intercept and slope
     I ~ int*1
     S ~ slp*1
  
  # set intercepts of manifest variables to zero
     y1 ~ 0*1; y2 ~ 0*1; y3 ~ 0*1; y4 ~ 0*1; y5 ~ 0*1;
  "

mxsem(model = model,
      data  = dataset) |>
  mxTryHard() |>
  summary()
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled6 
    #>  
    #> free parameters:
    #>     name matrix row col    Estimate   Std.Error A lbound ubound
    #> 1  y1↔y1      S  y1  y1  0.02578029 0.014488264       0!       
    #> 2  y2↔y2      S  y2  y2  0.04010524 0.008389750       0!       
    #> 3  y3↔y3      S  y3  y3  0.04008174 0.006984929       0!       
    #> 4  y4↔y4      S  y4  y4  0.01752572 0.006930941       0!       
    #> 5  y5↔y5      S  y5  y5  0.05936966 0.016067358    1e-06       
    #> 6    I↔I      S   I   I  1.02593601 0.148058876    1e-06       
    #> 7    I↔S      S   I   S -0.14724742 0.110045416                
    #> 8    S↔S      S   S   S  1.13051032 0.160486387    1e-06       
    #> 9    int      M   1   I  0.93112322 0.102209199                
    #> 10   slp      M   1   S  0.48442624 0.106475815                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             10                     10              841.2609
    #>    Saturated:             20                      0                    NA
    #> Independence:             10                     10                    NA
    #> Number of observations/statistics: 100/20
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:       821.2609               861.2609                 863.7328
    #> BIC:       795.2092               887.3126                 855.7301
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:12 
    #> Wall clock time: 0.2313709 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)

</details>

## Transformations

Sometimes, one may want to express one parameter as a function of other
parameters. In moderated non-linear factor analysis, for example, model
parameters are often expressed in terms of a covariate k. For instance,
the effect $a$ of $\xi$ on $\eta$ could be expressed as
$a = a_0 + a_1\times k$.

``` r
library(mxsem)
set.seed(9820)
dataset <- simulate_moderated_nonlinear_factor_analysis(N = 100)
head(dataset)
#>              x1         x2         x3          y1         y2            y3 k
#> [1,] -1.2166034 -1.2374549 -1.3731943 -1.01018683 -0.8296293 -1.2300555484 0
#> [2,]  1.1911346  0.9971499  1.0226322  0.86048030  0.4509088  0.6052786392 1
#> [3,] -0.7777169 -0.4725291 -0.8507347 -1.09582848 -0.5035753 -0.8048378456 0
#> [4,]  1.0027847  1.2351709  0.6951317  0.94040287  0.6684979  0.6596891858 0
#> [5,]  0.4387896  0.3919877  0.3260557 -0.58188691 -0.3614349 -0.4901022121 0
#> [6,] -1.4951549 -0.8834637 -1.1715535  0.01173845 -0.4697865 -0.0006475256 0
```

**mxsem** currently supports two ways of specifying such
transformations. First, they can be specified explicitly. To this end,
the parameters $a_0$ and $a_1$ must fist be initialized with `!a0` and
`!a1`. Additionally, the transformation must be defined with
`a := a0 + a1*data.k`.

``` r
model <- "
  # loadings
     xi =~ x1 + x2 + x3
     eta =~ y1 + y2 + y3
  # regression
     eta ~ a*xi
  
  # we need two new parameters: a0 and a1. These are created as follows:
     !a0
     !a1
  # Now, we redefine a to be a0 + k*a1, where k is found in the data
     a := a0 + data.k*a1
"

fit_mx <- mxsem(model = model,
                data  = dataset) |>
  mxTryHard()

summary(fit_mx)

# get just the value for parameter a:
mxEval(expression = a, model = fit_mx)
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled20 
    #>  
    #> free parameters:
    #>       name         matrix row col    Estimate   Std.Error A lbound ubound
    #> 1    xi→x2              A  x2  xi  0.79157858 0.026246184                
    #> 2    xi→x3              A  x3  xi  0.89166108 0.027991673                
    #> 3   eta→y2              A  y2 eta  0.81610411 0.028977474                
    #> 4   eta→y3              A  y3 eta  0.90741889 0.027924346                
    #> 5    x1↔x1              S  x1  x1  0.04060244 0.011022344 !     0!       
    #> 6    x2↔x2              S  x2  x2  0.04519865 0.008621643 !     0!       
    #> 7    x3↔x3              S  x3  x3  0.04647166 0.010143724       0!       
    #> 8    y1↔y1              S  y1  y1  0.03388962 0.008495346 !     0!       
    #> 9    y2↔y2              S  y2  y2  0.04210945 0.007766691 !     0!       
    #> 10   y3↔y3              S  y3  y3  0.03107010 0.007268278       0!       
    #> 11   xi↔xi              S  xi  xi  1.07304552 0.157796861    1e-06       
    #> 12 eta↔eta              S eta eta  0.26127631 0.041232786    1e-06       
    #> 13  one→x1              M   1  x1 -0.14881030 0.105057193                
    #> 14  one→x2              M   1  x2 -0.10969677 0.084338898                
    #> 15  one→x3              M   1  x3 -0.15448454 0.094426293                
    #> 16  one→y1              M   1  y1 -0.05304659 0.089761149                
    #> 17  one→y2              M   1  y2 -0.13040871 0.074578868                
    #> 18  one→y3              M   1  y3 -0.05666275 0.081647174                
    #> 19      a0 new_parameters   1   1  0.78168092 0.069381896                
    #> 20      a1 new_parameters   1   2 -0.19334145 0.107742907                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             20                      7              475.3822
    #>    Saturated:             27                      0                    NA
    #> Independence:             12                     15                    NA
    #> Number of observations/statistics: 100/27
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:       461.3822               515.3822                 526.0151
    #> BIC:       443.1460               567.4856                 504.3206
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:12 
    #> Wall clock time: 0.04142094 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)
    #>           [,1]
    #> [1,] 0.7816809

</details>

Alternatively, the transformations can be defined implicitly by placing
the algebra in curly braces and directly inserting it in the syntax in
place of the parameter label. This is inspired by the approach in
**metaSEM** (Cheung, 2015).

``` r
model <- "
  # loadings
     xi =~ x1 + x2 + x3
     eta =~ y1 + y2 + y3
  # regression
     eta ~ {a0 + a1*data.k} * xi
"

mxsem(model = model,
      data  = dataset) |>
  mxTryHard() |>
  summary()
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled48 
    #>  
    #> free parameters:
    #>       name         matrix row col    Estimate   Std.Error A lbound ubound
    #> 1    xi→x2              A  x2  xi  0.79157858 0.026246184                
    #> 2    xi→x3              A  x3  xi  0.89166108 0.027991673                
    #> 3   eta→y2              A  y2 eta  0.81610411 0.028977474                
    #> 4   eta→y3              A  y3 eta  0.90741889 0.027924346                
    #> 5    x1↔x1              S  x1  x1  0.04060244 0.011022344 !     0!       
    #> 6    x2↔x2              S  x2  x2  0.04519865 0.008621643 !     0!       
    #> 7    x3↔x3              S  x3  x3  0.04647166 0.010143724       0!       
    #> 8    y1↔y1              S  y1  y1  0.03388962 0.008495346 !     0!       
    #> 9    y2↔y2              S  y2  y2  0.04210945 0.007766691 !     0!       
    #> 10   y3↔y3              S  y3  y3  0.03107010 0.007268278       0!       
    #> 11   xi↔xi              S  xi  xi  1.07304552 0.157796861    1e-06       
    #> 12 eta↔eta              S eta eta  0.26127631 0.041232786    1e-06       
    #> 13  one→x1              M   1  x1 -0.14881030 0.105057193                
    #> 14  one→x2              M   1  x2 -0.10969677 0.084338898                
    #> 15  one→x3              M   1  x3 -0.15448454 0.094426293                
    #> 16  one→y1              M   1  y1 -0.05304659 0.089761149                
    #> 17  one→y2              M   1  y2 -0.13040871 0.074578868                
    #> 18  one→y3              M   1  y3 -0.05666275 0.081647174                
    #> 19      a0 new_parameters   1   1  0.78168092 0.069381896                
    #> 20      a1 new_parameters   1   2 -0.19334145 0.107742907                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             20                      7              475.3822
    #>    Saturated:             27                      0                    NA
    #> Independence:             12                     15                    NA
    #> Number of observations/statistics: 100/27
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:       461.3822               515.3822                 526.0151
    #> BIC:       443.1460               567.4856                 504.3206
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:13 
    #> Wall clock time: 0.0401659 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)

</details>

You can also provide custom names for the algebra results:

``` r
model <- "
  # loadings
     xi  =~ x1 + x2 + x3
     eta =~ y1 + y2 + y3
  # regression
     eta ~ {a := a0 + a1*data.k} * xi
"

fit_mx <- mxsem(model = model,
                data  = dataset) |>
  mxTryHard()

summary(fit_mx)

# get just the value for parameter a:
mxEval(expression = a, 
       model      = fit_mx)
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled76 
    #>  
    #> free parameters:
    #>       name         matrix row col    Estimate   Std.Error A lbound ubound
    #> 1    xi→x2              A  x2  xi  0.79157858 0.026246184                
    #> 2    xi→x3              A  x3  xi  0.89166108 0.027991673                
    #> 3   eta→y2              A  y2 eta  0.81610411 0.028977474                
    #> 4   eta→y3              A  y3 eta  0.90741889 0.027924346                
    #> 5    x1↔x1              S  x1  x1  0.04060244 0.011022344 !     0!       
    #> 6    x2↔x2              S  x2  x2  0.04519865 0.008621643 !     0!       
    #> 7    x3↔x3              S  x3  x3  0.04647166 0.010143724       0!       
    #> 8    y1↔y1              S  y1  y1  0.03388962 0.008495346 !     0!       
    #> 9    y2↔y2              S  y2  y2  0.04210945 0.007766691 !     0!       
    #> 10   y3↔y3              S  y3  y3  0.03107010 0.007268278       0!       
    #> 11   xi↔xi              S  xi  xi  1.07304552 0.157796861    1e-06       
    #> 12 eta↔eta              S eta eta  0.26127631 0.041232786    1e-06       
    #> 13  one→x1              M   1  x1 -0.14881030 0.105057193                
    #> 14  one→x2              M   1  x2 -0.10969677 0.084338898                
    #> 15  one→x3              M   1  x3 -0.15448454 0.094426293                
    #> 16  one→y1              M   1  y1 -0.05304659 0.089761149                
    #> 17  one→y2              M   1  y2 -0.13040871 0.074578868                
    #> 18  one→y3              M   1  y3 -0.05666275 0.081647174                
    #> 19      a0 new_parameters   1   1  0.78168092 0.069381896                
    #> 20      a1 new_parameters   1   2 -0.19334145 0.107742907                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             20                      7              475.3822
    #>    Saturated:             27                      0                    NA
    #> Independence:             12                     15                    NA
    #> Number of observations/statistics: 100/27
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:       461.3822               515.3822                 526.0151
    #> BIC:       443.1460               567.4856                 504.3206
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:13 
    #> Wall clock time: 0.03462195 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)
    #>           [,1]
    #> [1,] 0.7816809

</details>

## Adapting the Model

`mxsem` returns an `mxModel` object that can be adapted further by users
familiar with **OpenMx**.

## Trouble shooting

Sometimes things may go wrong. One way to figure out where **mxsem**
messed up is to look at the parameter table generated internally. This
parameter table is not returned by default. See
`vignette("create_parameter_table", package = "mxsem")` for more
details.

Another point of failure are the default labels used by **mxsem** to
indicate directed and undirected effects. These are based on unicode
characters. If you see parameter labels similar to `"eta\u2192y1"` in
your output, this indicates that your editor cannot display unicode
characters. In this case, you can customize the labels as follows:

``` r
library(mxsem)
model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8
'

mxsem(model      = model,
      data       = OpenMx::Bollen, 
      directed   = "_TO_", 
      undirected = "_WITH_") |>
  mxTryHard() |>
  summary()
```

<details>
<summary>
Show summary
</summary>

    #> Summary of untitled90 
    #>  
    #> free parameters:
    #>                name matrix   row   col   Estimate  Std.Error A lbound ubound
    #> 1       ind60_TO_x2      A    x2 ind60 2.18115661 0.13928298                
    #> 2       ind60_TO_x3      A    x3 ind60 1.81852856 0.15228300                
    #> 3                a1      A    y2 dem60 1.40364291 0.18389435                
    #> 4                 b      A    y3 dem60 1.17009128 0.10871690                
    #> 5                c1      A    y4 dem60 1.34853557 0.14637355                
    #> 6                a2      A    y6 dem65 1.20074397 0.14854212                
    #> 7                c2      A    y8 dem65 1.25031700 0.13637267                
    #> 8        x1_WITH_x1      S    x1    x1 0.08169543 0.01979127    1e-06       
    #> 9        x2_WITH_x2      S    x2    x2 0.11895803 0.07035983    1e-06       
    #> 10       x3_WITH_x3      S    x3    x3 0.46715672 0.08931237    1e-06       
    #> 11       y1_WITH_y1      S    y1    y1 1.96250172 0.40671919    1e-06       
    #> 12       y2_WITH_y2      S    y2    y2 6.49921810 1.20252769    1e-06       
    #> 13       y3_WITH_y3      S    y3    y3 5.32558348 0.95890941    1e-06       
    #> 14       y4_WITH_y4      S    y4    y4 2.87950381 0.63659586    1e-06       
    #> 15       y5_WITH_y5      S    y5    y5 2.37087652 0.45484953    1e-06       
    #> 16       y6_WITH_y6      S    y6    y6 4.37312946 0.82257502    1e-06       
    #> 17       y7_WITH_y7      S    y7    y7 3.56698909 0.68166517    1e-06       
    #> 18       y8_WITH_y8      S    y8    y8 2.96556937 0.62436037    1e-06       
    #> 19 ind60_WITH_ind60      S ind60 ind60 0.44829168 0.08674258    1e-06       
    #> 20 ind60_WITH_dem60      S ind60 dem60 0.63807551 0.19911416                
    #> 21 dem60_WITH_dem60      S dem60 dem60 4.50351022 1.00587173    1e-06       
    #> 22 ind60_WITH_dem65      S ind60 dem65 0.81413737 0.21686414                
    #> 23 dem60_WITH_dem65      S dem60 dem65 4.52637094 0.93201111                
    #> 24 dem65_WITH_dem65      S dem65 dem65 4.75141774 1.04788975    1e-06       
    #> 25        one_TO_x1      M     1    x1 5.05438376 0.08406360                
    #> 26        one_TO_x2      M     1    x2 4.79219456 0.17327235                
    #> 27        one_TO_x3      M     1    x3 3.55768963 0.16123393                
    #> 28        one_TO_y1      M     1    y1 5.46466632 0.29362329                
    #> 29        one_TO_y2      M     1    y2 4.25644402 0.45272824                
    #> 30        one_TO_y3      M     1    y3 6.56311128 0.39143132                
    #> 31        one_TO_y4      M     1    y4 4.45253318 0.38417804                
    #> 32        one_TO_y5      M     1    y5 5.13625219 0.30816376                
    #> 33        one_TO_y6      M     1    y6 2.97807359 0.38684837                
    #> 34        one_TO_y7      M     1    y7 6.19626378 0.36646597                
    #> 35        one_TO_y8      M     1    y8 4.04339038 0.37226405                
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             35                    790              3131.168
    #>    Saturated:             77                    748                    NA
    #> Independence:             22                    803                    NA
    #> Number of observations/statistics: 75/825
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:      1551.1683               3201.168                 3265.784
    #> BIC:      -279.6473               3282.280                 3171.970
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2023-08-11 09:04:13 
    #> Wall clock time: 0.08833313 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.21.8 
    #> Need help?  See help(mxSummary)

</details>

## References

- Bates, T. C., Maes, H., & Neale, M. C. (2019). umx: Twin and
  Path-Based Structural Equation Modeling in R. Twin Research and Human
  Genetics, 22(1), 27–41. <https://doi.org/10.1017/thg.2019.2>
- Bates, T. C., Prindle, J. J. (2014). ezMx.
  <https://github.com/OpenMx/ezMx>
- Boker, S. M., Neale, M., Maes, H., Wilde, M., Spiegel, M., Brick, T.,
  Spies, J., Estabrook, R., Kenny, S., Bates, T., Mehta, P., & Fox, J.
  (2011). OpenMx: An Open Source Extended Structural Equation Modeling
  Framework. Psychometrika, 76(2), 306–317.
  <https://doi.org/10.1007/s11336-010-9200-6>
- Cheung, M. W.-L. (2015). metaSEM: An R package for meta-analysis using
  structural equation modeling. Frontiers in Psychology, 5.
  <https://doi.org/10.3389/fpsyg.2014.01521>
- Rosseel, Y. (2012). lavaan: An R package for structural equation
  modeling. Journal of Statistical Software, 48(2), 1–36.
  <https://doi.org/10.18637/jss.v048.i02>
- van Lissa, C. J. (2023). tidySEM: Tidy Structural Equation Modeling. R
  package version 0.2.4, <https://cjvanlissa.github.io/tidySEM/>.

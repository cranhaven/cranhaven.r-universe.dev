
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dmlalg

The *dmlalg* package contains implementations of double machine learning
algorithms in *R*.

## Installation

You can install the released version of dmlalg from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dmlalg")
```

## Partially linear models with confounding variables

The aim of this first set of functions it to perform inference for the
linear parameter in partially linear models with confounding variables.
The standard DML estimator of the linear parameter has a two-stage least
squares interpretation, which can lead to a large variance and overwide
confidence intervals. We apply regularization to reduce the variance of
the estimator, which produces narrower confidence intervals that remain
approximately valid. Nuisance terms can be flexibly estimated with
machine learning algorithms.

This algorithm is described in Emmenegger and Bühlmann (2021b) and
implemented in the function *regsdml*.

-   *regsdml* computes the estimate of the linear parameter in a
    partially linear model with endogenous variables with regularized
    and standard double machine learning methods.
-   *summary* method for objects fitted with *regsdml*
-   *confint* method for objects fitted with *regsdml*
-   *coef* method for objects fitted with *regsdml*
-   *vcov* method for objects fitted with *regsdml*
-   *print* method for objects fitted with *regsdml*

### Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dmlalg)

## Generate some data:
set.seed(19)
# true linear parameter
beta0 <- 1
n <- 40
# observed confounder
w <- pi * runif(n, -1, 1)
# instrument
a <- 3 * tanh(2 * w) + rnorm(n, 0, 1)
# unobserved confounder
h <- 2 * sin(w) + rnorm(n, 0, 1)
# linear covariate
x <- -1 * abs(a) - h - 2 * tanh(w) + rnorm(n, 0, 1)
# response
y <- beta0 * x - 3 * cos(pi * 0.25 * h) + 0.5 * w ^ 2 + rnorm(n, 0, 1)

## Estimate the linear coefficient from x to y
## (The parameters are chosen small enough to make estimation fast):
## Caveat: A spline estimator is extrapolated, which raises a warning message. 
## Extrapolation lies in the nature of our method. To omit the warning message
## resulting from the spline estimator, another estimator may be used. 
fit <- regsdml(a, w, x, y,
               gamma = exp(seq(-4, 1, length.out = 4)),
               S = 3,
               do_regDML_all_gamma = TRUE,
               cond_method = c("forest",  # for E[A|W]
                               "spline",  # for E[X|W]
                               "spline"), # for E[Y|W]
               params = list(list(ntree = 1), NULL, NULL))
#> Warning in print_W_E_fun(errors, warningMsgs): 
#> Warning messages:
#> some 'x' values beyond boundary knots may cause ill-conditioned bases
## parm = c(2, 3) prints an additional summary for the 2nd and 3rd gamma-values
summary(fit, parm = c(2, 3),
        correlation = TRUE,
        print_gamma = TRUE) 
#> 
#> Coefficients :
#> regsDML (2.72e+00) :
#>    Estimate Std. Error  z value     Pr(>|z|)
#> b1 0.910255  0.1731559 5.256852 1.465421e-07
#> 
#> regDMLall (9.70e-02) :
#>     Estimate Std. Error  z value     Pr(>|z|)
#> b1 0.7986392  0.1514027 5.274935 1.328031e-07
#> 
#> regDMLall (5.13e-01) :
#>    Estimate Std. Error  z value     Pr(>|z|)
#> b1 0.846176  0.1651298 5.124308 2.986318e-07
#> 
#> 
#> Variance-covariance matrices :
#> regsDML (2.72e+00) :
#>            b1
#> b1 0.02998297
#> 
#> regDMLall (9.70e-02) :
#>            b1
#> b1 0.02292277
#> 
#> regDMLall (5.13e-01) :
#>            b1
#> b1 0.02726785
confint(fit, parm = c(2, 3),
        print_gamma = TRUE) 
#> 
#> Two-sided confidence intervals at level 0.95 : 
#> 
#> regsDML (2.72e+00) :
#>        2.5 %   97.5 %
#> b1 0.5708757 1.249634
#> 
#> regDMLall (9.70e-02) :
#>        2.5 %   97.5 %
#> b1 0.5018955 1.095383
#> 
#> regDMLall (5.13e-01) :
#>        2.5 %   97.5 %
#> b1 0.5225276 1.169824
coef(fit) # coefficients
#>     regsDML
#> b1 0.910255
vcov(fit) # variance-covariance matrices
#> 
#> Variance-covariance matrices :
#> regsDML :
#>            b1
#> b1 0.02998297

## Alternatively, provide the data in a single data frame
## (see also caveat above):
data <- data.frame(a = a, w = w, x = x, y = y)
fit <- regsdml(a = "a", w = "w", x = "x", y = "y", data = data,
               gamma = exp(seq(-4, 1, length.out = 4)),
               S = 3)
#> Warning in print_W_E_fun(errors, warningMsgs): 
#> Warning messages:
#> some 'x' values beyond boundary knots may cause ill-conditioned bases

## With more realistic parameter choices:
if (FALSE) {
  fit <- regsdml(a, w, x, y,
                 cond_method = c("forest",  # for E[A|W]
                                 "spline",  # for E[X|W]
                                 "spline")) # for E[Y|W]
  summary(fit)
  confint(fit)

  ## Alternatively, provide the data in a single data frame:
  ## (see also caveat above):
  data <- data.frame(a = a, w = w, x = x, y = y)
  fit <- regsdml(a = "a", w = "w", x = "x", y = "y", data = data)
}
```

## Estimating linear coefficients in partially linear mixed-effects models with repeated measurements using double machine learning

The aim of this second set of functions is to estimate and perform
inference for the linear coefficient in a partially linear mixed-effects
model with DML. Machine learning algorithms allows us to incorporate
more complex interaction structures and high-dimensional variables.

This algorithm is described in Emmenegger and Bühlmann (2021a) and
implemented in the function *mmdml*.

-   *mmdml* computes the estimate of the linear parameter in a partially
    linear mixed-effects model using double machine learning methods.
-   *confint* method for objects fitted with *mmdml*
-   *fixef* method for objects fitted with *mmdml*
-   *print* method for objects fitted with *mmdml*
-   *ranef* method for objects fitted with *mmdml*
-   *residuals* method for objects fitted with *mmdml*
-   *sigma* method for objects fitted with *mmdml*
-   *summary* method for objects fitted with *mmdml*
-   *vcov* method for objects fitted with *mmdml*
-   *VarCorr* method for objects fitted with *mmdml*

### Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dmlalg)

## generate data
RNGkind("L'Ecuyer-CMRG")
set.seed(19)
data1 <- example_data_mmdml(beta0 = 0.2)
data2 <- example_data_mmdml(beta0 = c(0.2, 0.2))

## fit models
## Caveat: Warning messages are displayed because the small number of
## observations results in a singular random effects model
fit1 <-
  mmdml(w = c("w1", "w2", "w3"), x = "x1", y = "resp", z = c("id", "cask"),
        data = data1, z_formula = "(1|id) + (1|cask:id)", group = "id", S = 3)
#> Warning in mmdml(w = c("w1", "w2", "w3"), x = "x1", y = "resp", z = c("id", : 
#> Warning messages:
#> boundary (singular) fit: see ?isSingular

fit2 <-
  mmdml(w = c("w1", "w2", "w3"), x = c("x1", "x2"), y = "resp", z = c("id", "cask"),
        data = data2, z_formula = "(1|id) + (1|cask:id)", group = "id", S = 3)
#> Warning in mmdml(w = c("w1", "w2", "w3"), x = c("x1", "x2"), y = "resp", : 
#> Warning messages:
#> boundary (singular) fit: see ?isSingular

## apply methods
confint(fit2)
#>           2.5%     97.5%
#> x1 -0.03415795 0.3480103
#> x2  0.15930098 0.3893938
fixef(fit2)
#>        x1        x2 
#> 0.1569261 0.2743474
print(fit2)
#> Semiparametric mixed model fit by maximum likelihood ['mmdml']
#> Random effects:
#>  Groups   Name        Std.Dev. 
#>  cask:id  (Intercept) 1.908e-06
#>  id       (Intercept) 1.107e-01
#>  Residual             2.756e-01
#> Number of obs: 46, groups:  cask:id, 20; id, 10
#> Fixed Effects:
#>     x1      x2  
#> 0.1569  0.2743  
#> optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings
ranef(fit2)
#> $`cask:id`
#>        (Intercept)
#> 1:1  -0.0023043914
#> 1:10 -0.0050894736
#> 1:2   0.0024571669
#> 1:3   0.0007708872
#> 1:4  -0.0012417525
#> 1:5   0.0029010344
#> 1:6   0.0012307712
#> 1:7  -0.0028418387
#> 1:8  -0.0015618712
#> 1:9  -0.0048037635
#> 2:1   0.0100768089
#> 2:10 -0.0031560819
#> 2:2  -0.0033427429
#> 2:3  -0.0044928425
#> 2:4  -0.0054049237
#> 2:5  -0.0021157461
#> 2:6  -0.0023122280
#> 2:7   0.0038004751
#> 2:8   0.0148222090
#> 2:9   0.0026385335
#> 
#> $id
#>     (Intercept)
#> 1   0.100740957
#> 10 -0.124434023
#> 2  -0.036918731
#> 3  -0.030230821
#> 4  -0.081051109
#> 5   0.018887512
#> 6  -0.006711504
#> 7   0.025545300
#> 8   0.235373382
#> 9  -0.020965920
residuals(fit2)
#> [[1]]
#>  [1] -0.1311195998  0.5733692328  0.1398125051 -0.0705463911 -0.1196552839
#>  [6] -0.0354080600  0.6205378654 -0.1057642425 -0.4355021749 -0.0633888854
#> [11]  0.0070044016 -0.1777683530 -0.0214893719  0.0052358066  0.1594839987
#> [16] -0.2353753755 -0.2216497409 -0.1034882421  0.0175984650 -0.0388497525
#> [21]  0.4636325671 -0.2597143034  0.3528825573 -0.4739722035  0.0007039458
#> [26]  0.0700307380 -0.1315655000 -0.1617002846  0.2162465843  0.0934414339
#> [31] -0.0480554546 -0.1342562672 -0.2349311153  0.4021334289  0.6761796261
#> [36]  0.3514207835 -0.0918140917 -0.2144924370 -0.3184478283 -0.2704273590
#> [41] -0.1953366308  0.7209607369 -0.1050645053 -0.2895904461 -0.2737160112
#> [46]  0.0941353224
#> 
#> [[2]]
#>  [1]  0.066708484  0.381936532  0.083961541 -0.244607521 -0.116940987
#>  [6] -0.015024540  0.605540877  0.128223071 -0.186010749 -0.119432458
#> [11] -0.101885530 -0.153724682 -0.214346785 -0.126400135  0.090522034
#> [16] -0.103818112 -0.170763502 -0.102507199  0.047067741 -0.026325741
#> [21]  0.472126666 -0.231575911  0.324749223 -0.423215690 -0.013990681
#> [26]  0.066537726 -0.086954711 -0.025470109  0.227756255  0.224213587
#> [31] -0.070700603  0.081484834 -0.226268534  0.615553468  0.723110460
#> [36]  0.333538915 -0.076459138 -0.198241935 -0.245660371 -0.366166157
#> [41] -0.142947352  0.677671159 -0.047532882 -0.305555800 -0.379445954
#> [46]  0.007159723
#> 
#> [[3]]
#>  [1]  0.09685066  0.34629638  0.09582384 -0.27150981 -0.12653048 -0.02387543
#>  [7]  0.62488259  0.12730531 -0.19466784 -0.12227940 -0.07635676 -0.16470188
#> [13] -0.20223445 -0.11432450  0.13844295 -0.12234863 -0.18662475 -0.09034621
#> [19]  0.07330126 -0.02704395  0.51049151 -0.23716208  0.36116367 -0.42669942
#> [25] -0.02948530  0.10139429 -0.06858354 -0.03611104  0.19153360  0.21971922
#> [31] -0.04085530  0.09453877 -0.20903814  0.60734696  0.69658489  0.33318587
#> [37] -0.09082740 -0.21317885 -0.24276713 -0.34992920 -0.09491974  0.68198892
#> [43] -0.07291051 -0.24350682 -0.40714805  0.05067157
sigma(fit2)
#> [1] 0.2756384
summary(fit2)
#> Semiparametric mixed model fit by maximum likelihood ['mmdml']
#> Scaled residuals (nr_res = 3): 
#>     Min      1Q  Median      3Q     Max 
#> -1.7195 -0.6674 -0.2394  0.3637  2.6234 
#> 
#> Random effects:
#>  Groups   Name        Variance  Std.Dev. 
#>  cask:id  (Intercept) 3.641e-12 1.908e-06
#>  id       (Intercept) 1.226e-02 1.107e-01
#>  Residual             7.598e-02 2.756e-01
#> Number of obs: 46, groups:  cask:id, 20; id, 10
#> 
#> Fixed effects:
#>    Estimate Std. Error z value Pr(>|z|)    
#> x1  0.15693    0.09749   1.610    0.107    
#> x2  0.27435    0.05870   4.674 2.96e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>    x1    
#> x2 -0.029
#> optimizer (nloptwrap) convergence code: 0 (OK)
#> boundary (singular) fit: see ?isSingular
vcov(fit2)
#> 2 x 2 Matrix of class "dpoMatrix"
#>               x1            x2
#> x1  9.505018e-03 -9.208662e-05
#> x2 -9.208662e-05  3.445483e-03
VarCorr(fit2)
#>  Groups   Name        Std.Dev.  
#>  cask:id  (Intercept) 1.9083e-06
#>  id       (Intercept) 1.1074e-01
#>  Residual             2.7564e-01
```

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Emmenegger2021b" class="csl-entry">

Emmenegger, Corinne, and Peter Bühlmann. 2021a. “Double Machine Learning
for Partially Linear Mixed-Effects Models with Repeated Measurements.”
*Preprint arXiv:2108.13657*. <https://arxiv.org/abs/2108.13657>.

</div>

<div id="ref-Emmenegger2021a" class="csl-entry">

———. 2021b. “Regularizing Double Machine Learning in Partially Linear
Endogenous Models.” *Preprint arXiv:2101.12525*.
<https://arxiv.org/abs/2101.12525>.

</div>

</div>

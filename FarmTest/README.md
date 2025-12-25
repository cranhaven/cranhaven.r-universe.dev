# FarmTest

**F**actor-**A**djusted **R**obust **M**ultiple **Test**ing

## Description

The **FarmTest** library implements the **F**actor-**A**djusted **R**obust **M**ultiple **Test**ing method proposed by [Fan et al., 2019](https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1527700). Let *X* be a *p*-dimensional random vector with mean *&mu; = (&mu;<sub>1</sub>,...,&mu;<sub>p</sub>)<sup>T</sup>*. This library carries out simultaneous inference on the *p* hypotheses *H<sub>0j</sub> : &mu;<sub>j</sub> = &mu;<sub>0j</sub>*.
To explicitly caputre the strong dependency among features, we assume that the data vectors *X<sub>i</sub>* that are independently drawn from *X* following a factor model: *X<sub>i</sub> = &mu; + Bf<sub>i</sub> + &epsilon;<sub>i</sub>*, where *f<sub>i</sub>* are the common factors, *B* denotes the factor loading matrix, and *&epsilon;<sub>i</sub>* are idiosyncratic errors. Specifically, we consider three different scenarios with (i) observable factors, (ii) latent factors and (iii) a mixture of covariates and latent factors. Assume *f<sub>i</sub>* and *&epsilon;<sub>i</sub>* are independent and have zero means. The number of hypotheses *p* may be comparable to or considerably exceed the sample size *n*.

FarmTest implements a series of adaptive Huber methods combined with fast data-driven tuning schemes to estimate model parameters and construct test statistics that are robust against heavy-tailed and/or asymetric error distributions. Extensions to two-sample simultaneous mean comparison are also included. As by-products, this library also contains functions that compute adaptive Huber mean and covariance matrix estimators that are of independent interest.

## Main updates for version 2.0.0

The FarmTest method involves multiple tuning parameters for fitting the factor models. In the case of latent factors, the algorithm first computes a robust covariance matrix estimator, and then use the eigenvalue ratio method ([Ahn and Horenstein, 2013](https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA8968)) along with SVD to estimate the number of factors and loading vectors. It is therefore computationally expenstive to select all the tuning parameters via cross-validation. Instead, the current version makes use of the fast data-driven tuning scheme proposed by [Ke et al., 2019](https://projecteuclid.org/euclid.ss/1570780979), which significantly reduces the computational cost.

## Installation

`FarmTest` is available on [CRAN](https://CRAN.R-project.org/package=FarmTest), and it can be installed into `R` environment using the command:

```r
install.packages("FarmTest")
```

## Functions

There are 7 functions in this library:

* `farm.test`: Factor-adjusted robust multiple testing.
* `print.farm.test`: Print function for `farm.test`.
* `summary.farm.test`: Summary function for `farm.test`.
* `plot.farm.test`: Plot function for `farm.test`.
* `huber.mean`: Tuning-free Huber mean estimation.
* `huber.cov`: Tuning-free Huber-type covariance estimation.
* `huber.reg`: Tuning-free Huber regression.

## Getting help

Help on the functions can be accessed by typing `?`, followed by function name at the `R` command prompt. 

For example, `?farm.test` will present a detailed documentation with inputs, outputs and examples of the function `farm.test`.

## Examples

First generate data from a three-factor model *X = &mu; + Bf + &epsilon;*. The sample size and dimension (the number of hypotheses) are taken to be 50 and 100, respectively. The number of nonnulls is 5.

```r
library(FarmTest)
n = 50
p = 100
K = 3
muX = rep(0, p)
muX[1:5] = 2
set.seed(2019)
epsilonX = matrix(rnorm(p * n, 0, 1), nrow = n)
BX = matrix(runif(p * K, -2, 2), nrow = p)
fX = matrix(rnorm(K * n, 0, 1), nrow = n)
X = rep(1, n) %*% t(muX) + fX %*% t(BX) + epsilonX
```

In this case, the factors are unobservable and thus need to be recovered from data. Assume one is interested in simultaneous inference on the means with two-sided alternatives. For a desired FDR level *&alpha;=0.05*, run FarmTest as follows:

```r
output = farm.test(X)
```

The library includes `summary.farm.test`, `print.farm.test` and `plot.farm.test` functions, which summarize, print and visualize the results of `farm.test`: 

```r
summary(output)
print(output)
plot(output)
```

Based on 100 simulations, we report below the average values of the true positive rate (TPR), false positive rate (FPR) and false discover rate (FDR).

| TPR | FPR | FDR |
| :---: | :---: | :---: | 
| 1.000 | 0.002 | 0.026 |

In addition, we illustrate the use of FarmTest under different circumstances. For one-sided alternatives, modify the `alternative` argument to be `less` or `greater`:

```r
output = farm.test(X, alternative = "less")
```

The number of factors can be user-specified. It should be a non-negative integer that is less than the minumum between sample size and number of hypotheses. However, without any subjective ground of the data, this is not recommended.

```r
output = farm.test(X, KX = 10)
```

As a special case, when we set number of factors to be zero, a robust test without factor adjustment will be conducted.

```r
output = farm.test(X, KX = 0)
```

In the situation with observable factors, put the *n* by *K* factor matrix into argument `fX`:

```r
output = farm.test(X, fX = fX)
```

Finally, as an extension to two-sample problems, we generate another sample *Y* with the same dimension 100, and conduct a two-sided test with latent factors.

```r
muY = rep(0, p)
muY[1:5] = 4
epsilonY = matrix(rnorm(p * n, 0, 1), nrow = n)
BY = matrix(runif(p * K, -2, 2), nrow = p)
fY = matrix(rnorm(K * n, 0, 1), nrow = n)
Y = rep(1, n) %*% t(muY) + fY %*% t(BY) + epsilonY
output = farm.test(X, Y = Y)
```
 
As by-products, robust mean and covariance matrix estimation is not only an important step in the FarmTest, but also of independent interest in many other problems. We write separate functions `huber.mean` and `huber.cov` for this purpose.

```r
library(FarmTest)
set.seed(1)
n = 1000
X = rlnorm(n, 0, 1.5)
huberMean = huber.mean(X)

n = 100
d = 50
X = matrix(rt(n * d, df = 3), n, d)
huberCov = huber.cov(X)
```

## Remark 

This library is built upon an earlier version written by Bose, K., Ke, Y. and Zhou, W.-X. ([GitHub](https://github.com/kbose28/FarmTest)). Another library named `tfHuber` that implements data-driven robust mean and covariance matrix estimation as well as standard and *l<sub>1</sub>*-regularized Huber regression can be found [here](https://github.com/XiaoouPan/tfHuber).

## License

GPL-3.0

##  System requirements 

C++11

## Author(s)

Xiaoou Pan <xip024@ucsd.edu>, Yuan Ke <yuan.ke@uga.edu>, Wen-Xin Zhou <wez243@ucsd.edu> 

## Maintainer

Xiaoou Pan <xip024@ucsd.edu>

## References

Ahn, S. C. and Horenstein, A. R. (2013). Eigenvalue ratio test for the number of factors. *Econometrica* **81** 1203–1227. [Paper](https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA8968)

Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to multiple testing. *J. R. Stat. Soc. Ser. B. Stat. Methodol.* **57** 289–300. [Paper](https://www.jstor.org/stable/2346101?seq=1#metadata_info_tab_contents)

Bose, K., Fan, J., Ke, Y., Pan, X. and Zhou, W.-X. (2019). FarmTest: An R package for factor-adjusted robust multiple testing. [Preprint](https://www.math.ucsd.edu/~xip024/Papers/FarmTest.pdf)

Eddelbuettel, D. and Francois, R. (2011). Rcpp: Seamless R and C++ integration. *J. Stat. Softw.* **40** 1-18. [Paper](http://dirk.eddelbuettel.com/code/rcpp/Rcpp-introduction.pdf)

Eddelbuettel, D. and Sanderson, C. (2014). RcppArmadillo: Accelerating R with high-performance C++ linear algebra. *Comput. Statist. Data Anal.* **71** 1054-1063. [Paper](http://dirk.eddelbuettel.com/papers/RcppArmadillo.pdf)

Fan, J., Ke, Y., Sun, Q. and Zhou, W.-X. (2019). FarmTest: Factor-adjusted robust multiple testing with approximate false discovery control. *J. Amer. Statist. Assoc.* **114** 1880-1893. [Paper](https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1527700) 

Huber, P. J. (1964). Robust estimation of a location parameter. *Ann. Math. Statist.* **35** 73-101. [Paper](https://projecteuclid.org/euclid.aoms/1177703732)

Ke, Y., Minsker, S., Ren, Z., Sun, Q. and Zhou, W.-X. (2019). User-friendly covariance estimation for heavy-tailed distributions. *Statis. Sci.* **34** 454-471. [Paper](https://projecteuclid.org/euclid.ss/1570780979)

Sanderson, C. and Curtin, R. (2016). Armadillo: A template-based C++ library for linear algebra. *J. Open Source Softw.* **1** 26. [Paper](http://conradsanderson.id.au/pdfs/sanderson_armadillo_joss_2016.pdf)

Storey, J. D. (2002). A direct approach to false discovery rates. *J. R. Stat. Soc. Ser. B. Stat. Methodol.* **64** 479–498. [Paper](https://www.jstor.org/stable/3088784?seq=1#metadata_info_tab_contents)

Sun, Q., Zhou, W.-X. and Fan, J. (2020). Adaptive Huber regression. *J. Amer. Statist. Assoc.* **115** 254-265. [Paper](https://www.tandfonline.com/doi/abs/10.1080/01621459.2018.1543124)

Zhou, W.-X., Bose, K., Fan, J. and Liu, H. (2018). A new perspective on robust M-estimation: Finite sample theory and applications to dependence-adjusted multiple testing. *Ann. Statist.* **46** 1904-1931. [Paper](https://projecteuclid.org/euclid.aos/1534492823)

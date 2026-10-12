
# gclm

`gclm` contains methods to estimate a sparse parametrization of
covariance matrix as solution of a continuous time Lyapunov equation
(CLE):

\[ B\Sigma + \Sigma B^t + C = 0 \]

Solving the following \(\ell_1\) penalized loss minimization
problem:

\[ \arg\min L(\Sigma(B,C)) + \lambda \rho_1(B) + \lambda_C ||C - C_0||^2_F   \]

subject to \(B\) stable and \(C\) diagonal, where \(\rho_1(B)\) is the
\(\ell_1\) norm of the off-diagonal elements of \(B\) and
\(||C - C_0||^2_F\) is the squared frobenius norm of the difference
between \(C\) and a fixed diagonal matrix \(C_0\) (usually the
identity).

## Installation

``` r
devtools::install_github("gherardovarando/gclm")
```

## Usage

``` r
library(gclm)

### define coefficient matrices
B <- matrix(nrow = 4, c(-4, 2,   0,  0, 
                         0, -3,  1,  0,
                         0,  0, -2,  0.5,
                         0,  0,  0, -1), byrow = TRUE)
C <- diag(c(1,4,1,4))

### solve continuous Lyapunov equation 
### to obtain real covariance matrix
Sigma <- clyap(B, C) 

### obtain observations 
sample <- MASS::mvrnorm(n = 100, mu = rep(0,4),Sigma =  Sigma)


### Solve minimization

res <- gclm(cov(sample), lambda = 0.3, lambdac = 0.01)

res$B
```

    ##           [,1]       [,2]       [,3]       [,4]
    ## [1,] -1.012851  0.3646450  0.0000000  0.0000000
    ## [2,]  0.000000 -0.5771159  0.0000000  0.0000000
    ## [3,]  0.000000  0.0000000 -0.9002638  0.1671158
    ## [4,]  0.000000  0.0000000  0.0000000 -0.3498257

``` r
res$C
```

    ## [1] 0.3169109 0.8481827 0.5380383 1.2108944

The CLE can be freely multiplied by a scalar and thus the \(B,C\)
parametrization can be rescaled. As an example we can impose
\(C_{11} = 1\) as in the true \(C\) matrix, obtaining the estimators:

``` r
C1 <- res$C / res$C[1]
B1 <- res$B / res$C[1]

B1 
```

    ##           [,1]      [,2]      [,3]       [,4]
    ## [1,] -3.196011  1.150623  0.000000  0.0000000
    ## [2,]  0.000000 -1.821067  0.000000  0.0000000
    ## [3,]  0.000000  0.000000 -2.840747  0.5273274
    ## [4,]  0.000000  0.000000  0.000000 -1.1038614

``` r
C1
```

    ## [1] 1.000000 2.676407 1.697759 3.820930

#### Solutions along a regularization path

``` r
path <- gclm.path(cov(sample), lambdac = 0.01)
t(sapply(path, function(res) c(lambda = res$lambda, 
                                npar = sum(res$B!=0),
                                fp = sum(res$B!=0 & B==0),
                                tp = sum(res$B!=0 & B!=0) ,
                                fn = sum(res$B==0 & B!=0),
                                tn = sum(res$B==0 & B==0),
                                errs = sum(res$B!=0 & B==0) + 
                                  sum(res$B==0 & B!=0))))
```

    ##           lambda npar fp tp fn tn errs
    ##  [1,] 0.85588233    4  0  4  3  9    3
    ##  [2,] 0.76078430    4  0  4  3  9    3
    ##  [3,] 0.66568626    5  0  5  2  9    2
    ##  [4,] 0.57058822    6  0  6  1  9    1
    ##  [5,] 0.47549019    6  0  6  1  9    1
    ##  [6,] 0.38039215    6  0  6  1  9    1
    ##  [7,] 0.28529411    6  0  6  1  9    1
    ##  [8,] 0.19019607    6  0  6  1  9    1
    ##  [9,] 0.09509804    7  1  6  1  8    2
    ## [10,] 0.00000000   16  9  7  0  0    9

## Related code

  - Some inspiration is from the `lyapunov` package
    (<https://github.com/gragusa/lyapunov>).

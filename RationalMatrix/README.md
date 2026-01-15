The ‘RationalMatrix’ package
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/RationalMatrix/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/RationalMatrix/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check-valgrind](https://github.com/stla/RationalMatrix/actions/workflows/R-CMD-check-valgrind.yaml/badge.svg)](https://github.com/stla/RationalMatrix/actions/workflows/R-CMD-check-valgrind.yaml)
<!-- badges: end -->

*Exact matrix algebra for matrices with rational entries.*

``` r
library(RationalMatrix)
```

``` r
# a rational matrix
M
##      [,1]   [,2]  [,3]  [,4] 
## [1,] "7"    "4/7" "1/4" "2/3"
## [2,] "3/5"  "2"   "3/2" "3/4"
## [3,] "10/3" "10"  "7"   "1"  
## [4,] "1"    "5/2" "1/3" "7/2"

# determinant
Qdet(M)
## [1] "-227405/3024"

# inverse
Qinverse(M)
##      [,1]            [,2]             [,3]              [,4]             
## [1,] "6678/45481"    "-2274/45481"    "2901/454810"     "-4338/227405"   
## [2,] "-17892/227405" "-491624/227405" "510993/1137025"  "397782/1137025" 
## [3,] "9324/227405"   "666168/227405"  "-525726/1137025" "-572424/1137025"
## [4,] "2352/227405"   "290964/227405"  "-316998/1137025" "101448/1137025"

# check
library(gmp)
as.bigq(M) %*% as.bigq(Qinverse(M))
## Big Rational ('bigq') 4 x 4 matrix:
##      [,1] [,2] [,3] [,4]
## [1,] 1    0    0    0   
## [2,] 0    1    0    0   
## [3,] 0    0    1    0   
## [4,] 0    0    0    1
```

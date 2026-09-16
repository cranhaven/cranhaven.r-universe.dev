# hint

<!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/hint)](https://CRAN.R-project.org/package=hint)
  [![metacran downloads](https://cranlogs.r-pkg.org/badges/hint)](https://cran.r-project.org/package=hint)
[![R build status](https://github.com/alextkalinka/hint/workflows/R-CMD-check/badge.svg)](https://github.com/alextkalinka/hint/actions)
<!-- badges: end -->

## Summary

`hint` is an `R` package for performing hypothesis testing based on Hypergeometric Intersection distributions. For example if you had three gene sets arising from three separate experiments with `x` genes shared between the three, you could determine the probability of arriving at this intersection size by chance. See the companion [paper](https://arxiv.org/abs/1305.0717) for more information.

## Installation

```r
install.packages("hint")
```

## Usage

```r
library(hint)

## The probability of an intersection of size 5 or greater 
## when sampling 15, 8, and 7 balls from three urns 
## each with 1 ball in each of 29 categories.

phint(29, c(15, 8, 7), vals = 5)
  v        cum.p
1 5 0.0002289938

## Formalising a hypothesis test using 'hint.test'.
# Categories given in the first column.
# Numbers of balls in each category given in subsequent columns: each column representing an urn.
dd <- data.frame(categories = letters[1:20], 
                 urn1_count = rep(1,20), 
                 urn2_count = rep(1,20))
tt <- hint.test(dd, letters[1:9], 
                letters[4:15], alternative = "greater")
print(tt)

Hypergeometric intersection test

Parameters:
 n  a  b  q  v 
20  9 12  0  6 

 P(X >= v)  =  0.4649917 

plot(tt)
```

![](./imgs/inters-distr-ex.png)

```r
## Allow duplicates in the second urn.
dd <- data.frame(letters[1:20], 
                 rep(1,20), 
                 c(rep(1,4), rep(2,16)))
tt <- hint.test(dd, letters[1:9], 
                letters[9:14], alternative = "less")
print(tt)

Hypergeometric intersection test

Parameters:
 n  a  b  q  v 
20  9  6 16  1 

 P(X <= v)  =  0.1596769

```

## References

Kalinka (2013). The probability of drawing intersections: extending the hypergeometric distribution. [**arXiv.1305.0717**](https://arxiv.org/abs/1305.0717)


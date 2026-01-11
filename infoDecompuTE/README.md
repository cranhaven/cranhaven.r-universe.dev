infoDecompuTE
=============
[![Build Status](https://travis-ci.org/kcha193/infoDecompuTE.png?branch=master)](https://travis-ci.org/kcha193/infoDecompuTE)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/infoDecompuTE)](https://cran.r-project.org/package=infoDecompuTE)



InfoDecompuTE is capable of generating the structure of the analysis of variance (ANOVA) table of the two-phase experiments. By inputting the design and the relationships of the random and fixed factors using the Wilkinson-Rogers' syntax, infoDecompuTE can generate the structure of the ANOVA table with the coefficients of the variance components for the expected mean squares. This package can also study the balanced incomplete block design and provides the efficiency factors of the fixed effects.

## Installation

infoDecompuTE is available from CRAN. Install it with:

``` r
install.packages("infoDecompuTE")
```

You can also install infoDecompuTE from github with:

``` r
# install.packages("devtools")
devtools::install_github("kcha193/infoDecompuTE")
```
## Examples

The following examples uses infoDecompuTE to construct ANOVA table of single-phase and two-phase experiments

```R
design1 <- local({ 
  Ani = as.factor(LETTERS[c(1,2,3,4,
                            5,6,7,8)])
  Trt = as.factor(letters[c(1,1,1,1,
                            2,2,2,2)])
  data.frame(Ani, Trt, stringsAsFactors = TRUE)
})

summaryAovOnePhase(design1, blk.str = "Ani", trt.str = "Trt") 

design2 <- local({ 
  Run = as.factor(rep(1:4, each = 4))
  Ani = as.factor(LETTERS[c(1,2,3,4,
                            5,6,7,8,
                            3,4,1,2,
                            7,8,5,6)])
  Sam = as.factor(as.numeric(duplicated(Ani)) + 1)
  Tag = as.factor(c(114,115,116,117)[rep(1:4, 4)])
  Trt = as.factor(c("healthy", "diseased")[c(1,2,1,2,
                            2,1,2,1,
                            1,2,1,2,
                            2,1,2,1)])
  data.frame(Run, Ani, Sam, Tag, Trt, stringsAsFactors = TRUE)
})
design2
                                  
summaryAovTwoPhase(design2, blk.str1 = "Ani", blk.str2 = "Run", 
trt.str = "Tag + Trt")  
```


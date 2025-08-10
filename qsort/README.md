<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/qsort)](https://cran.r-project.org/package=qsort) [![Rdoc](http://www.rdocumentation.org/badges/version/qsort)](http://www.rdocumentation.org/packages/qsort) [![Build Status](https://travis-ci.org/joaordaniel/qsort.svg?branch=master)](https://travis-ci.org/joaordaniel/qsort) [![DOI](https://zenodo.org/badge/132877059.svg)](https://zenodo.org/badge/latestdoi/132877059)

qsort
=====

Overview
--------

`qsort` is a package that allows scoring Q-sort data, using criteria sorts and derived scales from subsets of items. This package includes descriptions and scoring procedures for four different Q-sets:

-   Attachment Q-set (version 3.0) (Waters, 1995);
-   California Child Q-set (Block & Block, 1969);
-   Maternal Behaviour Q-set (version 3.1) (Pederson et al., 1999);
-   Preschool Q-set (Baumrind, 1968 revised by Wanda Bronson).

qsort package includes 7 objects:

-   `qsort_score()` a function for scoring Q-sort data;
-   `ex_qsort` a list containing four example data frames for the referred Q-sets;
-   `print_cards()` a function for printing Q-set item cards.
-   `qset_aqs` a data frame containing the Attachment Q-set (aqs; version 3.0);
-   `qset_ccq` a data frame containing the California Child Q-set (ccq);
-   `qset_mbqs` a data frame containing the Maternal Behaviour Q-set (mbqs; version 3.1);
-   `qset_pq` a data frame containing the Preschool Q-set (pq);

Read the documentation (with `?qset_aqs`, `?qset_ccq`, `?qset_mbqs` and `?qset_pq`) to learn more about which criteria sorts and scales are included for each Q-set. Or click the R documentation badge at the top of this page.

Installation
------------

To install `qsort` package from CRAN:

``` r
install.packages("qsort")
```

The `qsort` package can also be installed from GitHub:

``` r
# to install packages from github you first need to install devtools package from CRAN.
# if you haven't installed devtools already just type:
install.packages("devtools")

# to install qsort from github type:
devtools::install_github("joaordaniel/qsort")
```

Example
-------

The example bellow shows how to use `qsort_score()` function to compute scores from California Child Q-sort data, present in `ex_qsort` datasets (`ex_qsort$ccq`).

``` r
library(qsort)
data_ccq <- qsort_score(ex_qsort$ccq, qset_ccq, qsort_length = 100, item1 = "ccq1", subj_id = "participant", group_id = "classroom")
data_ccq
```

    ##    participant classroom scomp_c sest_c egores_c egocont_c sdes_c
    ## 1            1         1  -0.074 -0.093   -0.145    -0.012 -0.137
    ## 2            2         1  -0.023  0.008    0.090     0.157  0.053
    ## 3            3         1   0.092  0.086    0.112    -0.021  0.132
    ## 4            4         1  -0.105 -0.113   -0.182    -0.128 -0.160
    ## 5            5         1  -0.010 -0.039   -0.092    -0.092 -0.053
    ## 6            6         2  -0.104 -0.079   -0.042     0.156 -0.089
    ## 7            7         2   0.051  0.079    0.168     0.169  0.124
    ## 8            8         2   0.049  0.066    0.153     0.185  0.118
    ## 9            9         2  -0.024 -0.007    0.009    -0.026  0.007
    ## 10          10         2  -0.039 -0.046   -0.033     0.098 -0.042
    ##    partial_scomp_c partial_sest_c partial_egores_c partial_egocont_c
    ## 1            0.113          0.047           -0.054            -0.007
    ## 2           -0.160         -0.073            0.086             0.155
    ## 3           -0.061         -0.053           -0.002            -0.026
    ## 4            0.088          0.047           -0.089            -0.123
    ## 5            0.085          0.012           -0.090            -0.090
    ## 6           -0.054         -0.004            0.068             0.160
    ## 7           -0.139         -0.053            0.121             0.166
    ## 8           -0.130         -0.068            0.101             0.182
    ## 9           -0.069         -0.026            0.004            -0.026
    ## 10          -0.003         -0.019            0.005             0.100
    ##    shields_s_emreg
    ## 1              4.3
    ## 2              5.9
    ## 3              5.1
    ## 4              3.7
    ## 5              4.4
    ## 6              5.7
    ## 7              5.9
    ## 8              5.9
    ## 9              5.0
    ## 10             5.2

Read `qsort_score` help file (`?qsort_score`) to learn more about the function's four arguments, and `qset_ccq` help file (`?qset_ccq`) to learn more about variables' names.

Print cards
-----------

To print item descriptions in separate cards use `print_cards()` function. The example bellow uses the `print_cards()` function to create a pdf file with Attachment Q-set items, in a user defined directory (working directory in this case - `getwd()`) .

``` r
library(qsort)
print_cards(qset_aqs, desc_col = "description", dir.print = getwd())
```

Read `print_cards()` help file (`?print_cards`) to learn more about the function's two arguments.

Contributing
------------

[Contribution guidelines for this project](.github/CONTRIBUTING.md)

References
----------

Baumrind, D. (1968). Manual for the Preschool Behaviour Q-set. Parental Research Project. Berkeley, CA: Institute of Human Development.

Block, J. H., & Block, J. (1969). The California Child Q-Set. Berkeley, CA: Institute of Human Development, University of California.

Pederson, D. R., Moran, G., & Bento, S. (1999). Maternal Behaviour Q-sort (version 3.1). London, ON: Psychology Department, Western University.

Waters, E. (1995). Appendix A: The attachment Q-set (Version 3.0). Monographs of the Society for Research in Child Development, 60, 234-246.

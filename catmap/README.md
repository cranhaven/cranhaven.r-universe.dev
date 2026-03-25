
<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

This package conducts fixed-effects (with inverse variance weighting) and random-effects (DerSimonian and Laird (1986)) meta-analyses of case-control or family-based (TDT) genetic data. In addition, catmap performs meta-analyses which combine these two types of study designs. Specifically, this package implements a fixed-effects model (Kazeem and Farrall (2005)) and a random-effects model (Nicodemus (2008)) for combined studies. This package was removed from the CRAN repository sometime after 2009. This is a rendition of the original package updated to work with the newest version of R. The algorithms have not changed since catmap version 1.6.0; however, this version has added some aesthetic improvements.

Quick start
-----------

The main function, `catmap`, accepts `data.frame`, `matrix`, or file input. See `?catmapdata` for help.

``` r
library(catmap)
data(catmapdata)
catmapdata
```

    ##          name study  t nt caserisk controlrisk casenotrisk controlnotrisk
    ## 1  Peter,2002     2  0  0      316         338         220            218
    ## 2 Abrams,2001     2  0  0      710         146         422             96
    ## 3   Todd,2003     2  0  0     1004         344         233            543
    ## 4     Yu,2007     2  0  0     3344         434         544            322
    ## 5    Wei,2007     1 65 32        0           0           0              0

It is important to save the output of the `catmap` function for the next step in the analysis.

``` r
c1 <- catmap(catmapdata, 0.95, FALSE)
```

Four secondary functions use the output of the `catmap` function to build the meta-analysis figures, including the forest plot and the funnel plot. The functions below output these figures to the working directory as pdf files.

``` r
# Make forest plots
?catmap.forest
?catmap.sense
?catmap.cumulative

# Make funnel plot
?catmap.funnel
```

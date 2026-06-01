# NicheBarcoding

This is a quick guide to getting started with the two main functions.


## Installation

You can install the stable released version of `{NicheBarcoding}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Yangcq-Ivy/NicheBarcoding")
```

or directly install from [CRAN](https://cran.r-project.org/) with:

``` r
install.packages("NicheBarcoding")
```

## Functions

This package provides three primary actions:

-   `NBSI` and `NBSI2` functions execute the main identification
    integrating both DNA barcoding and ecological niche modeling.
-   `extractSpeInfo`, `niche.Model.Build`, `pseudo.present.points` 
    and `pseudo.absent.points` functions are the extractable intermediate
    steps of `NBSI` and `NBSI2` that execute information extraction,
    niche modeling and pseudo points generation.
-   `monophyly.prop`, `spe.mantel.test` and `niche.PCA` functions 
    execute the analysis of the characteristics of reference or/and query 
    datasets, including the phylogenetic monophyletic proportion, the
    correlation between interspecific pairwise genetic distance and 
    ecological distance, and the principal component analysis of ecological
    niche between datasets.


## Usage

``` r
rm(list=ls())
library(NicheBarcoding)
```

Load the example bioclimatic layers first.

``` r
data(en.vir)
data(bak.vir)
```

or if you want to download the complete bioclimatic layers from online 
[`worldclim`](https://www.worldclim.org/), run:

``` r
envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
en.vir<-raster::brick(envir)

# Generate random background points
back<-dismo::randomPoints(mask=en.vir,n=5000,ext=NULL,extf=1.1,
                          excludep=TRUE,prob=FALSE,
                          cellnumbers=FALSE,tryf=3,warn=2,
                          lonlatCorrection=TRUE)
bak.vir<-raster::extract(en.vir,back)
```

Here, users can start running the main functions from three different scenarios below.

## Scenario 0

This is the typical situation for most users, where the users have DNA barcodes of species under study for both reference and query samples, with coordinates of species / samples recorded from their own collections. 

``` r
#################################################################
### Scenario 0
### NBSI  DNA barcodes + coordinates of species distribution 
###       available (using online climate data)
#################################################################

library(ape)
data(LappetMoths)
ref.seq<-LappetMoths$ref.seq
que.seq<-LappetMoths$que.seq

NBSI.out<-NBSI(ref.seq,que.seq,ref.add=NULL,
               independence=TRUE,
               model="RF",variables="ALL",
               en.vir=en.vir,bak.vir=bak.vir)
NBSI.out
```

In addition, the coordinates collected from [`GBIF`](https://www.gbif.org/) or published literature can also be included through the `ref.add` parameter. 

When you have an additional reference coordinates information, run:

``` r
ref.add<-LappetMoths$ref.add

NBSI.out2<-NBSI(ref.seq,que.seq,ref.add=ref.add,
                independence=TRUE,
                model="RF",variables="SELECT",
                en.vir=en.vir,bak.vir=bak.vir)
NBSI.out2
```

## Scenario 1

In this case, users may already have species identified by another barcoding method. 

They attempt to further confirm their identifications with niche models built by environmental data. 

The function `NBSI2` is especially designed for this purpose.

``` r
#################################################################
### Scenario 1 
### NBSI2   species identified by other methods or barcodes + 
###         coordinates of species distribution available 
###         (for using online climatic data)
#################################################################

data(LappetMoths)
barcode.identi.result<-LappetMoths$barcode.identi.result
ref.infor<-LappetMoths$ref.infor
que.infor<-LappetMoths$que.infor

NBSI2.out<-NBSI2(ref.infor=ref.infor,que.infor=que.infor,
                 barcode.identi.result=barcode.identi.result,
                 model="RF",variables="SELECT",
                 en.vir=en.vir,bak.vir=bak.vir)
NBSI2.out
```

## Scenario 2

Sometimes users may have species identified by other methods or barcodes alone, or their own environmental data collected by themselves (or alternative online sources).

Obviously, users no longer need to provide species distribution data (coordinates), or use the online environmental data in this case. 

They should prepare two environmental datasets for both reference and query samples.

``` r
#################################################################
### Scenario 2
### NBSI2   species identified by other methods or barcodes + 
###         users possessing their own environmental data
#################################################################

data(LappetMoths)
barcode.identi.result<-LappetMoths$barcode.identi.result
ref.env<-LappetMoths$ref.env
que.env<-LappetMoths$que.env

NBSI2.out2<-NBSI2(ref.env=ref.env,que.env=que.env,
                  barcode.identi.result=barcode.identi.result,
                  model="RF",variables="ALL",
                  en.vir=en.vir,bak.vir=bak.vir)
NBSI2.out2
```

Complete examples can also be found in the help documentation of each functions.

Users can also read the manual to learn more.

## Citing this package

To cite `{NicheBarcoding}`, use:

> Yang, C. Q., X. H. Li, M. C. Orr, A. B. Zhang (2021). NicheBarcoding: An R package for 
> species identification using DNA barcodes integrated with Environmental Niche Models. 
> R package version 1.0. <https://github.com/Yangcq-Ivy/NicheBarcoding>

## Acknowledgments

We thank reviewer [ldecicco-USGS](https://github.com/ldecicco-USGS) for comments on an early 
version of the package.

This work was was supported by China National Funds for Distinguished Young Scientists (to Zhang, 
Grant No. 31425023), by Natural Science Foundation of China (to Zhang, Grant No. 31071963 and 
31272340), Program for Changjiang Scholars and Innovative Research Team in University (IRT13081), 
and Science and Technology Foundation Project (2012FY110803). 


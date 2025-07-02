[![DOI](https://zenodo.org/badge/120989038.svg)](https://zenodo.org/badge/latestdoi/120989038)
[![Travis build status](https://travis-ci.org/liamkendall/pollimetry.svg?branch=master)](https://travis-ci.org/liamkendall/pollimetry)
[![CRAN status](https://www.r-pkg.org/badges/version/pollimetry)](https://www.r-pkg.org/badges/version/pollimetry)



Pollimetry: Predictive allometry for pollinating insects
==========

Tools to estimate pollinator body size as well as bee tongue length and foraging distances.

To install
==========
```
if (!requireNamespace("devtools")) {
  install.packages("devtools")
}
devtools::install_github("liamkendall/pollimetry")
library(pollimetry)
```

We also recommend downloading the data package so the `bodysize` function runs faster

```
#Loading is slow (~ up to 26 Mb per model file)
if (!requireNamespace("devtools")) {
  install.packages("devtools")
}
devtools::install_github("liamkendall/pollimetrydata")
library(pollimetrydata)
```

Estimating body size
====================

The `bodysize` function uses Bayesian generalised linear mixed models (BGLMMs) to provide posterior estimates (along with S.E. and 95% credible intervals) of pollinator body size (i.e. dry body weight (mg)) using the intertegular distance (ITD), species taxonomy or phylogeny (bees only `type="phylo"`), sex and biogeography (at present only Australia, Europe, North America and South America). Estimates (and variance components) are returned as four additional columns bound to the original dataframe. These models will be periodically updated using novel data as and when it becomes available. See `bodysize` details for more information.

Pre-existing equations for Diptera, Hymenoptera and Lepidopteran taxa using body length (`lengthsize`), body length * width (`lengthwidthsize`) and head width (`headwidthsize`) are also provided.

Allometric traits
=================

Users can predict bee tongue length (`tonguelength`) from Cariveau et al. (2015) and bee foraging distances (`foragedist`) from equations described in van Nieuwstadt and Iraheta (1996) for Meliponini, Greenleaf et al. (2007) and Guedot et al. (2009) for Osmia spp.

Future traits of interest
=========================

Many other ecological traits crucial to pollination are likely to be allometric. Therefore, we hope to examine these unexplored body size - trait relationships with the aim of including new predictive models in the future. 

Example
========

Install and call the pollimetry library as follows:

```
devtools::install_github("liamkendall/pollimetry")
library(pollimetry)
```

This will install pollimetry and it's dependency `brms`.
You can check the raw data used in the paper as follows:

```
load("data/pollimetry_dataset.rdata")
head(pollimetry_dataset)
?pollimetry_dataset #for metadata.
```
Let's predict some body sizes from a dataframe using taxonomy:

```
(example <- cbind.data.frame(IT = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Ceratina_dupla","Andrena_flavipes")))

bodysize(x = example, taxa = "bee", type = "taxo")
?bodysize
```

Now let's calculate some foraging distances based only in ITDs:

```
foragedist(c(10,5,2), type = "GreenleafAll") 
```

Other functions included are `tonguelength()` for estimating tongue length and older allometric equations: `headwidthsize()`, `lengthwidthsize()`, and `lengthsize()`.

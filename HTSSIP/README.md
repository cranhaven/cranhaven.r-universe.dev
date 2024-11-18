HTSSIP
======

[![Travis-CI Build Status](https://travis-ci.org/seb369/HTSSIP.svg?branch=master)](https://travis-ci.org/seb369/HTSSIP)

HTSSIP is an R package for analyzing high throughput sequence data
from nucleotide stable isotope probing (DNA- & RNA-SIP) experiments. 


## Available analyzes 

* Identifying community-level isotope incorporatation
  * Ordinations of gradient fraction communities
  * Beta diversity of overlapping gradient fractions

* Identifying isotope incorporators
  * High resolution stable isotope probing (HR-SIP)
  * Multiple window high resolution stable isotope probing (MW-HR-SIP)
  * Quantitative stable isotope probing (q-SIP)


## Documentation

All documentation can be found on [CRAN](https://cran.r-project.org/package=HTSSIP).

A good place to start is the **HTSSIP introduction** vignette. 

The manuscript describing HTSSIP is:

> Youngblut ND, Barnett SE, Buckley DH (2018) HTSSIP: An R package for analysis of high throughput sequencing data from nucleic acid stable isotope probing (SIP) experiments. PLoS ONE 13(1): e0189616. https://doi.org/10.1371/journal.pone.0189616


## References 

See `References` in the **HTSSIP introduction** vignette.


## Installation

To get the current released version from [CRAN](https://cran.r-project.org/package=HTSSIP):

```R
install.packages("HTSSIP") 
```

To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("buckleylab/HTSSIP")
```




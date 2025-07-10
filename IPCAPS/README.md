
# R Package IPCAPS

## Summary

The R package ```IPCAPS``` is an unsupervised clustering algorithm based on 
iterative pruning to capture population structure. This version supports ordinal 
data which can be applied directly to SNP data to identify fine-level population 
structure and it is built on the iterative pruning Principal Component Analysis 
(ipPCA) algorithm by Intarapanich et al. (2009) <doi: 10.1186/1471-2105-10-382> 
and Limpiti et al. (2011)<doi: 10.1186/1471-2105-12-255>. The IPCAPS 
involves an iterative process using multiple splits based on multivariate 
Gaussian mixture modeling of principal components and Clustering EM estimation 
as in Lebret et al. (2015). In each iteration, rough clusters and outliers are 
also identified using the function ```rubikclust()``` from the R package ```KRIS```.

The R package ```IPCAPS``` requires ```stats```, ```utils```, ```graphics```, 
```grDevices```, ```MASS```, ```Matrix```, ```expm```, ```KRIS```, ```fpc```, 
```LPCM```, ```apcluster```, ```Rmixmod```.

Here is the list of functions in the R package ```IPCAPS```:

* ```export.groups```
* ```get.node.info```
* ```ipcaps```
* ```save.eigenplots.html```
* ```save.html```
* ```save.plots.cluster.html```
* ```save.plots.label.html```
* ```save.plots```
* ```top.discriminator```

Moreover, here is the list of example datasets in the R package ```IPCAPS```:

* ```label```
* ```PC```
* ```raw.data```

Lastly, here is the list of example data files included in the directory ```extdata``` of ```IPCAPS```:

* ```ipcaps_example.bed```
* ```ipcaps_example.bim```
* ```ipcaps_example.fam```
* ```ipcaps_example_PC10.txt.gz```
* ```ipcaps_example_individuals.txt.gz```
* ```ipcapsS_example_rowVar_colInd.txt.gz```
* ```ipcaps_example.rda```

## Installation

Install the released version of ```IPCAPS``` from CRAN:


```r
install.packages("IPCAPS")
```

## For developemenpers: problem sovling in checking the package as CRAN

### Error of Roxygen2 in building RD files

The source codes in this package include the Roxgen's syntax. If there is a 
problem for generating the RD files (facing some errors) using RStudio (_Build > 
Document_), try to use ```roxygen2::roxygenise()``` instead of _Build > Document_ 
from the menu. Alternatively, install the package ```devtools```, then enable 
RStudio to use the functions from ```devtools``` (check _Build > Configure Build 
Tools... > use devtools package functions if available_) or run 
```devtools::document()``` in the console.

### Error of testthat for unit testing

When facing error for ```testthat```, try to update the package ```testthat``` and add 
```Suggests: testthat``` in _DESCRIPTION_ file.

### Submit package to CRAN

Check the submission using ```R CMD check --as-cran``` and a current version of 
r-devel, as mandated by the CRAN Repository Policy. (You could do so using the 
win-builder service at http://win-builder.r-project.org)

## Resubmit new version to CRAN

Check downstream dependencies with ```devtools::revdep_check()```

### Error on checking DESCRIPTION meta-information in Linux

Edit ~/.profile, ~/.bash_profile or ~/.bashrc, then add

export LANG=en_US.UTF-8

export LC_ALL=en_US.UTF-8

## CONTRIBUTOR CODE OF CONDUCT

As contributors and maintainers of this project, we pledge to respect all people 
who contribute through reporting issues, posting feature requests, updating 
documentation, submitting pull requests or patches, and other activities.

We are committed to making participation in this project a harassment-free 
experience for everyone, regardless of level of experience, gender, gender 
identity and expression, sexual orientation, disability, personal appearance, 
body size, race, ethnicity, age, or religion.

Examples of unacceptable behavior by participants include the use of sexual 
language or imagery, derogatory comments or personal attacks, trolling, public 
or private harassment, insults, or other unprofessional conduct.

Project maintainers have the right and responsibility to remove, edit, or reject 
comments, commits, code, wiki edits, issues, and other contributions that are 
not aligned to this Code of Conduct. Project maintainers who do not follow the 
Code of Conduct may be removed from the project team.

Instances of abusive, harassing, or otherwise unacceptable behavior may be 
reported by opening an issue or contacting one or more of the project 
maintainers.

This Code of Conduct is adapted from the Contributor Covenant, version 1.0.0, 
available at https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html


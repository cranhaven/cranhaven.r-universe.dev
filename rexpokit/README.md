# rexpokit

The R package `rexpokit` is a key dependency of the [BioGeoBEARS](https://github.com/nmatzke/BioGeoBEARS) R package for phylogenetic biogeography.

`rexpokit` wraps some of the matrix exponentiation utilities from
[EXPOKIT](http://www.maths.uq.edu.au/expokit/), 
a Fortran 77 library that is widely recommended for matrix exponentiation 
(Sidje RB, 1998. "Expokit: A Software Package for Computing Matrix 
Exponentials." *ACM Trans. Math. Softw.* 24(1): 130-156). EXPOKIT includes 
functions for exponentiating both small, dense matrices, and large, sparse 
matrices (in sparse matrices, most of the cells have value 0). 

Rapid matrix exponentiation is useful in phylogenetics when we have a large 
number of states (as we do when we are inferring the history of transitions 
between the possible geographic ranges of a species), but is probably 
useful in other ways as well.

**Installation**

As rexpokit contains C++ and FORTRAN code, it is easiest to install the pre-compiled binaries from CRAN. This is done from the R command line, with:

```
install.packages("rexpokit")
```

If you want to install the GitHub version, you will need gfortran and gcc compilers installed on your machine. Then, install using devtools:

```
library(devtools)
install_github("nmatzke/rexpokit", INSTALL_opts="--byte-compile")
```

To see examples of the matrix exponentiation calculations, see the example code with:

```
library(rexpokit)
?rexpokit
```

**Build status** on Travis-CI: (Deleted, as Travis-CI now requires subscription.)

**NOTE:** As of 2023-06-21, a new version of ```rexpokit```, 0.26.6.8, has been submitted to CRAN. This version fixes some warnings caused by ```gfortan```/```gcc10```. Binaries should be available soon.  Old binaries have been archived at: [https://github.com/nmatzke/Matzke_R_binaries](https://github.com/nmatzke/Matzke_R_binaries)


**NOTE:** As of 2019-11-04, a new version of ```rexpokit```, 0.26.6.6, has been accepted on CRAN. This version fixes some warnings caused by ```gfortan```/```gcc10```, currently in beta, but nonetheless checked by CRAN. Binaries should be available soon.  Old binaries have been archived at: [https://github.com/nmatzke/Matzke_R_binaries](https://github.com/nmatzke/Matzke_R_binaries)

**NOTE:** As of 2018-10-03, a new version of rexpokit, 0.26.6, has been accepted on CRAN. This version fixes warnings due to upgrades in CRAN's FORTRAN compilers, and more importantly, removes some dependencies previously needed by cladoRcpp/BioGeoBEARS. CRAN version is here, binaries should be available in a few days: [https://cran.r-project.org/package=rexpokit](https://cran.r-project.org/package=rexpokit)

**Release v0.26.6** registered on Zenodo: [![DOI](https://zenodo.org/badge/17001945.svg)](https://zenodo.org/badge/latestdoi/17001945)

**Zenodo link for release:** https://zenodo.org/badge/latestdoi/17001945

**Zenodo DOI for release:** http://dx.doi.org/10.5281/zenodo.1442889

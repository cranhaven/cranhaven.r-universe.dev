This repository is just a wrapper to emulate a "standard" R package
from https://github.com/libKriging/bindings/R/rlibkriging content,
so you can install it just using:

```r
install.packages('devtools')
devtools::install_github("libKriging/rlibkriging")
```

The stable version is available from CRAN [![Downloads
(monthly)](https://cranlogs.r-pkg.org/badges/rlibkriging)](https://cran.r-project.org/package=rlibkriging) :

```r
install.packages('rlibkriging')
```


## Requirements

* `c++`, `cmake` and `gfortran`, should be installed using:
  * Linux/OSX: `brew/apt/yum/... install cpp cmake gfortran`
  * Windows: install Rtools (see https://cran.r-project.org/bin/windows/Rtools/)
    Note:
      * R>=4.2 & Rtools>=42 are required for this 'devtools::install_github'
      * for older R/Rtools, refer to manual install: https://github.com/libKriging/libKriging#compilation-for-linuxmacwindows-using-r-toolchain

Note: this repository mainly contains modified Makefiles, inspired by https://github.com/astamm/nloptr wrapper.

## CRAN

When submitting to CRAN, `./tools/setup.sh` should be run before `R CMD build rlibkriging` to fit CRAN policy.

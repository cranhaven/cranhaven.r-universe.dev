# MPTmultiverse

MPTmultiverse is an R package that provides functions for a multiverse analysis of multinomial processing tree (MPT) models. Note that the package is currently work in progress and should be considered alpha. If you experience problems, [open an issue](https://github.com/mpt-network/MPTmultiverse/issues/new).


[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/mpt-network/MPTmultiverse.svg?branch=master)](https://travis-ci.org/mpt-network/MPTmultiverse)


## Install

To install `MPTmultiverse`, make sure you already installed the `devtools` package via `install.packages("devtools")`. Moreover, you also need a to have JAGS installed: Go to http://mcmc-jags.sourceforge.net/ for instructions on how to install JAGS on your machine.

If these prerequisites are met, type `devtools::install_github("mpt-network/MPTmultiverse")` in your R console to install `MPTmultiverse` together with all required packages that it depends on. To make sure that you are using the latest versions of all packages, you should also run `update.packages(ask = FALSE)`.

## Usage

1. Create a new folder that contains the following three files
   (cf. the subfolder `vignettes/`):
    1. The MPT model in the `.eqn`-format
        * The model should be parameterized including all equality constraints.
        * To encode fixed parameters (e.g., g=.50), replace the parameter 
          in the eqn-file by constants.
    2. The data with individual frequencies as a `.csv`-file
    3. The file `analysis.rmd`(copied from the `vignettes` subfolder). 
2. Adjust the input options in `analysis.rmd` in the section 
   "MPT model definition & Data". You have to specify the correct file names
   and the names of the columns in your data that contain a subject identifier and
   between-subjects conditions.
3. Optionally, set some options (e.g., the number of bootstrap samples) via `mpt_options()`
3. Run the analysis script (e.g., by knitting the .rmd file).

## Current limitations

* For the Bayesian models with "no-pooling" and "complete-pooling", no additional 
  MCMC samples are drawn to achieve the desired level of convergence (e.g., `Rhat < 1.05`).
  This might be addressed in future versions of TreeBUGS. 
  As a remedy, the number of MCMC iterations can be increased a priori (via `mpt_options()`).
  
---

All code in this repository is released under the [GPL v2 or later license](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html). All non-code materials is released under the [CC-BY-SA license](https://creativecommons.org/licenses/by-sa/4.0/).


<!-- README.md is generated from README.Rmd. Please edit that file -->

# The whSample Package

***whSample*** helps analysts quickly generate statistical samples from
Excel or Comma Separated Value (CSV) files and write them to a new Excel
workbook. Users have a choice of Simple Random or Stratified Random
samples, and a third choice of having each stratum included in a
separate worksheet.

See package vignettes for detailed documentation.

## ssize

The workhorse function is *sampler*. A helper function, *ssize*,
estimates the minimum sample size necessary to achieve statistical
requirements using a Normal Approximation to the Hypergeometric
Distribution. This distribution spans the probabilities of yes/no-type
responses without replacement. These parameters are:

-   **N**, the population size.
-   **ci**, the required confidence interval. The default is 95%.
-   **me**, the required level of precision, or margin of error. The
    default is +/- 7%.
-   **p**, the anticipated rate of occurrence. The default is 50%.

*ssize(N, ci=0.95, me=0.07, p=0.50)* (showing the defaults) only
requires the **N** argument. Used as a standalone, it can be used to
explore sample sizes under other conditions. For example, a probe sample
may suggest that a 50-50 probability isn’t realistic. A revised sample
size can be estimated with the observed success probability (p=0.6, for
example).

## sampler

The *sampler* function calls *ssize* to get its sample size estimate.
Therefore, it requires the **ci**, **me**, and **p** arguments, which it
passes to *ssize*.

*sampler* also takes four additional arguments:

-   **irisData** opens the file chooser to a folder with example files
    of Anderston’s Iris dataset of flower characteristics.
-   **backups** provides a buffer for use if necessary to replace
    samples found to be invalid for some reason,
-   **seed** is used to seed the internal random number generator, and
-   **keepOrg** determines if a copy of the population is included in
    the output.

The defaults for these additional arguments are *backups=5*,
*irisData=F*, *seed=NULL* and *keepOrg=F*. The default seed will tell
*sampler* to use the current system time in milliseconds. Any number can
be used as a seed. Whichever one is used will be listed in the *Report*
output tab. The keep-original option (*keepOrg*) defaults to FALSE, but
could be set to *keepOrg=T* for smaller populations that wouldn’t exceed
Excel’s row limit is 1,048,576 rows.

To override any of these defaults, enter *name=value* as an argument.

*sampler* uses a series of menus to guide users through the sampling
process.

## Output

*sampler* creates a new Excel workbook with three parts:

-   a copy of the original (source) data if previously requested,

-   an Excel spreadsheet with the requested sample, and

-   a new tab called *Report* with key reference information:

    -   path and name of the source file

    -   size (in rows) of the source file

    -   sample type (Simple Random Sample, Stratified Random Sample, or
        Tabbed Stratified Sample)

    -   sampling parameters

    -   sample size

    -   stratification key

    -   number of strata

    -   number of backups requested (this number is applied to every
        stratum in a stratified sample)

    -   random number seed used, for documentation and reproducibility

    -   date-time stamp of when the sample was generated

    -   stratification information (name, number in the population,
        proportion of the population, and the number of samples)

## Installation

You can install *whSample* from CRAN with:

``` r
install.packages("whSample")
```

or get the latest developmental version with:

``` r
devtools::install_github("km4ivi/whSample")
```

### Other necessary packages

*sampler* depends on several external packages to run properly. If
you’re running a developmental version, make sure these packages are
installed on your computer:

-   tidyverse (or individually: magrittr, dplyr, purrr)
-   openxlsx
-   data.table
-   tools
-   utils
-   tcltk
-   bit64

## Examples

*ssize(5000)*: N=5000, other arguments use defaults

*ssize(5000, p=0.60)*: N=5000, with a 60% expected rate of occurrence

*sampler()*: Uses all defaults, gets N from the source data.

*sampler(backups=2, seed=12345)*: Overrides specific defaults

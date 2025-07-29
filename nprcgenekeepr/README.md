README
================
R. Mark Sharp, Ph.D.
2025-07-25

# nprcgenekeepr <a href="https://github.com/rmsharp/nprcgenekeepr"><img src="man/figures/logo.png" align="right" height="138" alt="" /></a>

Version 1.0.8 (2025-07-25)

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/rmsharp/nprcgenekeepr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmsharp/nprcgenekeepr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/rmsharp/nprcgenekeepr/graph/badge.svg)](https://app.codecov.io/gh/rmsharp/nprcgenekeepr)
[![CRAN
status](https://www.r-pkg.org/badges/version/nprcgenekeepr)](https://CRAN.R-project.org/package=nprcgenekeepr)
<!-- badges: end -->

## Introduction

The goal of **nprcgenekeepr** is to implement Genetic Tools for Colony
Management. It was initially conceived and developed as a Shiny web
application at the Oregon National Primate Research Center (ONPRC) to
facilitate some of the analyses they perform regularly. It has been
enhanced to have more capability as a Shiny application and to expose
the functions so they can be used either interactively or in R scripts.

This work has been supported in part by NIH grants P51 RR13986 to the
Southwest National Primate Research Center and P51 OD011092 to the
Oregon National Primate Research Center.

<!--It is now managed and maintained as a joint effort between ONPRC-->

<!--and Southwest National Primate Research Center (SNPRC) with the -->

<!--coding being done by R. Mark Sharp, Ph.D.-->

At present, the application supports 5 functions:

1.  Quality control of studbooks contained in text files or Excel
    workbooks and of pedigrees within LabKey Electronic Health Records
    (EHR)
2.  Creation of pedigrees from a lists of animals using the LabKey EHR
    integration
3.  Creation and display of an age by sex pyramid plot of the living
    animals within the designated pedigree
4.  Generation of Genetic Value Analysis Reports
5.  Creation of potential breeding groups with and without proscribed
    sex ratios and defined maximum kinships.

**For more information see:**  
A Practical Approach for Designing Breeding Groups to Maximize Genetic
Diversity in a Large Colony of Captive Rhesus Macaques (*Macaca
mulatto*) Vinson, A ; Raboin, MJ *Journal Of The American Association
For Laboratory Animal Science*, 2015 Nov, Vol.54(6), pp.700-707 \[Peer
Reviewed Journal\]

## Installation

You can install the CRAN version of **nprcgenekeepr** from the R console
prompt with:

``` r
install.packages("nprcgenekeepr")
```

You can install the development version of **nprcgenekeepr** from GitHub
from the R console prompt with:

``` r
install.packages("devtools")
devtools::install_github(file.path("rmsharp", "nprcgenekeepr"))
```

All missing dependencies should be automatically installed.

## Online Documentation

You can find the complete online documentation at
<https://rmsharp.github.io/nprcgenekeepr/>.

At the top of the page are three menus to the right of the `Home` icon:
`Reference`, `Articles`, and `Changelog`.

The `Reference` menu at the top of the page brings up the list of
documentation for `Data objects`, `Major Features and Functions`,
`Primary interactive functions` and `All exposed functions`.

The `Articles` menu brings up the list of vignettes, which are, except
for `Development Plans`, tutorials for using the package.

The `Changelog` brings up a copy of the NEWS file of the package, which
records the major changes made for each version.

## Running Shiny Application

The toolset available within nprcgenekeepr can be used inside standard R
scripts. However, it was originally designed to be used within a Shiny
application that can be started with:

``` r
library(nprcgenekeepr) # nolint: undesirable_function_linter
runGeneKeepR()
```

## Summary of Major Functions

### Quality Control

Studbooks maintained by breeding colonies generally contain information
of varying quality. The quality control functions of the toolkit check
to ensure all animals listed as parents have their own line entries, all
parents have the appropriate sex listed, no animals are listed as both a
sire and a dam, duplicate entries are removed, pedigree generation
numbers are added, and all dates are valid dates. In addition, exit
dates are added if possible and are consistent with other information
such as departure dates and death dates. Current ages of animals that
are still alive are added if a database connection is provided via a
configuration file and the user has read permission on a LabKey server
with the demographic data in an *EHR* (Electronic Health Record) module.
See [LabKey
documentation](https://www.labkey.org/Documentation/wiki-page.view?name=netrc).

Parents with ages below a user selected threshold are identified. A
minimum parent age in years is set by the user and is used to ensure
each parent is at least that age on the birth date of an offspring. The
minimum parent age defaults to 2 years. This check is not performed for
animals with missing birth dates.

### Creation of Pedigree From a List of Potential Breeders and LabKey 

The user can enter a list of focal animals in a CSV file that will be
used to create a pedigree containing all direct relative (ancestors and
descendants) via the **labkey.selectRows** function within the
**Rlabkey** package if a database connection is provided via a
configuration file and the user has read permission on a LabKey server
with the demographic data in an **EHR** (Electronic Health Record)
module.

Two configuration files are needed to use the database features of
nprcgenekeepr with LabKey. The first file is named **\_netrc** on
Microsoft Windows operating systems and **.netrc** otherwise, allows the
user to authenticate with LabKey through the LabKey API and is fully
described by [LabKey
documentation](https://www.labkey.org/Documentation/wiki-page.view?name=netrc)

The second file is named **\_nprcgenekeepr_config** on Microsoft Windows
operating systems and **.nprcgenekeepr_config** otherwise and is the
`nprcgenekeepr` [configuration
file](https://github.com/rmsharp/nprcgenekeepr/blob/master/inst/extdata/example_nprcgenekeepr_config)
An image of this example configuration file is included as a data object
and can be loaded and viewed with the following lines of R code in the R
console.

``` r
data("exampleNprcgenekeeprConfig")
View(exampleNprcgenekeeprConfig)
```

### Display of an age by sex pyramid plot

Adapted from
<https://www.thoughtco.com/age-sex-pyramids-and-population-pyramids-1435272>
on 20190603. Written by Matt Rosenberg. Updated May 07, 2019.

The most important demographic characteristic of a population is its
age-sex structure. Age-sex pyramids (also known as population pyramids)
graphically display this information to improve understanding and make
comparison easy. The population pyramid sometimes has a distinctive
pyramid-like shape when displaying a growing population.

#### How to Read the Age-Sex Graph

An age-sex pyramid breaks down a population into male and female genders
and age ranges. Usually, you’ll find the left side of the pyramid
graphing the male population and the right side of the pyramid
displaying the female population.

Along the horizontal axis (x-axis) of a population pyramid, the graph
displays the population either as a total population of that age or as a
percentage of the population at that age. The center of the pyramid
starts at zero population and extends out to the left for males and
right for females in increasing size, or proportion of the population.

Along the vertical axis (y-axis), age-sex pyramids display two-year age
increments, from birth at the bottom to old age at the top.

### Genetic Value Analysis Reports

The Genetic Value Analysis is a ranking scheme developed at ONPRC to
indicate the relative breeding value of animals in the colony. The
scheme uses the mean kinship for each animal to indicate how
inter-related it is with the rest of the current breeding colony
members. Genome uniqueness is used to provide an indication of whether
or not an animal is likely to possess alleles at risk of being lost from
the colony. Under the scheme, animals with low mean kinship or high
genome uniqueness are ranked more highly.

### Breeding Group Formation

One of the goals in breeding group formation is to avoid the potential
for mating of closely related animals. Since behavioral concerns and
housing constraints will also be taken into account in the group
formation process, it is our goal to provide the largest number of
animals possible from a list of candidates that can be housed together
without risk of consanguineous mating. To that end, this function uses
information from the Genetic Value Analysis to search for the largest
combinations of animals that can be produced from a list of candidates.

The default options do not consider the sex of individuals when forming
the groups, though this has likely been a consideration by the user in
selecting the candidate group members. Optionally the user may select to
form harem groups, which considers the sex of individuals when forming
groups and restricts the number of males to one per group.

**For more information see:**  
A Practical Approach for Designing Breeding Groups to Maximize Genetic
Diversity in a Large Colony of Captive Rhesus Macaques (*Macaca
mulatto*) Vinson, A ; Raboin, MJ *Journal Of The American Association
For Laboratory Animal Science*, 2015 Nov, Vol.54(6), pp.700-707 \[Peer
Reviewed Journal\]

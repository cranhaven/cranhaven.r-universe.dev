
<img src="https://raw.githubusercontent.com/aravind-j/PGRdup/master/inst/extdata/PGRdup.png" align="center" alt="logo" width="750" height = "85" style = "border: solid white 10px;">

###### Version : [0.3.0](https://aravind-j.github.io/PGRdup/articles/Introduction.html#version-history); Copyright (C) 2014-2026: [ICAR-NBPGR](https://nbpgr.org.in/); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### *Aravind, J.<sup>1</sup>, Radhamani, J.<sup>1</sup>, Kalyani Srinivasan<sup>1</sup>, Ananda Subhash, B.<sup>2</sup>, and Tyagi, R. K.<sup>1</sup>*

1.  ICAR-National Bureau of Plant Genetic Resources, New Delhi, India
2.  Centre for Development of Advanced Computing, Thiruvananthapuram,
    Kerala, India

------------------------------------------------------------------------

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/PGRdup)](https://cran.r-project.org/package=PGRdup)
[![Dependencies](https://tinyverse.netlify.app/status/PGRdup)](https://cran.r-project.org/package=PGRdup)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PGRdup?color=green)](https://CRAN.R-project.org/package=PGRdup)
[![develVersion](https://img.shields.io/badge/devel%20version-0.3.0-orange.svg)](https://github.com/aravind-j/PGRdup)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/aravind-j/PGRdup.svg)](https://github.com/aravind-j/PGRdup)
[![R-CMD-check](https://github.com/aravind-j/PGRdup/workflows/R-CMD-check/badge.svg)](https://github.com/aravind-j/PGRdup/actions)
[![Project Status:
Inactive](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2026--04--21-yellowgreen.svg)](https://github.com/aravind-j/PGRdup)
[![Rdoc](https://api.rdocumentation.org/badges/version/PGRdup)](https://www.rdocumentation.org/packages/PGRdup/)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.841963.svg)](https://doi.org/10.5281/zenodo.841963)
[![Website -
pkgdown](https://img.shields.io/website-up-down-green-red/https/aravind-j.github.io/PGRdup.svg)](https://aravind-j.github.io/PGRdup/)
<!-- [![.](https://pro-pulsar-193905.appspot.com/UA-112827210-2/welcome-page)](https://github.com/aravind-j/google-analytics-beacon) -->
[![GoatCounter](https://pgrdup-gh.goatcounter.com/count?p=/test)](https://pgrdup-gh.goatcounter.com/)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/aravind-j/PGRdup) -->
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/aravind-j/PGRdup/total.svg)] -->

------------------------------------------------------------------------

## Introduction <img src="https://raw.githubusercontent.com/aravind-j/PGRdup/master/inst/extdata/PGRdup_v2.png" align="right" alt="logo" width="173" height = "200" style = "padding: 10px; border: none; float: right;">

The `R` package `PGRdup` was developed as a tool to aid genebank
managers in the identification of probable duplicate accessions from
plant genetic resources (PGR) passport databases.

This package primarily implements a workflow designed to fetch groups or
sets of germplasm accessions with similar passport data particularly in
fields associated with accession names within or across PGR passport
databases.

The functions in this package are primarily built using the following R
packages:

- [`data.table`](https://cran.r-project.org/package=data.table)
- [`igraph`](https://cran.r-project.org/package=igraph)
- [`stringdist`](https://cran.r-project.org/package=stringdist)
- [`stringi`](https://cran.r-project.org/package=stringi)

## Installation

The package can be installed from CRAN as follows:

``` r
# Install from CRAN
install.packages('PGRdup', dependencies=TRUE)

The development version can be installed from github as follows:
# Install development version from Github
devtools::install_github("aravind-j/PGRdup")
```

## Workflow

The series of steps involve in the workflow along with the associated
functions are are illustrated below:

#### Step 1

**Function(s) :**

- `DataClean`
- `MergeKW`
- `MergePrefix`
- `MergeSuffix`

Use these functions for the appropriate data standardisation of the
relevant fields in the passport databases to harmonize punctuation,
leading zeros, prefixes, suffixes etc. associated with accession names.

#### Step 2

**Function(s) :**

- `KWIC`

Use this function to extract the information in the relevant fields as
keywords or text strings in the form of a searchable Keyword in Context
(KWIC) index.

#### Step 3

**Function(s) :**

- `ProbDup`

Execute fuzzy, phonetic and semantic matching of keywords to identify
probable duplicate sets either within a single KWIC index or between two
indexes using this function. For fuzzy matching the levenshtein edit
distance is used, while for phonetic matching, the double metaphone
algorithm is used. For semantic matching, synonym sets or ‘synsets’ of
accession names can be supplied as an input and the text strings in such
sets will be treated as being identical for matching. Various options to
tweak the matching strategies used are also available in this function.

#### Step 4

**Function(s) :**

- `DisProbDup`
- `ReviewProbDup`
- `ReconstructProbDup`

Inspect, revise and improve the retrieved sets using these functions. If
considerable intersections exist between the initially identified sets,
then `DisProbDup` may be used to get the disjoint sets. The identified
sets may be subjected to clerical review after transforming them into an
appropriate spreadsheet format which contains the raw data from the
original database(s) using `ReviewProbDup` and subsequently converted
back using `ReconstructProbDup`.

#### Adjuncts

**Function(s) :**

- `ValidatePrimKey`
- `DoubleMetaphone`
- `ParseProbDup`
- `AddProbDup`
- `SplitProbDup`
- `MergeProbDup`
- `ViewProbDup`
- `KWCounts`
- `read.genesys`

Use these helper functions if needed. `ValidatePrimKey` can be used to
check whether a column chosen in a data.frame as the primary primary
key/ID confirms to the constraints of absence of duplicates and NULL
values.

`DoubleMetaphone` is an implementation of the Double Metaphone phonetic
algorithm in `R` and is used for phonetic matching.

`ParseProbDup` and `AddProbDup` work with objects of class `ProbDup`.
The former can be used to parse the probable duplicate sets in a
`ProbDup` object to a `data.frame` while the latter can be used to add
these sets data fields to the passport databases. `SplitProbDup` can be
used to split an object of class `ProbDup` according to set counts.
`MergeProbDup` can be used to merge together two objects of class
`ProbDup`. `ViewProbDup` can be used to plot the summary visualizations
of probable duplicate sets retrieved in an object of class `ProbDup`.

`KWCounts` can be used to compute keyword counts from PGR passport
database fields(columns), which can give a rough indication of the
completeness of the data.

`read.genesys` can be used to import PGR data in a Darwin Core -
germplasm zip archive downloaded from genesys database into the R
environment.

## Tips

- Use [`fread`](https://rdrr.io/cran/data.table/man/fread.html) to
  rapidly read large files instead of `read.csv` or `read.table` in
  `base`.
- In case the PGR passport data is in any DBMS, use the appropriate
  [`R`-database interface
  packages](https://www.burns-stat.com/r-database-interfaces/) to get
  the required table as a `data.frame` in `R`.

## Note

- The `ProbDup` function can be memory hungry with large passport
  databases. In such cases, ensure that the system has sufficient memory
  for smooth functioning (See `?ProbDup`).

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = 'PGRdup')
```

The vignette for the latest version is also available
[online](https://aravind-j.github.io/PGRdup/articles/Introduction.html).

## What’s new

To know whats new in this version type:

``` r
news(package='PGRdup')
```

## Links

[CRAN page](https://cran.r-project.org/package=PGRdup)

[Github page](https://github.com/aravind-j/PGRdup)

[Documentation website](https://aravind-j.github.io/PGRdup/)

[Zenodo DOI](https://doi.org/10.5281/zenodo.841963)

## CRAN checks

[![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html)

|  |  |
|:---|----|
| r-devel-linux-x86_64-debian-clang | [![CRAN check - r-devel-linux-x86_64-debian-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-clang/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-devel-linux-x86_64-debian-gcc | [![CRAN check - r-devel-linux-x86_64-debian-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-gcc/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-devel-linux-x86_64-fedora-clang | [![CRAN check - r-devel-linux-x86_64-fedora-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-clang/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-devel-linux-x86_64-fedora-gcc | [![CRAN check - r-devel-linux-x86_64-fedora-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-gcc/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-patched-linux-x86_64 | [![CRAN check - r-patched-linux-x86_64](https://badges.cranchecks.info/flavor/r-patched-linux-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-release-linux-x86_64 | [![CRAN check - r-release-linux-x86_64](https://badges.cranchecks.info/flavor/r-release-linux-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |

[![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html)

|  |  |
|:---|----|
| r-devel-windows-x86_64 | [![CRAN check - r-devel-windows-x86_64](https://badges.cranchecks.info/flavor/r-devel-windows-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-release-windows-x86_64 | [![CRAN check - r-release-windows-x86_64](https://badges.cranchecks.info/flavor/r-release-windows-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-oldrel-windows-x86_64 | [![CRAN check - r-oldrel-windows-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-windows-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |

[![MacOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=apple&logoColor=white)](https://cran.r-project.org/web/checks/check_results_germinationmetrics.html)

|  |  |
|:---|----|
| r-release-macos-x86_64 | [![CRAN check - r-release-macos-x86_64](https://badges.cranchecks.info/flavor/r-release-macos-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |
| r-oldrel-macos-x86_64 | [![CRAN check - r-oldrel-macos-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-macos-x86_64/PGRdup.svg)](https://cran.r-project.org/web/checks/check_results_PGRdup.html) |

## Citing `PGRdup`

To cite the methods in the package use:

``` r
citation("PGRdup")
```

    To cite the R package 'PGRdup' in publications use:

      Aravind, J., Radhamani, J., Kalyani Srinivasan, Ananda Subhash, B., and Tyagi, R.
      K.  (2026).  PGRdup: Discover Probable Duplicates in Plant Genetic Resources
      Collections. R package version 0.3.0,
      https://github.com/aravind-j/PGRdup,https://cran.r-project.org/package=PGRdup.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {PGRdup: Discover Probable Duplicates in Plant Genetic Resources Collections},
        author = {J. Aravind and J. Radhamani and {Kalyani Srinivasan} and B. {Ananda Subhash} and Rishi Kumar Tyagi},
        note = {R package version 0.3.0 https://github.com/aravind-j/PGRdup, https://cran.r-project.org/package=PGRdup},
        year = {2026},
      }

    This free and open-source software implements academic research by the authors and
    co-workers. If you use it, please support the project by citing the package.

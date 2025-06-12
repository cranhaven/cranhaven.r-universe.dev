
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ramlegacy

[![Travis Build
Status](https://travis-ci.com/ropensci/ramlegacy.svg?branch=master)](https://travis-ci.com/ropensci/ramlegacy)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/ropensci/ramlegacy?branch=master&svg=true)](https://ci.appveyor.com/project/kshtzgupta1/ramlegacy)
[![Coverage
status](https://codecov.io/gh/ropensci/ramlegacy/branch/master/graph/badge.svg)](https://codecov.io/github/ropensci/ramlegacy)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://badges.ropensci.org/264_status.svg)](https://github.com/ropensci/software-review/issues/264)

  - **Authors**: Kshitiz Gupta, [Carl
    Boettiger](https://www.carlboettiger.info/)
  - **License**: [MIT](https://opensource.org/licenses/MIT)
  - [Package source code on
    Github](https://github.com/ropensci/ramlegacy)
  - [**Submit Bugs and feature
    requests**](https://github.com/ropensci/ramlegacy/issues)

`ramlegacy` is an R package that supports caching and reading in
different versions of the RAM Legacy Stock Assessment Data Base, an
online compilation of stock assessment results for commercially
exploited marine populations from around the world. More information
about the database can be found [here.](https://www.ramlegacy.org/)

## What does `ramlegacy` do?

  - Provides a function `download_ramlegacy()`, to download all the
    available versions of the RAM Legacy Stock Assessment Excel Database
    and cache them on the user’s computer as serialized RDS objects.
    This way once a version has been downloaded it doesn’t need to be
    re-downloaded for subsequent analysis.
  - Supports reading in specified tables or all tables from a cached
    version of the database through a function `load_ramlegacy()`
  - Provides a function `ram_dir()` to view the path of the location
    where the downloaded database was cached.

## Installation

You can install the development version from
[Github](https://github.com/ropensci/ramlegacy) with:

``` r
install.packages("devtools")
library(devtools)
install_github("ropensci/ramlegacy")
```

To ensure that the vignette is installed along with the package make
sure to remove `--no-build-vignettes` from the `build_opts` in
`install_github`

## Usage

Please see the ramlegacy
[vignette](https://ropensci.github.io/ramlegacy/articles/ramlegacy.html)
for more detailed examples and additional package functionality.

Start by loading the package using `library`.

``` r
library(ramlegacy)
```

### download\_ramlegacy

`download_ramlegacy()` downloads the specified version of **RAM Legacy
Stock Assessment Excel Database** and then saves it as an RDS object in
user’s application data directory as detected by the
[rappdirs](https://CRAN.R-project.org/package=rappdirs) package. This
location is also where `load_ramlegacy()` by default will look for the
downloaded database.

``` r
# downloads version 4.44
download_ramlegacy(version = "4.44")
```

If version is not specified then `download_ramlegacy` defaults to
downloading current latest version (4.44) :

``` r
# downloads current latest version 4.44
download_ramlegacy()
```

The latest versions of the RAM Legacy Database are [archived in
Zenodo](https://zenodo.org/communities/rlsadb/) but the older versions
(v4.3, v3.0, v2.5, v2.0, v1.0) are not. To ensure access to these older
versions of the database `download_ramlegacy` supports downloading them
from this [Github
repository](https://www.github.com/kshtzgupta1/ramlegacy-assets/):

``` r
# downloads older version 4.3
download_ramlegacy(version = "4.3")
```

### load\_ramlegacy

After the specified version of the database has been downloaded and
cached on your local machine through `download_ramlegacy` you can call
`load_ramlegacy` to obtain a list of specific tables/all the tables from
that version of the database. If version is not specified but tables is
then `load_ramlegacy` defaults to returning a list containing the
specified dataframes from the latest version (currently 4.44). If both
version and tables are not specified then `load_ramlegacy` defaults to
returning a list containing all the dataframes in the latest version
(currently 4.44)

``` r
# get a list containing area and bioparams tables from
# version 4.3 of the database
load_ramlegacy(version = "4.3", tables = c("area", "bioparams"))

# get a list containing area and bioparams tables from version 4.44
# of the database
load_ramlegacy(version = "4.44", tables = c("area", "bioparams"))

# if tables is specified but version is not then the function defaults
# to returning a list containing the specified tables from the current
# latest version 4.44
load_ramlegacy(tables = c("area", "bioparams"))

# since both tables and version are not specified the function returns
# a list containing all the tables from the current latest version 4.44
load_ramlegacy()
```

To learn more about the different tables present in the database, what
the various acronyms mean and the different stock summaries accompanying
the databases please see this
[page.](https://ropensci.github.io/ramlegacy/articles/tables_description.html)

### ram\_dir

To view the exact path where a certain version of the database was
downloaded and cached by `download_ramlegacy` you can run `ram_dir(vers
= 'version')`, specifying the version number inside the function call:

``` r
# download version 4.44
download_ramlegacy(version = "4.44")

# view the location where version 4.44 of the database was
# downloaded and cached
ram_dir(vers = "4.44")
```

## Similar Projects

1.  [`ramlegacy`](https://github.com/seananderson/ramlegacy) Sean
    Anderson has a namesake package that appears to be a stalled project
    on Github (last updated 9 months ago). However, unlike this package
    which supports downloading and reading in the Excel version of the
    database, Sean Anderson’s project downloads the Microsoft Access
    version and converts it to a local sqlite3 database.

2.  [`RAMlegacyr`](https://github.com/ashander/RAMlegacyr) `RAMlegacyr`
    is an older package last updated in 2015. Similar to Sean Anderson’s
    project, the package seems to be an R interface for the Microsoft
    Access version of the RAM Legacy Stock Assessment Database and
    provides a set of functions using RPostgreSQL to connect to the
    database.

## Citation

Current and older versions of the RAM Legacy Database are [archived in
Zenodo](https://zenodo.org/communities/rlsadb/), each version with its
own unique DOI. The suggested format for citing data is:

RAM Legacy Stock Assessment Database. 2018. Version
4.44-assessment-only. Released 2018-12-22. Accessed \[Date accessed
YYYY-MM-DD\]. Retrieved from
[DOI:10.5281/zenodo.2542919.](https://zenodo.org/record/2542919#.XE-rFs9KjBI)

The primary publication describing the RAM Legacy Stock Assessment
Database, and suggested citation for general use is:

Ricard, D., Minto, C., Jensen, O.P. and Baum, J.K. (2012) Evaluating the
knowledge base and status of commercially exploited marine species with
the RAM Legacy Stock Assessment Database. Fish and Fisheries 13 (4)
380-398.
[DOI: 10.1111/j.1467-2979.2011.00435.x](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-2979.2011.00435.x)

Several [publications](https://sites.uw.edu/ramlegac/publications/) have
relied on the RAM Legacy Stock Assessment
Database.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)

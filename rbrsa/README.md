
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbrsa: An R Package for Turkish Banking Sector Data

[![Documentation](https://img.shields.io/badge/docs-pkgdown-blue)](https://obakis.github.io/rbrsa/)

An R package for programmatic access to Turkish banking sector data from
the [Turkish Banking Regulation and Supervision
Agency](https://www.bddk.org.tr) (BRSA, known as BDDK in Turkish). The
package provides R users with a clean interface to fetch monthly and
quarterly banking statistics, financial reports, and sectoral indicators
directly from BRSA’s official APIs.

## Key Features

- Direct API access to BRSA monthly bulletins (17 financial tables)
- Quarterly FinTurk data with city-level granularity (7 tables, 82
  cities including 'HEPSI’ for all cities)
- Consistent parameter interface for both data sources
- Built-in metadata for tables, banking groups, and provinces
- Multiple export formats: RDS, CSV, Excel via `save_data()`
- Returns base R data frames ready for analysis

## Design Philosophy

**Lightweight and Authentic:** Other packages providing access to BDDK
data (like `bddkR`) also fetch data programmatically, but they add a
heavy translation layer -maintaining manual configuration files to map
Turkish column names and categorical values to English. This provides
user convenience at a high maintenance cost.

`rbrsa` takes a different path. It interacts directly with the API and
uses the data it returns with minimal alteration:

- For the **Monthly Bulletin**, it uses the **official English column
  names and labels** provided by the API when `lang = "en"` is set.
- For the **FinTurk dataset**, where the API provides data only in
  Turkish, it returns the **authentic Turkish names**.

**This is a deliberate choice.** By avoiding a separate translation
file, `rbrsa` eliminates a major maintenance burden, aiming to adapt
instantly to any API changes. This way, the data you see is exactly what
the official source provides.

## Related Packages

- [bddkR](https://github.com/ozancanozdemir/bddkR) (R): Uses manual
  configuration for translations and column mappings and provides access
  only to the Monthly Bulletin.
- [bddk](https://github.com/barbasan/bddk) (Python): Provides similar
  functionality for Python users with the same constraints as bddkR.  
- [bddkdata](https://github.com/urazakgul/bddkdata) (Python): Provides
  similar functionality for Python/pandas users with the same
  constraints as bddkR.  
- [pybrsa](https://github.com/obakis/pybrsa) (Python, forthcoming):
  Python companion to this package with consistent API.

## Installation

You can install from CRAN

``` r
install.packages("rbrsa")
```

The development version can be installed from GitHub:

``` r
#install.packages("pak")
pak::pkg_install("obakis/rbrsa")
```

## Getting started

*Full package documentation with function references is available at*:
<https://obakis.github.io/rbrsa/>

The `rbrsa` package retrieves tables from two distinct publication
portalsmaintained by the Turkish Banking Regulation and Supervision
Agency (BDDK). Both portals are official sources, but they organize the
data differently:

- The [Monthly Bulletin Portal](https://www.bddk.org.tr/bultenaylik/)
  provides high-level, summary reports designed for general consumption
  and quick overviews of monthly trends without any geographic coverage.
- The [FinTurk Data System](https://www.bddk.org.tr/BultenFinTurk/)
  provides granular, detailed data, including statistics broken down by
  province, whereas the standard Monthly Bulletin offers national-level
  aggregates.

``` r
library(rbrsa)
## Explore available tables
list_tables("bddk") # list_tables("bddk","tr")
list_tables("finturk")  # list_tables("finturk", "tr")
list_groups("bddk")  
list_groups("bddk","tr")  
list_groups("finturk")

## Monthly data (Table 15: Ratios)
data <- fetch_bddk(2024, 1, 2024, 3, table_no = 15, grup_kod = 10001)

## Quarterly FinTurk data
q_data <- fetch_finturk(2024, 3, 2024, 9, table_no = 1, grup_kod = 10007)

## Save results
temp_file <- tempfile() # filename should be without extension
save_data(q_data, temp_file, format = "csv")
```

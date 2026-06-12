
# nysOpenData

`nysOpenData` provides simple, reproducible access to datasets from
the [NY State Open Data](https://data.ny.gov/) platform — directly from
R, with **no API keys** or manual downloads required. The package is
available on **CRAN**.

Version **0.1.1** introduces a streamlined, catalog-driven interface for
NYS Open Data.

The package provides three core functions:

- `nys_list_datasets()` — Browse available datasets from the live nys
  Open Data catalog
- `nys_pull_dataset()` — Pull any cataloged dataset by key, with
  filtering, ordering, and optional date controls
- `nys_any_dataset()` — Pull any NYS Open Data dataset directly via its
  Socrata JSON endpoint

Datasets pulled via `nys_pull_dataset()` automatically apply sensible
defaults from the catalog (such as default ordering and date fields),
while still allowing user control over:

- limit
- filters
- date / from / to
- where
- order
- clean_names
- coerce_types

This redesign reduces maintenance burden, improves extensibility, and
provides a more scalable interface for working with nys Open Data.

All functions return clean **tibble** outputs and support filtering
via  
`filters = list(field = "value")`.

------------------------------------------------------------------------

## Installation

### Development version (GitHub)

``` r
devtools::install_github("martinezc1/nysOpenData")
```

------------------------------------------------------------------------

## Example

``` r
library(nysOpenData)

data <- nys_pull_dataset(dataset = "lottery_cash_4_life_winning_numbers_beginning_2014", limit = 5000)

head(data)
```

    ## # A tibble: 6 × 3
    ##   draw_date           winning_numbers cash_ball
    ##   <dttm>              <chr>               <dbl>
    ## 1 2026-02-21 00:00:00 20 25 30 52 55          4
    ## 2 2026-02-20 00:00:00 02 28 49 50 56          1
    ## 3 2026-02-19 00:00:00 51 54 57 59 60          3
    ## 4 2026-02-18 00:00:00 03 28 38 40 49          1
    ## 5 2026-02-17 00:00:00 11 22 32 56 60          4
    ## 6 2026-02-16 00:00:00 02 11 14 15 22          1

## About

`nysOpenData` makes New York State’s civic datasets accessible to
students,  
educators, analysts, and researchers through a unified and user-friendly
R interface.  
Developed to support reproducible research, open-data literacy, and
real-world analysis.

------------------------------------------------------------------------

## Comparison to Other Software

While the [`RSocrata`](https://CRAN.R-project.org/package=RSocrata)
package provides a general interface for any Socrata-backed portal,
`nysOpenData` is specifically tailored for **New York State Open Data**.

This package is part of a broader ecosystem of tools for working with New York open data:

- `nycOpenData` — streamlined access to NYC Open Data  
- `nysOpenData` — streamlined access to NY State Open Data  

Together, these packages provide a consistent, user-friendly interface for working with civic data across jurisdictions.

- **Ease of Use**: No need to hunt for 4x4 dataset IDs (e.g., `kwxv-fwze`); use catalog-based keys instead.
- **Open Literacy**: Designed specifically for students and researchers to lower the barrier to entry for civic data analysis.

------------------------------------------------------------------------

## Contributing

We welcome contributions! If you find a bug or would like to request a
wrapper for a specific nys dataset, please open an issue or submit a
pull request on [GitHub](https://github.com/martinezc1/nysOpenData).

------------------------------------------------------------------------

## Authors & Contributors

### Maintainer

**Christian A. Martinez** 📧 <c.martinez0@outlook.com>  
GitHub: [@martinezc1](https://github.com/martinezc1)

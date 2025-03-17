
<!-- README.md is generated from README.Rmd. Please edit that file -->

# noegletalR

<!-- badges: start -->

[![R-CMD-check](https://github.com/FrLars21/noegletalR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FrLars21/noegletalR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of noegletalR is to provide a simple interface between R and
‘noegletal.dk’, a Danish government website providing many interesting
variables on Danish municipalities.

## Installation

``` r
install.packages("noegletalR")
```

### Development version

You can install the development version of noegletalR from GitHub with:

``` r
# install.packages("pak")
pak::pak("FrLars21/noegletalR")
```

## TODO

In no particular order, these are the features that I will work towards
implementing in the near future:

- Full test coverage for all functions (especially ensuring accurate
  parsing of ‘noegletal.dk’ data in diverse situations!).
- More thoughrough validation and error handling (especially for failed
  http requests).
- Response caching (perhaps 1 day for both `noegletal_get()` and
  `noegletal_vars()`, with the possibility to use `ignore_cache=True`?).
- Improving the UX for browsing `noegletal_vars()`.
- Possibly better output formats (i.e. not including years with
  all-empty rows in the output tibbles?)
- More options in `noegletal_get()` (e.g. ‘Pris- og lønregulering (PL)’,
  ‘Absolut- og pct. vækst’ & ‘Indeks-tal’)

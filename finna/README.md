# finna

[![R-CMD-check](https://github.com/fennicahub/finna/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/fennicahub/finna/actions/workflows/check-standard.yaml)
[![issues](https://img.shields.io/github/issues/fennicahub/finna)](https://github.com/fennicahub/finna/issues)
[![pulls](https://img.shields.io/github/issues-pr/fennicahub/finna)](https://github.com/fennicahub/finna/pulls)


The `finna` package provides tools to access and analyze metadata from the Finna API, which aggregates content from Finnish archives, libraries, and museums.


## Installation instructions

Install the released version from CRAN:

```r
install.packages("finna")
```

The devel version of finna can be installed from GitHub as follows:

``` r
# Install finna if not already installed
if (!requireNamespace("finna", quietly = TRUE)) {
  remotes::install_github("fennicahub/finna")
}
```

``` r
remotes::install_github("fennicahub/finna")
```

## Example
The basic functionality of finna can be explored as follows:

**N.B** In the search_finna() default limit of 100 records is being used. Specify 'limit' argument for more records.

``` r
# Load the package
library(finna)
# Perform a simple search and print a table

record <- search_finna("sibelius")
head(record)
```
| Title                                                                                        | Author           | Year | Language | Formats           | Subjects          | Library             | Series |
|:---------------------------------------------------------------------------------------------|:-----------------|:-----|:---------|:------------------|:------------------|:--------------------|:-------|
| Sibelius favourites: Sibelius collection                                                     | Sibelius         | 2001 | N/A      | Äänite (audio)    | Orkesterimusiikki  | Lapin               | N/A    |
| SIBELIUS                                                                                     | TAWASTSTJERNA    | 1997 | Finnish  | Kirja (book)      | SIBELIUS           | Anders Chydenius    | N/A    |
| Sibelius                                                                                     | TAWASTSTJERNA    | 1997 | Finnish  | Kirja (book)      | Sibelius           | Anders Chydenius    | N/A    |
| Sibelius                                                                                     | Lampila          | 1984 | Finnish  | Kirja (book)      | Sibelius           | Helka-arkisto       | N/A    |
| Sibelius                                                                                     | TAWASTSTJERNA    | 2003 | Finnish  | Kirja (book)      | Sibelius           | Kansalliskirjasto   | N/A    |
| Sibelius                                                                                     | Ringbom          | 1948 | Finnish  | Kirja (book)      | Sibelius           | Kirkes              | N/A    |

To search all related in descending order
``` r
record <- search_finna("sibelius", sort = "main_date_str des")
head(record)
```
| Title                                                                                  | Author                    | Year | Language | Formats         | Subjects      | Library         | Series   |
|:---------------------------------------------------------------------------------------|:--------------------------|:-----|:---------|:----------------|:--------------|:----------------|:---------|
| He selvisivät sodasta                                                                  | Kirves, Jenni, Werner Söderström | 2024 | Finnish  | Kirja (book)    | Sotilaat       | Anders Chydenius | N/A      |
| Yli-ihmisiä ja traagisia kuolevaisia: esseitä ja kirjoituksia 1901-1945                | Frosterus, Sigurd, Sarje Maaria | 2024 | Finnish  | Kirja (book)    | Wagner, etc.   | Helka-arkisto    | N/A      |
| Eero Järnefelt                                                                         | Järnefelt, Eero, Selkokari | 2024 | Finnish  | Kirja (book)    | Järnefelt      | Anders Chydenius | Ateneum  |
| Eero Järnefelt                                                                         | Järnefelt, Eero, Selkokari | 2024 | Swedish  | Kirja (book)    | Järnefelt      | Helle-kirjastot  | Ateneum  |
| Eero Järnefelt                                                                         | Järnefelt, Eero, Selkokari | 2024 | English  | Kirja (book)    | Järnefelt      | Helmet-kirjasto  | Ateneum  |
| Solace                                                                                 | N/A                        | 2024 | No language (zxx) | Äänite (audio)  | N/A            | Anders Chydenius | N/A      |


## Case studies

The analysis codes in case studies 1-2 are included in the inst/extras or [here](https://github.com/fennicahub/finna/tree/main/inst/extras)

## Contribute

Contributions are very welcome:

- [Use issue tracker](https://github.com/fennicahub/finna/issues) for
  feedback and bug reports.
- [Send pull requests](https://github.com/fennicahub/finna/)
- [Star us on the Github page](https://github.com/fennicahub/finna/)

## Acknowledgements

This work has been supported by the Research Council of Finland (decisions 358720, 348946). The work is part of [FIN-CLARIAH](https://www.kielipankki.fi/organization/fin-clariah/) research infrastucture for digital humanities. This functionality related to music data has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 101095295 ([OpenMUSE](https://www.openmuse.eu/)).

### Disclaimer

This package is in no way officially related to or endorsed by Finna.

When using metadata retrieved from Finna database in your work, please
indicate that the metadata source is Finna. If your re-use involves some
kind of modification to data or text, please state this clearly to the
end user. See Finna policy on [copyright and free re-use of
metadata](https://www.finna.fi/Content/terms?lng=en-gb) for more
detailed information and certain exceptions.

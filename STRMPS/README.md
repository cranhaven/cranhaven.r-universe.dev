# STRMPS
The `STRMPS` package is designed to extract and collect the short tandem repeat (STR) information from the `fastq` files produced by massively parallel sequencing (MPS). 

## Installation

The `STRMPS`-package depends on `R` (>= 4.4), `methods`, `utils`, `tidyr`, `tibble`, `dplyr`, `stringr`, `purrr`, `parallel`, as well as the bioconductor packages `Biostrings`, `pwalign`, `ShortRead`, and `IRanges`. Version 0.5.8 is available on CRAN, but to get the newest version devtools is needed to install the package from github. 

From R, run the following commands:  

```r
install.packages("devtools")
devtools::install_github("svilsen/STRMPS")
```

## License

This project is licensed under the MIT License.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# eider <img src="man/figures/eider_hexagon.png" align="right" height="138" style="max-height: 138px" />

<!-- badges: start -->
<!-- badges: end -->

**eider** is an R package for extracting machine learning features from
tabular data, in particular health records, in a declarative manner.

Features are specified as JSON objects which contain all the necessary
information required to perform a given calculation. For example, the
following calculates the number of total rows per patient `id` in the
table labelled `ae2` (details on how to specify this table are in the
function documentation).

``` json
{
  "source_table": "ae2",
  "transformation_type": "COUNT",
  "grouping_column": "id",
  "absent_default_value": 0,
  "output_feature_name": "total_ae_attendances"
}
```

The output of this is a column named `total_ae_attendances`, containing
the number of rows per patient, and with a value of 0 for any patients
who do not appear in the `ae2` table.

This declarative approach provides an alternative to traditional,
imperative-style, `dplyr` pipelines which can be more difficult to
reason about, especially when a series of features is being extracted
and merged together. As features are specified without reference to a
specific programming language or paradigm, it also encourages code that
is concise, easy to read, and maintainable.

`eider` is a collaboration between The Alan Turing Institute, Public
Health Scotland, and the Universities of Edinburgh and Durham. It grew
out of a desire to generalise the feature extraction process for health
data, specifically the [SPARRA *(Scottish Patients At Risk of
Readmission and Admission)*
project](https://www.gov.scot/publications/sparra-made-easy/) ([GitHub
repo](https://github.com/jamesliley/SPARRAv4)), and to allow similar
analyses to be carried out in different contexts.

## Installation

Install via CRAN:

``` r
install.packages("eider")
```

Alternatively, install `eider` from its source code on
[GitHub](https://github.com/alan-turing-institute/eider) using:

``` r
install.packages("devtools")
devtools::install_github("alan-turing-institute/eider", build_vignettes = TRUE)
```

## Documentation

The package documentation is available
[online](https://alan-turing-institute.github.io/eider/). In particular,
the [package
articles](https://alan-turing-institute.github.io/eider/articles/)
contain a series of vignettes which provide detailed guidance on the
package and its features.

## Development

If you are making changes to the library itself, first clone the
repository:

    git clone git@github.com:alan-turing-institute/eider.git

You will need to install the `lintr`, `pkgdown`, `devtools` R packages
to build documentation, run tests, and lint. Then, from the repository
root, you can use the following commands:

- `make doc` generates all function documentation, and also generates
  the `README.md` file from `README.rmd`
- `make lint` lints the project directory
- `make test` runs all tests

You can also use [`pre-commit`](https://pre-commit.com/) to run all of
these before committing, to ensure that you do not commit incomplete
code. Firstly, install `pre-commit` according to the instructions on the
webpage above. Then run `pre-commit install`.

*What about vignettes?* Well, building vignettes is slightly more
complicated. You can perform a one-time build from the R console using
`pkgdown::build_site()`, but running this every time you edit a file
gets tiring quickly. To automate this, first install the package with
`make install`, and install a working version of Python and also
[`entr`](https://github.com/eradman/entr) (the latter is available on
Homebrew via `brew install entr`). Then run `make vig`: this will
monitor your vignette RMarkdown files, rebuild the vignettes any time
they are changed, and launch a HTTP server on port 8000 to view the
files. If you change any library code you will have to run
`make install` again before rerunning `make vig`.

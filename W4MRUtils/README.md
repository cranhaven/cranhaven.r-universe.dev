
# W4MRUtils

  - **VERSION**: 1.0.0

<!-- badges: start -->

<!-- badges: end -->

W4MRUtils is a R packages provided by W4M to ease galaxy tools writing.
It contains some utility functions that will help you in common tasks.

## Parameters

  - Parsing parameters with parse\_args (easier)
  - Parsing parameters with optparse\_parameters (better)
  - Enforce checking on parameters with check\_param\_type\_n\_length

## R script chores

  - Silently load a package with shy\_lib
  - Sourcing relative file with source\_local

## Galaxy

  - Am I in a galaxy env? see in\_galaxy\_env
  - Show log headers with show\_galaxy\_header
  - Show log footer with show\_galaxy\_footer
  - Execute a function with run\_galaxy\_processing
  - Restore parameters names, modified by galaxy with
    unmangle\_galaxy\_param

## Logfiles

  - What is a logger? - See logging section what-is-a-logger)
  - How to create a logger? - See logging section
    how-to-create-a-logger)
  - How to create a log file? - See logging section
    how-to-create-a-log-file)

## TODO

Do the documentation and the referencing of the documentation for:

  - stock\_id
  - reproduce\_id
  - check\_err
  - match2
  - match3
  - import2
  - import3
  - df\_is
  - df\_force\_numeric
  - df\_read\_table

## Installation

You can install the development version of W4MRUtils like so:

``` bash
$ git clone https://github.com/workflow4metabolomics/W4MRUtils
$ cd W4MRUtils
```

then

``` bash
$ make install
```

or

``` r
> rmarkdown::render("README.Rmd")
> devtools::document(".")
> roxygen2::roxygenize(".")
> devtools::test(".")
> devtools::install(".", dependencies = FALSE, repos = NULL, type = "source")
```

or

``` bash
$ R -q -e "install.packages('W4MRUtils', repos='https://cran.irsn.fr');"
```

## Uninstallation

You can uninstall the version of W4MRUtils you installed with:

``` bash
$ make remove_package
```

or

``` r
> remove.packages("W4MRUtils")
```

## XML Wrapper

Please follow [the
guidelines](https://galaxy-iuc-standards.readthedocs.io/en/latest/best_practices/tool_xml.html)
during the redaction of the xml wrapper.

Read [the doc](https://docs.galaxyproject.org/en/latest/dev/schema.html)
in case of problems.

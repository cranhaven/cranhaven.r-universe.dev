# admiral.test

#### `admiral.test` v0.7.0  will be the final version. At the end of 2023, the package will be archived in favor of [pharmaversesdtm](https://github.com/pharmaverse/pharmaversesdtm/). 

Test data for the `{admiral}` package taken from the [CDISC pilot project](https://github.com/cdisc-org/sdtm-adam-pilot-project) and renamed with `admiral_` prefix for clarity.
As this mostly contains safety data only, we extend this as needed by adding further test data required such as for PK and TA-specific efficacy analyses.
See the "How To Update" section below for more details.

# Installation

The package is available from CRAN and can be installed by running `install.packages("admiral.test")`.

To install the latest development version of the package directly from GitHub use the following code:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("pharmaverse/admiral.test", ref = "devel")
```

# How To Update

Firstly, make a GitHub issue in this repo with the planned updates and tag `@pharmaverse/admiral` so that one of the development core team can sanity check the request.
Then there are two main ways to extend the test data, either by adding new datasets or extending existing datasets with new records/variables. Whichever method you choose, it is worth noting the following:

* Programs that generate test data are stored in the `dev/` folder.
* Each of these programs is written as a standalone R script: if any packages need to be loaded for a given program, then call `library()` at the start of the program (but please do __not__ call `library(admiral.test)`).
* Most of the packages that you are likely to need will already be specified in the `renv.lock` file, so they will already be installed if you have been keeping in sync--you can check this by entering `renv::status()` in the Console. However, you may also wish to install `{metatools}` and `{ggplot2}`, which are currently not specified in the `renv.lock` file. If you feel that you need to install any other packages in addition to those just mentioned, then please tag `@pharmaverse/admiral` to discuss with the development core team.
* When you have created a program in the `dev/` folder, you need to run it as a standalone R script, in order to generate a test dataset that will become part of the `{admiral.test}` package, but you do not need to build the package.
* Following [best practice](https://r-pkgs.org/data.html#sec-data-data), each dataset is stored as a `.rda` file whose name is consistent with the name of the dataset: for example, the dataset `dm` should be renamed to `raw_dm` before saving it as `raw_dm.rda`; if you save `dm` as `raw_dm.rda` and subsequently load the `.rda` file, then `dm` (not `raw_dm`) will be loaded into the global environment.
* The programs in `dev/` are stored within the `{admiral.test}` GitHub repository, but they are __not__ part of the `{admiral.test}` package--the `dev/` folder is specified in `.Rbuildignore`.
* When you run a program that is in the `dev/` folder, you generate a dataset that is written to the `data/` folder, which will become part of the `{admiral.test}` package.
* The names of test datasets are specified in `R/data.R`, for the purpose of generating documentation in the `man/` folder.

## Adding New SDTM Datasets

* Create a program in the `dev/` folder, named `<name>.R`, where `<name>` is the SDTM domain name, (e.g. `rs.R`), to generate the test data and output `admiral_<name>.rda` to the `data/` folder. Use CDISC pilot data such as `admiral_dm` as input in this program in order to create realistic synthetic data that remains consistent with other domains. Note that __no personal data should be used__ as part of this package, even if anonymized.
* Run the program.
* Reflect this update, by specifying `admiral_<name>` in `R/data.R`.
* Run `devtools::document()` in order to update `NAMESPACE` and update the `.Rd` files in `man/`.

## Updating Existing SDTM Datasets

* Rename the source dataset as `raw_<name>`, where `<name>` is the SDTM domain name (e.g. rename `ds` to `raw_ds`), and then save it to the `data/` folder as `raw_<name>.rda` (e.g. `save(raw_ds, file = "data/raw_ds.rda")`).
* Create a program in the `dev/` folder, named `update_<name>.R`, to load `raw_<name>.rda`, make the updates, and output `admiral_<name>.rda` to the `data/` folder.
* Run the program.
* Reflect this update, by specifying both `raw_<name>` and `admiral_<name>` in `R/data.R`.
* Run `devtools::document()` in order to update `NAMESPACE` and update the `.Rd` files in `man/`.

# Amber R

[![Build Status](https://app.travis-ci.com/obiba/amberr.svg?branch=master)](https://app.travis-ci.com/github/obiba/amberr)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/amberr)](https://cran.r-project.org/package=amberr)

R package for accessing [OBiBa/Amber](https://www.obiba.org/pages/products/amber/) web services:

* list users and groups
* list studies, forms, form revisions, case report forms and interview designs
* extract case report and interview records with full data dictionary

## Installation

Requires R >3.x.

```
# Install from source code repository
remotes::install_github("obiba/amberr")
```

## Usage

Steps:

* open connection to Amber server
* extract and process documents
* close connection with Amber server

Examples: 

* [user and group queries](https://github.com/obiba/amberr/blob/master/inst/examples/amber-user-group-queries.R)
* [study and form queries](https://github.com/obiba/amberr/blob/master/inst/examples/amber-study-form-queries.R)
* [case report form and record queries](https://github.com/obiba/amberr/blob/master/inst/examples/amber-case-report-queries.R) and save them in [Opal](https://www.obiba.org/pages/products/opal/) for further analysis
* [interview designs, campaigns, participants and record queries](https://github.com/obiba/amberr/blob/master/inst/examples/amber-interview-queries.R) and save them in [Opal](https://www.obiba.org/pages/products/opal/) for further analysis


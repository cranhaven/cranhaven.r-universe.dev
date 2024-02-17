# odns - access Open Data from NHS Scotland

<!-- badges: start -->

<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active – The project has reached a stable, usable state and is being actively developed." /></a>
[![CRAN status](https://cranchecks.info/badges/flavor/release/odns)](https://cran.r-project.org/web/checks/check_results_odns.html)
[![R-CMD-check](https://github.com/jrh-dev/odns/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jrh-dev/odns/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/jrh-dev/odns/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jrh-dev/odns/actions/workflows/test-coverage.yaml)
[![downloads](https://cranlogs.r-pkg.org/badges/odns)](https://www.rdocumentation.org/trends)
[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

<!-- badges: end -->

`odns` provides a base for exploring and obtaining data available through the [Scottish Health and Social Care Open Data platform](https://www.opendata.nhs.scot/). The package provides a wrapper for the underlying [CKAN](https://ckan.org) API and simplifies the process of accessing the available data with R, allowing users to quickly explore the available data and start using it without having to write complex queries.

# Installation
Install `odns` from CRAN;
```
install.packages("odns")
```
&nbsp;

Install `odns` from GitHub;

```
devtools::install_github("https://github.com/jrh-dev/odns")
```

# Usage

## The Language of CKAN
CKAN and by extension this package refers to *packages* and *resources*. 

The term *package* refers to a dataset, a collection of *resources*. A *resource*, contains the data itself.

Example of CKAN structure;

```
CKAN
│
├── package_1
│   ├── resource_1
│   ├── resource_2
│   └── resource_3
|
└── package_2
    ├── resource_1
    └── resource_2
```
&nbsp;

## Exploring the available data

To view available packages in a data.frame along with the package ID;

```
#' view all available packages

> all_packages()
  
# package_name                                      package_id
# covid-19-vaccination-in-scotland                  6dbdd466-…
# enhanced-surveillance-of-covid-19-in-scotland     3c5231ee-…
# hospital-onset-covid-19-cases-in-scotland         d67b13ef-…
# weekly-covid-19-statistical-data-in-scotland      524b42b4-…
# covid-19-in-scotland                              b318bddf-…
# … with 85 more rows


#' limit the return by specifying a search string

> all_packages(contains = "population")

# package_name                           package_id                          
# population-estimates                   7f010430-6ce1-4813-b25c-f7f335bdc4dc
# standard-populations                   4dd86111-7326-48c4-8763-8cc4aa190c3e
# population-projections                 9e00b589-817e-45e6-b615-46c935bbace0
# gp-practice-populations                e3300e98-cdd2-4f4e-a24e-06ee14fcc66c
# scottish-index-of-multiple-deprivation 78d41fa9-1a62-4f7b-9edb-3e8522a93378
```

&nbsp;

To view details of the all available resources;

```
#' view all available resources
> all_resources()

#  resource_name        resource_id package_name package_id url   last_modified    
#  Daily Trend of Tota… 42f17a3c-a… covid-19-va… dbdd466-…  http… 2022-07-06T1…
#  Daily Trend of Vacc… 9b99e278-b… covid-19-va… dbdd466-…  http… 2022-07-06T1…
#  Daily Trend of Vacc… 758f72d6-7… covid-19-va… dbdd466-…  http… 2022-07-06T1…
#  Daily Trend of Vacc… 09f5073d-2… covid-19-va… dbdd466-…  http… 2022-07-06T1…
#  Daily Trend of Vacc… 8f7b64b1-e… covid-19-va… 6dbdd46-…  http… 2022-07-06T1…
# … with 720 more rows


#' view all resources within packages whose names contain "population"

> all_resources(package_contains = "population")

# resource_name         resource_id package_name package_id url   last_modified
# Data Zone (2011) Pop… c505f490-c… population-… 7f010430-… http… 2021-10-11T1…
# Intermediate Zone (2… 93df4c88-f… population-… 7f010430-… http… 2021-10-11T1…
# Council Area (2019) … 09ebfefb-3… population-… 7f010430-… http… 2021-07-06T0…
# Health and Social Ca… c3a393ce-2… population-… 7f010430-… http… 2021-07-06T0…
# Health Board (2019) … 27a72cc8-d… population-… 7f010430-… http… 2021-07-06T0…
# … with 53 more rows

#' view all resources whose names contain "population"

> all_resources(resource_contains = "european")

# resource_name         resource_id package_name package_id url   last_modified
# Population mortality… ec2af2be-8… hospital-st… c88a5231-… http… 2022-05-10T0…
# GP Practice Populati… 2c701f90-c… gp-practice… e3300e98-… http… 2022-05-10T0…
# GP Practice Populati… d07debcf-7… gp-practice… e3300e98-… http… 2022-02-07T1…
# GP Practice Populati… 4a3c438b-2… gp-practice… e3300e98-… http… 2021-11-02T1…
# GP Practice Populati… 0779e100-1… gp-practice… e3300e98-… http… 2022-02-17T1…
# … with 45 more rows

#' view all resources within packages whose names contain "population" and where 
#' the resource name contains contain "european"

> all_resources(package_contains = "population", resource_contains = "european")

# resource_name          resource_id package_name package_id url   last_modified
# European Standard Pop… edee9731-d… standard-po… 4dd86111-… http… 2018-04-05T1…
# European Standard Pop… 29ce4cda-a… standard-po… 4dd86111-… http… 2018-04-05T1…

```

In the examples above the search strings are **NOT** case sensitive.

&nbsp;

## Viewing package and resource metadata

Package and resource metadata contains useful information about the available data. To view metadata;

```
#' view metadata for a package using a valid package name

> package_metadata(package = "standard-populations")

# $nhs_language
# [1] "English"
# 
# $license_title
# [1] "UK Open Government Licence (OGL)"
# 
# $maintainer
# [1] ""
# 
# $version
# [1] ""
#
#...

#' view metadata for a package using a valid package id

> package_metadata(package = "4dd86111-7326-48c4-8763-8cc4aa190c3e")

# $nhs_language
# [1] "English"
# 
# $license_title
# [1] "UK Open Government Licence (OGL)"
# 
# $maintainer
# [1] ""
# 
# $version
# [1] ""
#
#...

#' view metadata for a resource using a valid resource id

> resource_metadata(resource="edee9731-daf7-4e0d-b525-e4c1469b8f69")

# $cache_last_updated
# named list()
#
# $cache_url
# named list()
#
# $mimetype_inner
# named list()
#
# $hash
# [1] ""
#
# $description
# [1] "Different countries across Europe have different population ...
# 
# $format
# [1] "CSV"
#...
```

To view the data items available within a resource along with their types;

```
> resource_data_items(resource="edee9731-daf7-4e0d-b525-e4c1469b8f69")
#                           id    type
# 1                        _id     int
# 2                   AgeGroup    text
# 3 EuropeanStandardPopulation numeric
```

&nbsp;

## Importing data to R

There are multiple ways to import resources into R.

### Using `get_resource`

`get_resource()` is often the quickest and simplest way to import data where all resources within one or more packages are required.

To import all resources within a package as a list with each element containing
one resource;

```
#' get all resources in a package

> get_resource(package = "4dd86111-7326-48c4-8763-8cc4aa190c3e")

#' get the first 10 rows of each resource in a package

> get_resource(package = "4dd86111-7326-48c4-8763-8cc4aa190c3e", limit = 10L)

#' both package IDs and names can be used

> get_resource(package = "standard-populations", limit = 10L)

#' multiple packages can be specified returning all resources under each

> get_resource(package = c("standard-populations", "population-projections")

```

To import specific resources;

```
#' get specific resources

> get_resource(
    resource = c("European Standard Population",
    "9e00b589-817e-45e6-b615-46c935bbace0"),
    limit = 5L
    )
    
#' get a specific resource, if it exists within a specified package

> get_resource(
    package = "standard-populations",
    resource = "European Standard Population"
    )
 
```

&nbsp;

### Using `get_data`

The `get_data()` function can be used to exact more control over the content returned, allowing for selection of specific fields, and basic filtering. `get_data()` is for use with one resource at a time and only accepts a resource ID, not a resource name.

```
# import specified fields from a resource
>  get_data(
     resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
     fields = c("AgeGroup", "EuropeanStandardPopulation")
   )

# AgeGroup     EuropeanStandardPopulati…
# 0-4 years                         5000
# 5-9 years                         5500
# 10-14 years                       5500
# 15-19 years                       5500
# 20-24 years                       6000
# … with 14 more rows
   
```

&nbsp;

The `where` argument of `get_data()` can be used to extract more specific subsets of the full resources available by passing the "WHERE" element of a SQL style query[^1].

```
#' import specified fields from a data set utilising a SQL style where query

> get_data(
    resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
    fields = c("AgeGroup", "EuropeanStandardPopulation"),
    where = "\"AgeGroup\" = \'45-49 years\'"
  )
  
# AgeGroup    EuropeanStandardPopulation
# 45-49 years                       7000
 ```
 
&nbsp;
 
[^1]: The option provided by the `get_data()` function to specify a `where` argument requires specific formatting for compatibility with the CKAN API. Field names must be double quoted `"`, non-numeric values must be single quoted `'`, and both single and double quotes must be delimited. Example; `where = "\"AgeGroup\" = \'45-49 years\\'"`.

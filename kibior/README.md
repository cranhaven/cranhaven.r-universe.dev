

![](https://github.com/regisoc/kibior/blob/master/inst/logo/kibior.png)

# kibior: easy scientific data handling, searching and sharing with Elasticsearch

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build Status](https://travis-ci.com/regisoc/kibior.svg?branch=master)](https://travis-ci.com/regisoc/kibior)

Version: `0.1.1`

## TL;DR

| | |
|-|-|
| What  | `kibior` is a R package dedicated to ease the pain of data handling in science, and more notably with biological data. | 
| Where | `kibior` is using `Elasticsearch` as database and search engine. |
| Who   | `kibior` is built for data science and data manipulation, so when any data-related action or need is involved, notably `sharing data`. It mainly targets bioinformaticians, and more broadly, data scientists. |
| When  | Available now from this repository, or [CRAN repository](https://cran.r-project.org/package=kibior). |
| Public instances | Use the `$get_kibio_instance()` method to connect to `Kibio` and access known datasets. See `Kibio datasets` at the end of this document for a complete list. |
| Cite this package | In R session, run `citation("kibior")` |
| Publication | `coming soon`. |



## Main features

This package allows:

- `Pushing`, `pulling`, `joining`, `sharing` and `searching` tabular data between an R session and one or multiple Elasticsearch instances/clusters. 
- `Massive data query and filter` with Elasticsearch engine.
- `Multiple living Elasticsearch connections` to different addresses.
- `Method autocompletion` in proper environments (e.g. R cli, RStudio). 
- `Import and export datasets` from an to files.
- `Server-side execution` for most of operations (i.e. on Elasticsearch instances/clusters).


## How

### Install

```r
# Get from CRAN
install.packages("kibior")

# or get the latest from Github
devtools::install_github("regisoc/kibior")
```

### Run

```r
# load
library(kibior)

# Get a specific instance
kc <- Kibior$new("server_or_address", port)

# Or try something bigger...
kibio <- Kibior$get_kibio_instance()
kibio$list()

```

## Examples 

Here is an extract of some of the features proposed by `KibioR`. 
See `Introduction` vignette for more advanced usage.

### Example: `push` datasets

```r
# Push data (R memory -> Elasticsearch)
dplyr::starwars %>% kc$push("sw")
dplyr::storms %>% kc$push("st")
```

### Example: `pull` datasets

```r
# Pull data with columns selection (Elasticsearch -> R memory)
kc$pull("sw", query = "homeworld:(naboo || tatooine)", 
              columns = c("name", "homeworld", "height", "mass", "species"))
# see vignette for query syntax
```

### Example: `copy` datasets

```r
# Copy dataset (Elasticsearch internal operation)
kc$copy("sw", "sw_copy")
```

### Example: `delete` datasets

```r

# Delete datasets
kc$delete("sw_copy")
```

### Example: `list`, `match` dataset names

```r
# List available datasets
kc$list()

# Search for index names starting with "s"
kc$match("s*")
```

### Example: get `columns` names and list unique `keys` in values

```r
# Get columns of all datasets starting with "s"
kc$columns("s*")

# Get unique values of a column
kc$keys("sw", "homeworld")
```

### Example: some Elasticsearch basic statistical methods 

```r
# Count number of lines in dataset
kc$count("st")

# Count number of lines with query (name of the storm is Anita)
kc$count("st", query = "name:anita")

# Generic stats on two columns
kc$stats("sw", c("height", "mass"))

# Specific descriptive stats with query
kc$avg("sw", c("height", "mass"), query = "homeworld:naboo")
```

### Example: `join`

```r
# Inner join between:
#   1/ a Elasticsearch-based dataset with query ("sw"), 
#   2/ and a in-memory R dataset (dplyr::starwars) 
kc$inner_join("sw", dplyr::starwars, 
              left_query = "hair_color:black",
              left_columns = c("name", "mass", "height"),
              by = "name")
```


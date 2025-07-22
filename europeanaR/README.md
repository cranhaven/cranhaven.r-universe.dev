
# europeanaR <a href="https://alekoure.github.io/europeanaR/" rel="nofollow"><img src='man/figures/logo.svg' align="right" height="130"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/AleKoure/europeanaR/workflows/R-CMD-check/badge.svg)](https://github.com/AleKoure/europeanaR/actions)
[![Codecov test
coverage](https://codecov.io/gh/AleKoure/europeanaR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/AleKoure/europeanaR?branch=main)
<!-- badges: end -->

The goal of europeanaR is to provide tools to access Linked Open Data
published by Europeana. Gain access to millions of entries related to
artworks, books, music, and videos on art, newspapers, archaeology,
fashion, science, sport, and much more. This thesaurus of cultural
heritage can be used in numerous creative ways like for example building
applications, data analysis, Statistical and Deep Learning.

## Getting started

You can install the development version of europeanaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AleKoure/europeanaR")
```

Sign up for your free API key and get developing
[here](https://pro.europeana.eu/page/get-api)! When you get you personal
key set it as environmental variable.

``` r
europeanaR::set_key("YOUR_KEY_GOES_HERE")
```

Alternatively, you can edit `.Renviron` by inserting your personal key
to the environmental variable `EUROPEANA_KEY`,

## Example

The Search API provides a way to search for metadata records and media
on the Europeana repository, for instance give me all results for the
word “Vermeer”. The basic API call to the search API can return up to
100 items.

``` r
library(europeanaR)
library(data.table)
resp <- query_search_api("Vermeer", rows = 10, reusability = "open")
#transform to tabular data
resp_tidy <- tidy_search_items(resp)
#get preview of first items
first_image <- resp_tidy[type == "IMAGE"][1, edmPreview]
```

<p align="center">
<img align="center" src="https://api.europeana.eu/thumbnail/v2/url.json?uri=https%3A%2F%2Fwww.rijksmuseum.nl%2Fassetimage2.jsp%3Fid%3DSK-A-2344&type=IMAGE">
</p>

The API call repeats in case of an error, and it is designed to be kind
to the server, see [here](https://httr.r-lib.org/reference/RETRY.html).
The retrieved metadata are given in JSON format and are parsed into an R
object using [jsonlite](https://arxiv.org/abs/1403.2805). In addition,
these data can be transformed in tabular format and are compatible also
with the [data.table](https://github.com/Rdatatable/data.table) package
for fast data manipulation operations.

The response richness can be controlled by using the `profile` query
parameter. Also, get bulk downloads of metadata and associated media
files by using the functions `tidy_cursored_search()` and
`download_media()`.

``` r
bulk_and_rich <- tidy_cursored_search("Vermeer", max_items = 500,
                                      profile = "rich")
```

The Europeana services documentation can be found
[here](https://pro.europeana.eu/page/search). See the vignettes section
and other resources at the package
[homepage](https://alekoure.github.io/europeanaR/) for more information
and examples.

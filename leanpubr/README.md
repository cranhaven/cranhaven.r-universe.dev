
<!-- badges: start -->

[![R-CMD-check](https://github.com/muschellij2/leanpubr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/muschellij2/leanpubr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# leanpubr Package:

The goal of `leanpubr` is to provide provides access to the ‘Leanpub’
‘API’ <https://leanpub.com/help/api> for gathering information about
publications and submissions to the ‘Leanpub’ platform.

## Installation

You can install `leanpubr` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("muschellij2/leanpubr")
```

## Setting the API key

The main thing you need to do is get an API key from Leanpub. You can do
this by visiting <https://leanpub.com/user_dashboard/api_key> if you are
an author. If you are not an author, I don’t believe access to the API
is possible. You can pass this `api_key` to functions in Leanpub (almost
all prefixed by `lp_`), or you can set the environmental variable
`LEANPUB_API_KEY`. You can set this in an interactive `R` session such
as:

``` r
Sys.setenv("LEANPUB_API_KEY" = "YOUR_KEY")
```

or put this in your `~/.Renviron`:

``` r
LEANPUB_API_KEY = "YOUR_KEY"
```

## Example

You can use `lp_book_info` to get information about any book in Leanpub:

``` r
library(leanpubr)
slug = "biostatmethods"
res = lp_book_info(slug, error = FALSE, verbose = TRUE)
#> GET command is:
#> Response [https://leanpub.com/login]
#>   Date: 2025-04-01 15:44
#>   Status: 200
#>   Content-Type: text/html; charset=utf-8
#>   Size: 25.4 kB
#> <!DOCTYPE html>
#> <!--[if lt IE 7]> <html class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->
#> <!--[if IE 7]> <html class="no-js lt-ie9 lt-ie8"> <![endif]-->
#> <!--[if IE 8]> <html class="no-js lt-ie9"> <![endif]-->
#> <!--[if gt IE 9]><!--><html class="no-js" lang="en"><!--<![endif]--><head>
#> <meta content='IE=edge' http-equiv='X-UA-Compatible'>
#> <meta charset='utf-8'>
#> <meta content='width=device-width, initial-scale=1.0, maximum-scale=1, user-s...
#> <meta content='en' name='Content-Language'>
#> <link rel="apple-touch-icon-precomposed" type="image/png" href="https://leanp...
#> ...
res$content
#> {html_document}
#> <html class="no-js" lang="en">
#> [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF-8 ...
#> [2] <body id="sessions-login">\n\n<div class="flash" id="js-flash-prototype"> ...
```

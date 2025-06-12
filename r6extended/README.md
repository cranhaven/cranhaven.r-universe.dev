
<br><br> **Status**

[![Travis-CI Build
Status](https://travis-ci.org/petermeissner/r6extended.svg?branch=master)](https://travis-ci.org/petermeissner/r6extended)
[![codecov](https://codecov.io/gh/petermeissner/r6extended/branch/master/graph/badge.svg)](https://codecov.io/gh/petermeissner/r6extended/tree/master/R)
[![CRAN
version](http://www.r-pkg.org/badges/version/r6extended)](https://cran.r-project.org/package=r6extended)

# Extension for ‘R6’ Base Class

<br><br> **Version**

0.1.2 <br> 2018-01-14

<br><br> **Description**

Useful methods and data fields to extend the bare bones ‘R6’ class
provided by the ‘R6’ package - ls-method, hashes, warning- and
message-method, general get-method and a debug-method that assigns self
and private to the global environment.

<br><br> **License**

MIT + file LICENSE <br>Peter Meissner \[aut, cre\]

<br><br> **Citation**

To cite package ‘r6extended’ in publications use:

Peter Meissner (2019). r6extended: Extension for ‘R6’ Base Class. R
package version 0.1.2. <https://github.com/petermeissner/r6extended>

A BibTeX entry for LaTeX users is

@Manual{, title = {r6extended: Extension for ‘R6’ Base Class}, author =
{Peter Meissner}, year = {2019}, note = {R package version 0.1.2}, url =
{<https://github.com/petermeissner/r6extended>}, }

<br><br> **BibTex for citing**

<code style="white-space:normal;"> @Manual{, title = {r6extended:
Extension for ‘R6’ Base Class}, author = {Peter Meissner}, year =
{2019}, note = {R package version 0.1.2}, url =
{<https://github.com/petermeissner/r6extended>}, } </code>

<br><br> **Installation**

stable version from CRAN

``` r
install.packages("r6extended")
```

``` r
devtools::install_github("petermeissner/r6extended")
```

<br><br> **Example Usage**

<br><br> ***… starting up …***

``` r
library(r6extended)
```

***new instance***

``` r
ext <- r6extended$new()
```

***whats there?***

``` r
ext$ls()
```

    ##        name   where    class
    ## 1      hash private function
    ## 2    hashed private function
    ## 3    hashes private     list
    ## 4     clone    self function
    ## 5     debug    self function
    ## 6       get    self function
    ## 7   hash_do    self function
    ## 8  hash_get    self function
    ## 9        ls    self function
    ## 10  message    self function
    ## 12  warning    self function
    ## 11  options    self     list

***getting things (wherever they are, also private stuff)***

``` r
ext$get("options")
```

    ## $verbose
    ## [1] TRUE
    ## 
    ## $warning
    ## [1] TRUE

``` r
ext$get("hashes")
```

    ## list()

***messages***

``` r
ext$message("Please note ...")
```

    ## r6extended : Please note ...

``` r
ext$options$verbose <- FALSE
ext$message("Please note ...")
```

***build in hashing***

``` r
ext$hash_do()
ext$hash_get("options")
```

    ## [1] "feb524178c59d96d"

***debugging***

``` r
ext$debug()

private$hash()
```

    ## $hash
    ## [1] "a2145d8a65aed2fd"
    ## 
    ## $hashed
    ## [1] "6ddac4b4cd8556db"
    ## 
    ## $hashes
    ## [1] "192f75af59696813"
    ## 
    ## $clone
    ## [1] "11f7a5b9d5763be9"
    ## 
    ## $debug
    ## [1] "3f1f72468b467261"
    ## 
    ## $get
    ## [1] "31fd6f69e480adaa"
    ## 
    ## $hash_do
    ## [1] "236b962b10c0eb01"
    ## 
    ## $hash_get
    ## [1] "bdc96d1de6eea991"
    ## 
    ## $ls
    ## [1] "d18ce6a302986879"
    ## 
    ## $message
    ## [1] "0eb0aed48eca3590"
    ## 
    ## $warning
    ## [1] "c30df76f92169fbd"
    ## 
    ## $options
    ## [1] "feb524178c59d96d"

``` r
self$ls()
```

    ##        name   where    class
    ## 1      hash private function
    ## 2    hashed private function
    ## 3    hashes private     list
    ## 4     clone    self function
    ## 5     debug    self function
    ## 6       get    self function
    ## 7   hash_do    self function
    ## 8  hash_get    self function
    ## 9        ls    self function
    ## 10  message    self function
    ## 12  warning    self function
    ## 11  options    self     list

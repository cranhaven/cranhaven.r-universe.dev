
# webqueue

<!-- badges: start -->
[![cran](https://www.r-pkg.org/badges/version/webqueue)](https://CRAN.R-project.org/package=webqueue)
[![conda](https://anaconda.org/conda-forge/r-webqueue/badges/version.svg)](https://anaconda.org/conda-forge/r-webqueue)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/webqueue)](https://cranlogs.r-pkg.org/)
[![dev](https://github.com/cmmr/webqueue/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmmr/webqueue/actions/workflows/R-CMD-check.yaml)
[![covr](https://codecov.io/gh/cmmr/webqueue/graph/badge.svg)](https://app.codecov.io/gh/cmmr/webqueue)
<!-- badges: end -->


The goal of webqueue is to process HTTP requests on interruptible background processes.

Use cases:

* Prevent user-submitted jobs from excessively hogging compute resources.

* Stop tasks that are no longer needed.




## Installation

```r
# Install the latest stable version from CRAN:
install.packages("webqueue")

# Or the development version from GitHub:
install.packages("pak")
pak::pak("cmmr/webqueue")
```


## Example

```r
library(webqueue)

wq <- WebQueue$new(~{ 'Hello world!\n' })

readLines('http://localhost:8080')
#> [1] "Hello world!"

wq$stop()
```


## Query Parameters

```r
wq <- WebQueue$new(~{ jsonlite::toJSON(.$ARGS) })

cat(RCurl::getURL('http://localhost:8080?myvar=123'))
#> {"myvar":["123"]}

wq$stop()
```

Accepts both GET and POST parameters.


## Simple API

```r
wq <- WebQueue$new(
  handler = function (req) {
    switch(
      EXPR = req$PATH_INFO,
      '/date' = date(),
      '/req'  = ls.str(as.list(req)),
      '/args' = req$ARGS,
      '/cpus' = as.numeric(parallelly::availableCores()),
      404L )
})

cat(RCurl::getURL('http://localhost:8080/cpus'))
#> 6

cat(RCurl::getURL('http://localhost:8080/req?x=123&id=ABC'))
#> ARGS : List of 2
#>  $ x : chr "123"
#>  $ id: chr "ABC"
#> COOKIES :  Named list()
#> HEADERS : List of 1
#>  $ host: chr "localhost:8080"
#> PATH_INFO :  chr "/req"
#> REMOTE_ADDR :  chr "127.0.0.1"
#> REQUEST_METHOD :  chr "GET"
#> SERVER_NAME :  chr "127.0.0.1"
#> SERVER_PORT :  chr "8080"

wq$stop()
```



## Interrupting Requests

The strength of webqueue is its ability to cleanly abort a request at any point.

See `vignette('interrupts')` for more detailed examples.


### Set a time limit

```r
wq <- WebQueue$new(
  handler = ~{ Sys.sleep(.$ARGS$s); 'Hello world!' }, 
  timeout = 1 )

RCurl::getURL('http://localhost:8080?s=2')
#> [1] "timeout: total runtime exceeded 1 second\n"

RCurl::getURL('http://localhost:8080?s=0')
#> [1] "Hello world!"

wq$stop()
```



### Merge duplicate requests

```r
wq <- WebQueue$new(
  handler = function (req) { Sys.sleep(1); req$ARGS$x }, 
  copy_id = function (job) job$req$PATH_INFO )
# ^^^^^^^   `copy_id` will be '/a' or '/b'

# Fetch two URLs at the same time. '/b' path is merged.
jq <- jobqueue::Queue$new(workers = 3L)$wait()   # vv
a1 <- jq$run({ RCurl::getURL('http://localhost:8080/a?x=first') })
b1 <- jq$run({ RCurl::getURL('http://localhost:8080/b?x=second') })
b2 <- jq$run({ RCurl::getURL('http://localhost:8080/b?x=third') })

dput(c(a1 = a1$result, b1 = b1$result, b2 = b2$result))
#> c(a1 = "first", b1 = "second", b2 = "second")

jq$stop()
wq$stop()
```


### Stop duplicate requests
```r
wq <- WebQueue$new(
  handler = function (req) { Sys.sleep(1); req$ARGS$x }, 
  stop_id = function (job) job$req$PATH_INFO )
# ^^^^^^^   `stop_id` will be '/a' or '/b'

# Fetch three URLs at the same time. '/b' path is stopped.
jq <- jobqueue::Queue$new(workers = 3L)$wait()   # vv
a1 <- jq$run({ RCurl::getURL('http://localhost:8080/a?x=first') })
b1 <- jq$run({ RCurl::getURL('http://localhost:8080/b?x=second') })
b2 <- jq$run({ RCurl::getURL('http://localhost:8080/b?x=third') })

dput(c(a1 = a1$result, b1 = b1$result, b2 = b2$result))
#> c(a1 = "first", b1 = "superseded: duplicated stop_id\n", b2 = "third")

jq$stop()
wq$stop()
```


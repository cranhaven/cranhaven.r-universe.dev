
An Example in R
---------------

Let's again say you have the example logs in the file `accesslog.txt`.

``` r
log_file <- 'vignettes/accesslog.txt'
cat(readr::read_file(log_file))
```

    10.0.0.8 - - [2019-01-01:10:58:12 -0500] "https://mysite.com/index.html"
    173.28.102.33 - - [2019-01-01:10:58:25 -0500] "https://mysite.com/login"

We first define the template as before.

``` r
template <- '{{ ip ip_address }} - - [{{ date date_time }}] "{{ url URL }}"'
```

We then need to define our classes. `ip` and `url` are builtins with the package, but dates come in a variety of formats so we must explicitly define ours here. Note you can see all builtins using `default_classes()`

``` r
date_parser <- parser(
  '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{2}[ ][\\-\\+][0-9]{4}',
  function(x) lubridate::as_datetime(x, format = '%Y-%m-%d:%H:%M:%S %z'),
  name = 'date'
)
date_parser
```

    ## Parser: date
    ## ------------
    ## Matches:
    ##   [0-9]{4}\-[0-9]{2}\-[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{2}[ ][\-\+][0-9]{4}
    ## Formatter:
    ##   function (x) 
    ##   lubridate::as_datetime(x, format = "%Y-%m-%d:%H:%M:%S %z")

``` r
default_classes()[c('ip', 'url')]
```

    ## $ip
    ## Parser: ip
    ## ----------
    ## Matches:
    ##   [0-9]{1,3}(\.[0-9]{1,3}){3}
    ## Formatter:
    ##   .Primitive("(")
    ## 
    ## $url
    ## Parser: url
    ## -----------
    ## Matches:
    ##   (-|(?:http(s)?:\/\/)?[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#[\]@!\$&'\(\)\*\+,;=.]+)
    ## Formatter:
    ##   .Primitive("(")

Both `ip` and `url` require no formatting, so they have the identity function, (`(` in R), as their formatter.

To get our final output in tabular format, we simply make the follow call to `parse_logs`.

``` r
# Naming the date_parser 'date' in the list tells Tabulog to use it to parse
# the field with class 'date' in the template.
parse_logs(readLines(log_file), template, classes = list(date = date_parser))
```

    ##      ip_address           date_time                           URL
    ## 1      10.0.0.8 2019-01-01 15:58:12 https://mysite.com/index.html
    ## 2 173.28.102.33 2019-01-01 15:58:25      https://mysite.com/login

Note that we only had to pass our custom class `date`. The builtin classes `ip` and `url` were included by default.

A more elegant and portable way of completing this task would be to define the template and the custom class in the same file, which can be ported to other Tabulog libraries in other languages, leaving only the formatters to be defined in the R script.

First, we define the `template` and the `classes` in a yaml file

``` r
template_file <- 'vignettes/accesslog_template.yml'
cat(readr::read_file(template_file))
```

    template: '{{ ip ip_address }} - - [{{ date date_time }}] "{{ url URL }}"'
    classes:
      date: '[0-9]{4}\-[0-9]{2}\-[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{2}[ ][\-\+][0-9]{4}'

Next, we define the formatters for each of our classes. Here we only have one, but we still put it in a named list, with the name matching the name of the class in the template file.

``` r
formatters <- list(
  date = function(x) lubridate::as_datetime(x, format = '%Y-%m-%d:%H:%M:%S %z')
)
```

Finally, we make one call to `parse_logs_file`.

``` r
parse_logs_file(log_file, template_file, formatters)
```

    ##      ip_address           date_time                           URL
    ## 1      10.0.0.8 2019-01-01 15:58:12 https://mysite.com/index.html
    ## 2 173.28.102.33 2019-01-01 15:58:25      https://mysite.com/login

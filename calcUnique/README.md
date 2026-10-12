calcUnique Package
================

This is a one-function package so that you can pass only unique values
to a computationally expensive function that returns an output of the
same length as the input.

In importing and working with tidy data, it is common to have index
columns, often including time stamps that are far from unique. Some
funcitons to work with these such as text conversion, various
`grep()`-based functions, and often the `cut()` function are relatively
slow when working with tens of millions of rows or more.

This wrapper function pares down the input vector to process to only
unique values using hte `unique()` function, and in my experience the
`unique()` and `match()` functions upon which `calcUnique()` is based
are so fast that only a small amount of repition is necessary to make
calcUnique the faster option.

`calcUnique()` is a wrapper for any function that takes in a vector or
list and returns a vector or list the same length. The inputs and
outputs are the same as they would be otherwise … it just happens
faster.

## Examples:

``` r
#Create a sample of some date text with repeats
ts_sample <-
  sample(as.character(seq(
    from = as.POSIXct('2020-03-01'),
    to = as.POSIXct('2020-03-05'),
    by = 'day'
  )),
  size = 10, replace = TRUE)

ts_sample
```

    ##  [1] "2020-03-01" "2020-03-02" "2020-03-02" "2020-03-02" "2020-03-04"
    ##  [6] "2020-03-05" "2020-03-05" "2020-03-01" "2020-03-01" "2020-03-01"

``` r
#Now convert the time text back to POSIXct timestamps:
as.POSIXct(ts_sample)
```

    ##  [1] "2020-03-01 MST" "2020-03-02 MST" "2020-03-02 MST" "2020-03-02 MST"
    ##  [5] "2020-03-04 MST" "2020-03-05 MST" "2020-03-05 MST" "2020-03-01 MST"
    ##  [9] "2020-03-01 MST" "2020-03-01 MST"

``` r
#Do the same with the calcUnique function:
calcUnique(ts_sample, as.POSIXct)
```

    ##  [1] "2020-03-01 MST" "2020-03-02 MST" "2020-03-02 MST" "2020-03-02 MST"
    ##  [5] "2020-03-04 MST" "2020-03-05 MST" "2020-03-05 MST" "2020-03-01 MST"
    ##  [9] "2020-03-01 MST" "2020-03-01 MST"

``` r
#Just to show that the output is the same with and without calcUnique:
all.equal(as.POSIXct(ts_sample), calcUnique(ts_sample, as.POSIXct))
```

    ## [1] TRUE

``` r
#An example for when the function doesn't take the vector as the first argument:
gsub("03", "$3", ts_sample)
```

    ##  [1] "2020-$3-01" "2020-$3-02" "2020-$3-02" "2020-$3-02" "2020-$3-04"
    ##  [6] "2020-$3-05" "2020-$3-05" "2020-$3-01" "2020-$3-01" "2020-$3-01"

``` r
calcUnique(ts_sample, function(i) gsub("03", "$3", i))
```

    ##  [1] "2020-$3-01" "2020-$3-02" "2020-$3-02" "2020-$3-02" "2020-$3-04"
    ##  [6] "2020-$3-05" "2020-$3-05" "2020-$3-01" "2020-$3-01" "2020-$3-01"

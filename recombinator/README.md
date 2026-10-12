R recombinators [![Build Status](https://travis-ci.org/robertzk/recombinator.svg?branch=master)](https://travis-ci.org/robertzk/recombinator) [![Coverage Status](https://coveralls.io/repos/robertzk/recombinator/badge.svg?branch=master)](https://coveralls.io/r/robertzk/recombinator)
============

An R utility for turning nested lists into data.frames. This can
be useful for turning JSON into R lists, and then into data.frames.

Installation
------------

The latest stable build can be downloaded from CRAN:

```R
install.packages("recombinator")
```

To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/recombinator")
```

Usage
-----

There are two supported formats.

 * __Homogeneous lists__. A list where the first list element
    is a character vector giving the names of the data.frame,
    and the subsequent list elements themselves lists of values.
 * __Heterogeneous lists__. A list where each element is a named
    list of values. In this format, `plyr::rbind` will be used
    to take the union of all names and impute the ones missing
    with NA values.

Here are two examples of the respective format:

```r
recombinator(list(c("a","b","c"), list(1, F, 2), list(2, T, 3)))
#   a     b c
# 1 1 FALSE 2
# 2 2  TRUE 3

recombinator(list(c("a","b","c"), list(1, F, 2), list(2, T, 3)))
# recombinator(list(list(a = 1, b = F, c = 2), list(a = 2, b = T, c = 3)))
#  a     b c
# 1 1 FALSE 2
# 2 2  TRUE 3

# The union of all observed keys is used for column names.
recombinator(list(list(a = 1, b = F, c = 2), list(a = 2, b = T, d = 4)))
#   a     b  c  d
# 1 1 FALSE  2 NA
# 2 2  TRUE NA  4
```



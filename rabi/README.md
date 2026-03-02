
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rabi

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rabi)](https://cran.r-project.org/package=rabi)

The goal of `rabi` is to facilitate the design and generation of color
(or symbol) codes that can be used to mark and identify individual
animals (specifically aimed for research projects). These codes can be
selected such that the IDs are robust to partial erasure: even if parts
of the code are lost, the entire identity of the animal can be
reconstructed Thus, animal subjects are not confused and no ambiguity is
introduced.

## Installation

You can install `rabi` from CRAN with:

``` r
install.packages("rabi")
```

You can also install `rabi` from Github with:

``` r
# install.packages("devtools")
devtools::install_github("andburch/rabi")
```

## Example

Although these methods and principles are applicable across taxa and
situations, we will demonstrate their usefulness on ants as an
illustration. A well-thought-out identification system is important in
social insect research because colonies are often composed of hundreds
to thousands of virtually identical workers. Additionally, their
tendency to groom themselves (and each other) creates a situation where
markings are especially susceptible to becoming detached and disrupting
identification.

Let’s say we have 100 ants we need to track and observe: we need to
*always* know their individual identities. We have five colors of paint
which we will apply as drops to locations on their backs. Their heads
and thoraxes will each receive a colored drop and then they will get two
drops on their abdomens (i.e. gasters). We want a coding scheme that
will still perfectly identify the ants even if they *all* lose one of
these marks.

``` r
total.length <- 4  #we have four places to put paint,
redundancy <- 1    #we want robustness to single erasures,
alphabet <- 5      #and we currently have five types of paint in stock
 #rs_IDs() is one of the functions that can generate codes
codes <- rabi::rs_IDs(total.length, redundancy, alphabet)
 #prep the output to be displayed in a table
codes <- t(do.call("cbind",codes))
knitr::kable(
  head(codes, n = 10L), 
  col.names = c("Head","Thorax","R. Abdomen","L. Abdomen"), 
  align = "c", caption = "ID sequences for ants")
```

| Head | Thorax | R. Abdomen | L. Abdomen |
| :--: | :----: | :--------: | :--------: |
|  0   |   0    |     0      |     0      |
|  1   |   1    |     1      |     1      |
|  2   |   2    |     2      |     2      |
|  3   |   3    |     3      |     3      |
|  4   |   4    |     4      |     4      |
|  0   |   1    |     2      |     3      |
|  1   |   2    |     3      |     4      |
|  2   |   3    |     4      |     0      |
|  3   |   4    |     0      |     1      |
|  4   |   0    |     1      |     2      |

ID sequences for ants

We can easily add our paint color names to the output as well. (However,
I would personally save the mapping for future reference, etc.)

``` r
paint.names <- c("red","light blue", "greenish", "off-white", "yellow")
color.codes <- rabi::codes_to_colors(codes, paint.names)
 #prep the output to be displayed in a table
color.codes <- t(do.call("cbind",color.codes))
knitr::kable(
  head(color.codes, n = 10L), 
  col.names = c("Head","Thorax","R. Abdomen","L. Abdomen"), 
  align = "c", caption = "ID paint sequences for ants")
```

|    Head    |   Thorax   | R. Abdomen | L. Abdomen |
| :--------: | :--------: | :--------: | :--------: |
|    red     |    red     |    red     |    red     |
| light blue | light blue | light blue | light blue |
|  greenish  |  greenish  |  greenish  |  greenish  |
| off-white  | off-white  | off-white  | off-white  |
|   yellow   |   yellow   |   yellow   |   yellow   |
|    red     | light blue |  greenish  | off-white  |
| light blue |  greenish  | off-white  |   yellow   |
|  greenish  | off-white  |   yellow   |    red     |
| off-white  |   yellow   |    red     | light blue |
|   yellow   |    red     | light blue |  greenish  |

ID paint sequences for
    ants

    #> Note: The mapping (see below) that was used to assign color names to numeric values is not saved or assigned to a variable. 
    #>  The exact mapping may change with repeated function calls. Depending on your circumstances, you may want to record this now.

    #>            0            1            2            3            4 
    #>        "red" "light blue"   "greenish"  "off-white"     "yellow"

\#\#More For a (possibly buggy) Shiny-based GUI suited for new R users,
try running this command.

``` r
rabi::exampleGUI()
```

For additional functions, deeper examples, and such, check out the
vignette on designing color band schemes for bird legs: it’s chock full
of cool stuff.

``` r
utils::vignette('loosebirdtag', package='rabi')
```

Lastly, check out our journal article on this subject:

> Burchill, A. T., & Pavlic, T. P. (2019). Dude, where’s my mark?
> Creating robust animal identification schemes informed by
> communication theory. *Animal Behaviour*, 154, 203-208.
> [doi:10.1016/j.anbehav.2019.05.013](https://doi.org/10.1016/j.anbehav.2019.05.013)

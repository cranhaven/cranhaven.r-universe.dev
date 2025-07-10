
# Bodi

<!-- badges: start -->
<!-- badges: end -->

This R package 
implements the [Bodi algorithm](https://hal.archives-ouvertes.fr/hal-03041309/).
The goal of Bodi (BOosting DIversity) is to construct a sequence of learners 
actively promoting diversity among them. Then, increasing diversity ensures the 
reduction of the mean-square-error producing a strong(er) ensemble learner. 

## Installation

You can install the released version of Bodi from [CRAN](https://CRAN.R-project.org) with (soon):

``` r
install.packages("Bodi")
```

or from Gitlab (need to make repo public or use a PAT)

``` r
remotes::install_gitlab("yannig/bodi")
```

## Example

Bodi has a main function called `boosting_diversity`. This basic example shows how to obtain the Bodi fit of a simple dataset :

``` r
library(Bodi)
all <- na.omit(airquality)
smp <- sample(nrow(all), .8 * nrow(all))
boosting_diversity("Ozone", "Solar.R+Wind+Temp+Month+Day", 
                   data0 = all[smp, ], data1 = all[-smp, ])
```

See the documentation for further details.

---

## Acknowledgements

This research development benefited from the support of the «FMJH Research Initiative Data Science for Industry», and from the support to this program from EDF.

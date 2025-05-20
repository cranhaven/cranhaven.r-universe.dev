
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DemografixeR<img src="man/figures/logo.png" align="right" height=140/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/DemografixeR)](https://CRAN.R-project.org/package=DemografixeR)
[![Travis build
status](https://travis-ci.org/matbmeijer/DemografixeR.svg?branch=master)](https://travis-ci.org/matbmeijer/DemografixeR)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/matbmeijer/DemografixeR?branch=master&svg=true)](https://ci.appveyor.com/project/matbmeijer/DemografixeR)
[![Codecov test
coverage](https://codecov.io/gh/matbmeijer/DemografixeR/branch/master/graph/badge.svg)](https://codecov.io/gh/matbmeijer/DemografixeR?branch=master)
<!-- badges: end -->

‘DemografixeR’ allows to estimate gender, age & nationality from a name.
The package is an API wrapper of all 3 ‘Demografix’ API’s - all three
APIs supported in one package:

  - [https://genderize.io](https://genderize.io/) - **Gender
    estimation** based on a name
  - [https://agify.io](https://agify.io/) - **Age estimation** based on
    a name
  - [https://nationalize.io](https://nationalize.io/) - **Nationality
    estimation** based on a name

## Documentation

You can find all the necessary documentation about the package
    here:

  - [https://matbmeijer.github.io/DemografixeR](https://matbmeijer.github.io/DemografixeR/)

## Installation

You can install the CRAN release version of DemografixeR following this
`R` command:

``` r
install.packages("DemografixeR")
```

You can also install the development version of DemografixeR following
these `R` commands:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("matbmeijer/DemografixeR")
```

## Examples

These are basic examples, which shows you how to estimate
**nationality**, **gender** and **age** by a given name with & without
specifying a country. The package takes care of multiple background
tasks:

  - API pagination
  - Duplicated names (one request made per name)
  - Missing values
  - Workflow integration (e.g. with `dplyr` or `data.table`)

<!-- end list -->

``` r
library(DemografixeR)

#Simple example without country_id
names<-c("Ben", "Allister", "Lucie", "Paula")
genderize(name = names)
#> [1] "male"   "male"   "female" "female"
nationalize(name = names)
#> [1] "AU" "ZA" "CZ" "PT"
agify(name = names)
#> [1] 48 44 24 50

#Simple example with
genderize(name = names, country_id = "US")
#> [1] "male"   "male"   "female" "female"
agify(name = names, country_id = "US")
#> [1] 67 46 65 70

#Workflow example with dplyr with missing values and multiple different countries
df<-data.frame(names=c("Ana", NA, "Pedro",
                       "Francisco", "Maria", "Elena"),
                 country=c(NA, NA, "ES",
                           "DE", "ES", "NL"), stringsAsFactors = FALSE)

df %>% dplyr::mutate(guessed_nationality=nationalize(name = names),
                guessed_gender=genderize(name = names, country_id = country),
                guessed_age=agify(name = names, country_id = country)) %>% 
  knitr::kable()
```

| names     | country | guessed\_nationality | guessed\_gender | guessed\_age |
| :-------- | :------ | :------------------- | :-------------- | -----------: |
| Ana       | NA      | PT                   | female          |           58 |
| NA        | NA      | NA                   | NA              |           NA |
| Pedro     | ES      | PT                   | male            |           69 |
| Francisco | DE      | CL                   | male            |           58 |
| Maria     | ES      | CY                   | NA              |           59 |
| Elena     | NL      | CC                   | female          |           69 |

``` r

#Detailed data.frame example:
genderize(name = names, simplify = FALSE, meta = TRUE) %>% knitr::kable()
```

|   | name     | type   | gender | probability | count | api\_rate\_limit | api\_rate\_remaining | api\_rate\_reset | api\_request\_timestamp |
| - | :------- | :----- | :----- | ----------: | ----: | ---------------: | -------------------: | ---------------: | :---------------------- |
| 2 | Ben      | gender | male   |        0.95 | 77991 |             1000 |                  831 |             5214 | 2020-05-04 22:33:05     |
| 1 | Allister | gender | male   |        0.98 |   129 |             1000 |                  831 |             5214 | 2020-05-04 22:33:05     |
| 3 | Lucie    | gender | female |        0.99 | 85580 |             1000 |                  831 |             5214 | 2020-05-04 22:33:05     |
| 4 | Paula    | gender | female |        0.98 | 74130 |             1000 |                  831 |             5214 | 2020-05-04 22:33:05     |

## Disclaimer

  - This package is in no way affiliated to the Demografix ApS company,
    the owner of the [‘genderize.io’](https://genderize.io/),
    [‘agify.io’](https://agify.io/) and
    [‘nationalize.io’](https://nationalize.io/) APIs.
  - An open mind towards gender & gender diversity is promoted, warning
    that the results from the ‘genderize.io’ API reflect an
    oversimplification of gender identity, gender roles and the meaning
    of ‘gender’. For more information visit the active discussion in the
    following [Wikipedia article](https://en.wikipedia.org/wiki/Gender).

## Code of Conduct

Please note that the ‘DemografixeR’ project is released with a
[Contributor Code of
Conduct](https://github.com/matbmeijer/DemografixeR/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## License

[MIT © Matthias
Brenninkmeijer](https://github.com/matbmeijer/DemografixeR/blob/master/LICENSE.md)

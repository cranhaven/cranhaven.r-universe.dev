
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lotterybr

<!-- badges: start -->

[![R-CMD-check](https://github.com/tomasbp2/lotterybr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tomasbp2/lotterybr/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of the package is to provide a function to automatically import
data from the lotteries of Caixa Econômica Federal into the R
environment. Data from the lotteries of Caixa Econômica Federal includes
information on various games such as Mega-Sena, Lotofácil, Quina, among
others. They are downloaded from the official website of the Federal
Savings Bank at <https://loterias.caixa.gov.br/>.

## Installation

You can install the development version of lotterybr like so:

``` r
# install.packages("devtools")
# devtools::install_github("tomasbp2/lotterybr")
```

## Example

Downloading Lotofacil winners data:

``` r

library(lotterybr)

lotofacil <- get_data(game= "lotofacil", type= "winners")
```

Opening the shiny app

``` r

library(lotterybr)
open_app()
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

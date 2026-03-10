

<!-- README.md is generated from README.qmd. Please edit that file -->

# measles <img src="man/figures/logo.png" width="200px" alt="epiworld logo" align="right">

<!-- badges: start -->

[![ForeSITE Group](https://github.com/EpiForeSITE/software/raw/e82ed88f75e0fe5c0a1a3b38c2b94509f122019c/docs/assets/foresite-software-badge.svg)](https://github.com/EpiForeSITE)
[![CRAN status](https://www.r-pkg.org/badges/version/measles)](https://CRAN.R-project.org/package=measles)
[![R-CMD-check](https://github.com/UofUEpiBio/measles/actions/workflows/r.yml/badge.svg)](https://github.com/UofUEpiBio/measles/actions/workflows/r.yml)
[![CRANlogs downloads](https://cranlogs.r-pkg.org/badges/grand-total/measles)](https://cran.r-project.org/package=measles)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/UofUEpiBio/measles/blob/master/LICENSE.md)
[![codecov](https://codecov.io/gh/UofUEpiBio/measles/graph/badge.svg?token=ZB8FVLI7GN)](https://app.codecov.io/gh/UofUEpiBio/measles)
[![status](https://tinyverse.netlify.app/badge/measles)](https://CRAN.R-project.org/package=measles)

<!-- badges: end -->

## Overview

The **measles** package is a specialized spinoff from
[epiworldR](https://github.com/UofUEpiBio/epiworldR), focusing
exclusively on measles epidemiological models. This package provides
fast, agent-based models (ABMs) for studying measles transmission
dynamics, vaccination strategies, and intervention policies.

Built on the powerful [epiworld](https://github.com/UofUEpiBio/epiworld)
C++ library, these models leverage the speed and flexibility of
epiworldR while providing specialized functionality for measles outbreak
modeling.

## Features

- **Fast simulation**: Leverages the high-performance C++ backend from
  epiworld
- **Specialized measles models**: Three distinct models tailored for
  different scenarios
- **Flexible interventions**: Support for vaccination, quarantine,
  isolation, and contact tracing
- **Population mixing**: Models can account for different population
  groups with varying contact patterns
- **Risk-based strategies**: Advanced models support risk-stratified
  quarantine policies

## Models Included

The package includes three measles-specific models:

1.  **ModelMeaslesSchool**: A SEIHR
    (Susceptible-Exposed-Infectious-Hospitalized-Recovered) model
    designed for school settings with isolation and quarantine policies.

2.  **ModelMeaslesMixing**: A measles model with population mixing
    between different groups, including vaccination, quarantine,
    isolation, and contact tracing mechanisms.

3.  **ModelMeaslesMixingRiskQuarantine**: An advanced mixing model with
    risk-based quarantine strategies that assign different quarantine
    durations based on exposure risk levels (high, medium, low).

## Installation

You can install the measles package from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("UofUEpiBio/measles")
```

Or from <a href="https://uofuepibio.r-universe.dev/"
target="_blank">R-universe</a> (recommended for the latest development
version):

``` r
install.packages(
  'measles',
  repos = c(
    'https://uofuepibio.r-universe.dev',
    'https://cloud.r-project.org'
  )
)
```

## Quick Example

Here’s a simple example using the ModelMeaslesSchool:

``` r
library(measles)

# Create a measles model for a school with 500 students
model_school <- ModelMeaslesSchool(
  n = 500,
  prevalence = 1,
  prop_vaccinated = 0.70,
  contact_rate = 15,
  transmission_rate = 0.9
)

# Run the simulation
run(model_school, ndays = 100, seed = 1912)

# View results
summary(model_school)
plot(model_school)
```

## Example with Population Mixing

The mixing models allow you to simulate measles spread across different
population groups:

``` r
library(measles)

# Define three populations
e1 <- entity("Population 1", 3000, as_proportion = FALSE)
e2 <- entity("Population 2", 3000, as_proportion = FALSE)
e3 <- entity("Population 3", 3000, as_proportion = FALSE)

# Define contact matrix (row-stochastic: rows sum to 1)
contact_matrix <- matrix(c(
  0.9, 0.05, 0.05,
  0.1, 0.8,  0.1,
  0.1, 0.2,  0.7
), byrow = TRUE, nrow = 3)

# Create the model
measles_model <- ModelMeaslesMixing(
  n = 9000,
  prevalence = 1 / 9000,
  contact_rate = 15,
  transmission_rate = 0.9,
  vax_efficacy = 0.97,
  prop_vaccinated = 0.95,
  contact_matrix = contact_matrix,
  quarantine_period = 14,
  isolation_period = 10
)

# Add entities and run
measles_model |>
  add_entity(e1) |>
  add_entity(e2) |>
  add_entity(e3)

run(measles_model, ndays = 100, seed = 331)
summary(measles_model)
```

## Relationship to epiworldR

This package is a spinoff from epiworldR, which provides a comprehensive
framework for agent-based epidemiological models. While epiworldR
includes many different disease models (SIR, SEIR, SIS, etc.), the
measles package focuses specifically on measles-related models with
specialized features for:

- Measles-specific disease progression (incubation, prodromal, and rash
  periods)
- School-based outbreak scenarios
- Vaccination coverage and efficacy
- Quarantine and isolation policies
- Contact tracing strategies
- Risk-stratified interventions

For general-purpose epidemiological modeling or other disease types,
please see the [epiworldR
package](https://github.com/UofUEpiBio/epiworldR).

## Citation

If you use the measles package in your research, please cite both this
package and epiworldR:

``` r
citation("measles")
citation("epiworldR")
```

## Contributing

Contributions are welcome! Please see the [`measles` development
guidelines](https://github.com/UofUEpiBio/measles/blob/main/DEVELOPMENT.md)
for information on how to contribute.

## Acknowledgments

This project was made possible by cooperative agreement
CDC-RFA-FT-23-0069 from the CDC’s Center for Forecasting and Outbreak
Analytics. Its contents are solely the responsibility of the authors and
do not necessarily represent the official views of the Centers for
Disease Control and Prevention.

# brfinance 📊🇧🇷

[![CRAN Status](https://www.r-pkg.org/badges/version/brfinance)](https://cran.r-project.org/package=brfinance)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/efram2/brfinance/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/efram2/brfinance/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/brfinance)](https://cran.r-project.org/package=brfinance)
[![Downloads](https://cranlogs.r-pkg.org/badges/brfinance)](https://cran.r-project.org/package=brfinance)
[![GitHub stars](https://img.shields.io/github/stars/efram2/brfinance.svg)](https://github.com/efram2/brfinance/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/efram2/brfinance.svg)](https://github.com/efram2/brfinance/network)


# Overview

The `brfinance` package simplifies access to official Brazilian macroeconomic data from reliable institutions such as the Central Bank of Brazil (BCB) and the IBGE.
It is designed to make it easier to collect, organize, and visualize key indicators like inflation, interest rates, and unemployment, directly from R.

# Installation

```r
install.packages("brfinance")

# Or development version from GitHub
install.packages("devtools")
devtools::install_github("efram2/brfinance")

library(brfinance)

```

# Quick Start

```R
library(brfinance)

# Download inflation data for 2023
inflation <- get_inflation_rate("2023-01-01")
head(inflation)

# Retrieve SELIC rate from 2020 to 2024
selic <- get_selic_rate(2020, 2024)
head(selic)

# Retrieve unemployment data
unemployment <- get_unemployment(2020, 2024)
head(unemployment)
```
# Language Support

All functions support both English and Portuguese through the language parameter:

* language = "eng" (default): Returns English column names and labels
* language = "pt": Returns Portuguese column names and labels

# About the data

All data used in brfinance is retrieved from official Brazilian institutions:

* SGS (Sistema Gerenciador de Séries Temporais) by the Central Bank of Brazil
* SIDRA (Sistema IBGE de Recuperação Automática) by IBGE

The package aims to simplify the access and visualization of key Brazilian macroeconomic indicators, especially for researchers, students, and analysts who work with national economic data.
#  Functions Overview

## 1. Inflation Analysis

### `get_inflation_rate()`

Downloads monthly IPCA inflation data from the Central Bank of Brazil.

**Parameters**

- `start_date`: Start date `"YYYY-MM-DD"` (default: `"2012-01-01"`)
- `end_date`: End date `"YYYY-MM-DD"` (default: `NULL`, most recent)
- `language`: Column names — `"eng"` (default) or `"pt"`
- `labels`: Whether to add variable labels (`TRUE`, default)

**Usage**
```r
devtools::install_github("efram2/brfinance")
library(brfinance)

# Complete data (default)
inflation <- get_inflation_rate()

# Specific period in English
inflation_eng <- get_inflation_rate(
  start_date = "2020-01-01",
  end_date = "2024-12-01",
  language = "eng"
)

# Portuguese version
inflation_pt <- get_inflation_rate(
  start_date = "2020-01-01",
  language = "pt"
)
```

## 2. Selic and Interest Rates

### `get_selic_rate()`

Downloads daily SELIC rate from the Central Bank of Brazil.

**Parameters**

- `start_year`: Start date `"YYYY-MM-DD"`
- `end_year`: End date `"YYYY-MM-DD"`
- `language`: Column names — `"eng"` (default) or `"pt"`

**Usage**

```R
library(brfinance)

# English version
selic_eng <- get_selic_rate(2020, 2024)
head(selic_eng)

# Portuguese version
selic_pt <- get_selic_rate(2020, 2024, language = "pt")
head(selic_pt)
```
### `plot_selic_rate()`

Creates a time series plot of the SELIC rate.

**Parameters**

- `data`: Output from `"get_selic_rate()"`
- `language`: Column names — `"eng"` (default) or `"pt"`

**Usage**
```r
# Get data
selic_data <- get_selic_rate(2020, 2024)

# Create plot
selic_plot <- plot_selic_rate(selic_data, language = "eng")
print(selic_plot)

# Portuguese version
selic_data_pt <- get_selic_rate(2000, 2005, language = "pt")
selic_plot_pt <- plot_selic_rate(selic_data_pt, language = "pt")
print(selic_plot_pt)
```

## 3. Unemployment Data

### `get_unemployment()`

Retrieves Brazil’s quarterly unemployment rate from IBGE’s PNAD Contínua.

**Parameters**

- `start_year`: Start date `"YYYY-MM-DD"`
- `end_year`: End date `"YYYY-MM-DD"`
- `language`: Column names — `"eng"` (default) or `"pt"`

**Usage**
```r
library(brfinance)

# English version
unemp_eng <- get_unemployment(2018, 2024, language = "eng")
head(unemp_eng)

# Portuguese version
unemp_pt <- get_unemployment(2018, 2024, language = "pt")
head(unemp_pt)
``` 

### `plot_unemployment()`

Generates a line chart of Brazil’s unemployment rate.

**Parameters**

- `data`: Output from `"get_unemployment()"`
- `language`: Column names — `"eng"` (default) or `"pt"`

**Usage**
```r
# English
unemp_data <- get_unemployment(2020, 2024)
unemp_plot <- plot_unemployment(unemp_data, language = "eng")
print(unemp_plot)

# Portuguese
unemp_data2 <- get_unemployment(2020, 2024, language = "pt")
unemp_plot_pt <- plot_unemployment(unemp_data2, language = "pt")
print(unemp_plot_pt)
```

# Contribution

Suggestions, feature requests, and pull requests are welcome!


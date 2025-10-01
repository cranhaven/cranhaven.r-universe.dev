# SouthKoreAPIs

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

The `SouthKoreAPIs` package provides a unified interface to access open data from the **World Bank API**, **Nager.Date API**, and the **REST Countries API**, with a focus on **South Korea**. It allows users to retrieve up-to-date or historical information on topics such as economic indicators, population statistics, national holidays, and key geopolitical details.

In addition to API-access functions, the package includes one of the largest curated collections of open datasets related to **South Korea**. These datasets cover a wide range of topics including public health outbreaks, demographics, social surveys, elections, economic indicators, natural disasters, administrative divisions, air quality, climate data, energy consumption, cultural information, and financial markets.


## Installation

You can install the `SouthKoreAPIs` package from CRAN with the following R function:

```R

install.packages("SouthKoreAPIs")

```


## Usage

After installation, load the package and start exploring and using its functions and datasets.

```R

library(SouthKoreAPIs)

```

### SouthKoreAPIs Functions

Below is a list of the main functions included in the package:

- `get_southkorea_child_mortality()`: Get South Korea’s Under-5 Mortality Rate data from the World Bank.

- `get_southkorea_cpi()`: Get South Korea’s Consumer Price Index (2010 = 100) data from the World Bank.

- `get_southkorea_energy_use()`: Get South Korea’s Energy Use (kg of oil equivalent per capita) data from the World Bank.

- `get_southkorea_gdp()`: Get South Korea’s GDP (current US$) data from the World Bank.

- `get_southkorea_holidays()`: Get official public holidays in South Korea for a given year, e.g., `get_southkorea_holidays(2025)`.

- `get_southkorea_hospital_beds()`: Get South Korea’s Hospital Beds (per 1,000 people) data from the World Bank.

- `get_southkorea_literacy_rate()`: Get South Korea’s Adult Literacy Rate data from the World Bank.

- `get_southkorea_life_expectancy()`: Get South Korea’s Life Expectancy at Birth data from the World Bank.

- `get_southkorea_population()`: Get South Korea’s Total Population data from the World Bank.

- `get_southkorea_unemployment()`: Get South Korea’s Total Unemployment Rate data from the World Bank.

- `get_country_info_kr()`: Get key country information for South Korea.

- `view_datasets_SouthKoreAPIs()`: View available curated datasets included in SouthKoreAPIs.

## Dataset Suffixes

Each dataset in `SouthKoreAPIs` is labeled with a *suffix* to indicate its structure and type:

- `_df`: A standard data frame.

- `_tbl_df`: A tibble data frame object.

- `_list`: A list object.

- `_sf`: A sf (Simple Features) object.


## Datasets Included in SouthKoreAPIs

In addition to API access functions, `SouthKoreAPIs` offers one of the largest curated collections of open datasets focused on **South Korea**. These preloaded datasets cover a wide range of topics including public health outbreaks, demographics, social surveys, elections, economic indicators, natural disasters, administrative divisions, air quality, climate data, energy consumption, cultural information, and financial markets. Below are some featured examples:

- **MERSKorea2015_list**: A list containing two data frames with information collected during the first weeks of the 2015 MERS-CoV outbreak in South Korea.

- **KoreanSocialSurvey_tbl_df**: A tibble containing a sample of data from the Korean General Social Survey (KGSS) conducted in 2023.

- **SeoulMosquito_tbl_df**: A tibble containing daily mosquito indicator data and weather measurements for Seoul, South Korea, from 2016 to 2019.


## Example Code:

```R

# Load the package

library(SouthKoreAPIs)

# Retrieves essential information about South Korea

get_country_info_kr()

# Get South Korea's Population (Total) from World Bank

get_southkorea_population()

# Load a dataset

data(SouthKoreaBirths_tbl_df)

# Shows six rows of the dataset

head(SouthKoreaBirths_tbl_df)

# Display the structure of the dataset

str(SouthKoreaBirths_tbl_df)

# Shows the whole dataset

View(SouthKoreaBirths_tbl_df)


```

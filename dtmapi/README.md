<br>

<p align="center">
  <img alt="DTM Logo" src="https://dtm.iom.int/themes/custom/dtm_global/logo.svg" width="400">
</p>

-----------------

## About
`dtmapi` is an R package developed by [Displacement Tracking Matrix (DTM)](https://dtm.iom.int/). This package allows the humanitarian community, academia, media, government, and non-governmental organizations to utilize the data collected by DTM. It provides non-sensitive Internally Displaced Person (IDP) figures, aggregated at the country, Admin 1 (states, provinces, or equivalent), and Admin 2 (smaller subnational administrative areas) levels. Country Name and Operation can be found in this [data coverage](https://dtm.iom.int/data-and-analysis/dtm-api/data-coverage) matrix. 

Please find more information about [DTM API](https://dtm.iom.int/data-and-analysis/dtm-api) here.

## Installation
The `dtmapi` package is available on [CRAN](https://CRAN.R-project.org/package=dtmapi) and can be installed using the following command:

```sh
install.packages("dtmapi")
```

## Load Package
After installation, load the package using library():
```sh
library(dtmapi)
```

## Usage
Here's a quick example to get you started:
```R
# Get all countries for which DTM data is publicly available through the API.
countries_df <- get_all_countries()
head(countries_df)

# Get all operations for which DTM data is publicly available through the API.
operations_df <- get_all_operations()
head(operations_df)

# Get IDP Admin 0 Data for Ethiopia from Round 1 to Round 10
idp_admin0_df <- get_idp_admin0_data(CountryName='Ethiopia', FromRoundNumber=1, ToRoundNumber=10)
head(idp_admin0_df)

# Get IDP Admin 1 Data for Sudan from reporting date 2020-01-01 to 2024-08-15
idp_admin1_df <- get_idp_admin1_data(CountryName='Sudan', Admin1Name="Blue Nile", FromReportingDate='2020-01-01', ToReportingDate='2024-08-15')
head(idp_admin1_df)

# Get IDP Admin 2 Data for Lebanon
idp_admin2_df <- get_idp_admin2_data(Operation="Displacement due to conflict", CountryName='Lebanon')
head(idp_admin2_df)
```
## Documentation
Comprehensive documentation is available at [github.io](https://displacement-tracking-matrix.github.io/dtmapi-R/).

## Source Code
The source code for `dtmapi` is available on [GitHub](https://github.com/Displacement-tracking-Matrix/dtmapi-R).

Feel free to explore the repository, contribute, or raise any issues you may encounter.

## Contact
For any questions or feedback, please reach out to us at [dtmdataconsolidation@iom.int](mailto:dtmdataconsolidation@iom.int).

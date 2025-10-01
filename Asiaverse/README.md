# Asiaverse

The **`Asiaverse`** is a metapackage that brings together a comprehensive collection of R packages providing access to APIs functions and curated datasets from **China**, **Japan**, **South Korea**, **India**, and **Indonesia**. 

It integrates both real-time and historical data retrieved through public RESTful APIs, such as:
- *Nager.Date*
- *World Bank API* 
- *REST Countries API*

The **Asiaverse** also offers extensive curated collections of open datasets covering economics, demographics, public health, environmental data, natural disasters, political indicators, and social metrics.

## Installation

To install the `Asiaverse` package, use the following:

```r 

# Install from CRAN 
install.packages("Asiaverse")

# Then load the package:
library(Asiaverse)


```

## Using the Asiaverse() Function

Once the package is loaded, you can call the Asiaverse() function to display the list of included packages and their versions:

```r 

Asiaverse()

```


## Important Note on Detaching Packages

`Asiaverse` imports and depends on several subpackages. Therefore, you cannot detach an individual subpackage (like SouthKoreAPIs) while Asiaverse is still loaded.

### Example of an Error


```r 

# This will raise an error
detach("package:SouthKoreAPIs", unload = TRUE)

```

## Correct Way to Detach

To properly unload a subpackage, you must first detach Asiaverse:

```r

# First detach the metapackage
detach("package:Asiaverse", unload = TRUE)

# Now you can safely detach the subpackage
detach("package:SouthKoreAPIs", unload = TRUE)

```

By installing the `Asiaverse` package this will attach the following packages to your R session:

- `ChinAPIs`

- `JapanAPIs`

- `SouthKoreAPIs`

- `IndiAPIs`

- `IndonesiAPIs`


## Included Packages in the Asiaverse

### ChinAPIs

The `ChinAPIs` package provides a unified interface to access open data from the World Bank API, Nager.Date API, and the REST Countries API, with a focus on **China**. It allows users to retrieve up-to-date information on topics such as economic indicators, population statistics, unemployment rates, holidays, and basic geopolitical details.

In addition to API-access functions, the package includes one of the largest curated collections of open datasets related to China and Hong Kong. These datasets cover areas such as air quality, demographic indicators, input-output economic tables, epidemiology, administrative divisions, name distributions, political structure, and various social indicators.


### JapanAPIs

The `JapanAPIs` package provides a unified interface to access open data from the World Bank API, Nager.Date API, and the REST Countries API, with a focus on **Japan**. It allows users to retrieve up-to-date or historical information on topics such as economic indicators, population statistics, national holidays, and basic geopolitical details.

In addition to API-access functions, the package includes one of the largest curated collections of open datasets related to Japan. These datasets cover a wide range of topics including natural disasters, economic production, the vehicle industry, air quality, demographic trends, and administrative divisions.


### SouthKoreAPIs

The `SouthKoreAPIs` package provides a unified interface to access open data from the World Bank API, Nager.Date API, and the REST Countries API, with a focus on **South Korea**. It allows users to retrieve up-to-date or historical information on topics such as economic indicators, population statistics, national holidays, and key geopolitical details.

In addition to API-access functions, the package includes one of the largest curated collections of open datasets related to South Korea. These datasets cover a wide range of topics including public health outbreaks, demographics, social surveys, elections, economic indicators, natural disasters, administrative divisions, air quality, climate data, energy consumption, cultural information, and financial markets.

### IndiAPIs

The `IndiAPIs` package provides a unified interface to access open data from the World Bank API and the REST Countries API, with a focus on **India**. It allows users to retrieve up-to-date or historical information on topics such as economic indicators, international demographic statistics, and key geopolitical details related to India.

In addition to API-access functions, the package includes one of the largest curated collections of open datasets focused on India. These datasets cover a wide range of topics including population, economy, weather, politics, health, biodiversity, sports, agriculture, cybercrime, infrastructure, and more.


### IndonesiAPIs

The `IndonesiAPIs` package provides a unified interface to access open data from the **World Bank API**, **Nager.Date API**, and the **REST Countries API**, with a focus on **Indonesia**. It allows users to retrieve up-to-date or historical information on topics such as economic indicators, population statistics, national holidays, and basic geopolitical details.

In addition to API-access functions, the package includes a curated collection of open datasets related to **Indonesia**. These datasets cover a wide range of topics including consumer prices, poverty probability, food prices by region, tourism destinations, and minimum wage statistics.


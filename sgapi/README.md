<p align="center">
<img width="554" alt="Defra logo" src="https://github.com/Defra-Data-Science-Centre-of-Excellence/sgapi/assets/126087299/1c7cfe02-87cd-407e-b245-991374cfc488">
</p>

## Overview

The sgapi package provides a simple wrapper around the nomis and Open Geography Portal APIs, aiding geospatial analysis of the population, labour market, and social measures. Simplifies process of querying nomis datasets and extracting area shapefiles at chosen resolution from ONS Open Geography.

The authors thank ONS for their work in maintaining both NOMIS and Open Geography Portal, without them this work would not be possible.

More information on theses sources can be found here:

[**ONS Open Geography Portal**](https://geoportal.statistics.gov.uk/)

[**nomis**](https://www.nomisweb.co.uk/)

Users must abide by the licensing agreements when publishing information extracted from these ONS sources, for more copyright information, see the [copyright](##copyright-and-permissions) section. 

## Installation
     install.packages("sgapi")
     
### Install Development Version
    install.packages("remotes")
    remotes::install_github("https://github.com/Defra-Data-Science-Centre-of-Excellence/sgapi")

## Quick start

```R
# List available nomis tables
list_tables()

# Pull all data from the "Jobseeker's Allowance" dataset
get_ons_table("nm_1_1")

# Filter "Jobseeker's Allowance" dataset and 'select' columns to output
get_ons_table("nm_1_1",
              geography = "TYPE480", time = "latest", measures = 20100, item = 1,
              select = c("geography_name", "sex_name", "obs_value"))

# Aggregate statistics using 'rows' and 'cols'
get_ons_table("nm_1_1",
              geography = "TYPE480", time = "latest", measures = 20100, item = 1,
              select = c("geography_name", "sex_name", "obs_value"),
              rows = c("geography_name"), cols = c("sex_name"))
```

## Key Functions

1. **get_boundaries.R** - returns shapefile of areas in contact with a user selected rectangular area, at your chosen ONS resolution
2. **get_boundaries_areanames.R** - returns the shapefiles for all areas input into the function.
3. **get_table_dimensions.R** - for a chosen nomis table this function returns all of the parameters which can be filtered
4. **get_ons_table.R** - extracts a dataframe from the chosen nomis table for your selected area and selected filters
5. **get_table_link_lookup.R** - retrieves a dataframe with the lookup table between two resolutions, taken from ONS Open Geography lookup tables
6. **get_table_info_brief.R** - returns summary information for the selected nomis table, including contact details, data description and table status

## Exploratory Functions

1. **list_boundaries.R** - lists all boundary masks available on the ONS Open Geography
2. **list_tables.R** - lists all tables, including their name and reference code, from nomis
3. **get_available_scales.R** - provides list of available geographical resolutions for your chosen nomis table
4. **list_data_sources.R** - lists all available data sources on nomis

## Additional API Information

[**ONS Open Geography Portal**](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm)

[**nomis**](https://www.nomisweb.co.uk/api/v01/help)

## Copyright and Permissions

### ONS Open Geography Portal 
Terms and conditions of supply
Digital boundary products and reference maps are supplied under the Open Government Licence. You must use the following copyright statements when you reproduce or use this material:

**Source: Office for National Statistics licensed under the Open Government Licence v.3.0**
**Contains OS data © Crown copyright and database right [year]**

### nomis

All material on the Office for National Statistics (ONS) and Nomis websites is subject to Crown Copyright protection unless otherwise indicated.

**Source: Office for National Statistics**
           
For more information on the Open Government Licence, see the [open government license](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) 

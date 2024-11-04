# datagovsgR
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/clintonwxy/datagovsgR/graphs/commit-activity)
![!Downloads](https://cranlogs.r-pkg.org/badges/grand-total/datagovsgR)


datagovsgR is a R package used to access data-frames returned by [Developer API Tools](https://data.gov.sg/developer) from Data.gov.sg. The package calls upon the real-time APIs, to obtain information such as taxi availability,  real time weather readings, weather forecasts and PSI readings.



## Installation

You can install the latest version of datagovsgR though CRAN.

```r
install.packages("datagovsgR")
```
Alternatively, you can install the developmental version of datagovsgR through `install_github` in *R*.

``` r
install.packages("devtools")
devtools::install_github("clintonwxy/datagovsgR")
```


## Using datagovsgR

The API provided by [data.gov.sg](https://data.gov.sg/developer) does not require an API key and hence the package is able to query the relevant information directly. Below are two examples of the functions within the package

#### PSI
The `psi` function is shown below, which returns 12 different psi measures across 5 sectors in Singapore for a given date and time. The user is able query a specific date and time, or for current available information by ommiting the parameter.

``` r
psi(date_time = "2019-11-08T17:30:02")
#> Closest timestamp: 2019-11-08T17:00:00+08:00
#>                psi_measures  west national  east central south north
#>  1             o3_sub_index 12.00    20.00 13.00   20.00 13.00 17.00
#>  2  pm10_twenty_four_hourly 33.00    39.00 35.00   29.00 39.00 30.00
#>  3           pm10_sub_index 33.00    39.00 35.00   29.00 39.00 30.00
#>  4             co_sub_index  4.00     9.00  9.00    2.00  4.00  5.00
#>  5  pm25_twenty_four_hourly 21.00    22.00 21.00   18.00 22.00 19.00
#>  6            so2_sub_index  8.00     8.00  3.00    3.00  8.00  5.00
#>  7        co_eight_hour_max  0.43     0.95  0.95    0.24  0.39  0.52
#>  8         no2_one_hour_max 12.00    33.00 27.00   14.00 33.00 20.00
#>  9   so2_twenty_four_hourly 13.00    13.00  5.00    4.00 13.00  8.00
#> 10           pm25_sub_index 61.00    63.00 61.00   57.00 63.00 59.00
#> 11   psi_twenty_four_hourly 61.00    63.00 61.00   57.00 63.00 59.00
#> 12        o3_eight_hour_max 27.00    48.00 32.00   48.00 30.00 41.00
```

#### Taxi Availability
The `taxi_availability` function returns the total number of available taxis for a given date and time, and their locations in latitude and longitude. Similarly, ommitting the `date_time` parameter returns the latest available information.

``` r
taxi_availability(date_time = "2019-08-07T09:30:00") %>% 
    head()
#> Timestamp: 2019-08-07T09:29:55+08:00
#> Availible Taxis: 4586
#>       long     lat
#> 1 103.6142 1.25267
#> 2 103.6235 1.28648
#> 3 103.6236 1.30047
#> 4 103.6238 1.28678
#> 5 103.6275 1.31123
#> 6 103.6282 1.31332
```


## Coverage

| APIs      | Availability  | Description/Remarks      | 
| :------------------:  |:---------:| :-----------------------------------------|
| Pollutant Standards Index (PSI) | :heavy_check_mark: | National Environment Agency / Overall and regional PSI data (24-hr PSI, Pollutant Concentration and Sub-Index) |
| Carpark Availability | :heavy_check_mark: | GovTech / Get the latest carpark availability in Singapore: - Retrieved every minute - Use the date_time parameter to retrieve the latest carpark availability at that moment in time - Detailed carpark information can be found at https://data.gov.sg/dataset/hdb-carpark-information - Limited to 60 requests per minute per API key |
| Realtime Weather Readings across Singapore |:heavy_check_mark:| National Environment Agency / NEA provides APIs for readings of temperature, humidity, precipitation and wind conditions at up to one-minute intervals. The data is provided at weather-station level. |
| Ultra-violet Index (UVI) | :heavy_check_mark: | National Environment Agency / UV Index value averaged over the past hour. Updated every hour between 7 AM and 7 PM everyday. | 
| Traffic Images |:heavy_check_mark:| Land Transport Authority / Returns links to images of live traffic conditions along expressways and Woodlands & Tuas Checkpoints. |
| Taxi Availability | :heavy_check_mark: | Land Transport Authority / For access to real-time taxi availability data. Returns location coordinates of all Taxis that are currently available for hire. Does not include "Hired" or "Busy" Taxis. |
| PM2.5 | :heavy_check_mark: | National Environment Agency / Regional hourly PM2.5 value measured in μg/m3 |
| Weather Forecast | :heavy_check_mark: | National Environment Agency / Weather forecast for next 2 hours, next 24 hours and next 4 days. |

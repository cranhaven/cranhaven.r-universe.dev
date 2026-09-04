# kofdata - Get Data from the KOF Datenservice API

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kofdata)](https://cran.r-project.org/package=kofdata)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/kofdata)](https://cran.r-project.org/package=kofdata)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/kofdata)](https://cran.r-project.org/package=kofdata)
[![license](https://img.shields.io/badge/license-gplv3-lightgrey.svg)](https://choosealicense.com/)


Read Swiss time series data from the KOF Datenservice API. The API provides macroeconomic survey data, business cycle and further macro economic time series about Switzerland. The package itself is a set of wrappers around API. The kofdata package is able to consume public information as well as data that needs an API token.

For further information on the data, the API and the KOF Swiss Economic Institute please consult the corresponding links. 

## Getting Started - Installation


### Official CRAN Version

Fire up R and run install.packages or use the GUI, if you got one. 

```
install.packages("kofdata")

```

### Developer Version from github

The easiest way to install the very latest version that might not have completed all checks (but contains the latest, hottest, most awesome, most exploratory, adventurous features) is to run: 

```
devtools::install_github("KOF-ch/kofdata")
```

Note to those of you who do not use R regularly: in order to install directly from github make sure the `devtools` package is installed. 


## Example Usage


### Basic Usage

Read the [KOF Barometer](https://kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-economic-barometer.html) into your R Session. The resulting object will be a list of time series objects. In this case it's a named list of length **1** because only a **single series** was queried. 

```r
tsl <- get_time_series("ch.kof.barometer")
length(tsl)
# [1] 1
str(tsl)
# List of 1
# $ ch.kof.barometer: Time-Series [1:368] from 1991 to 2022: 78.3 69.3 69.5 78 77.9 ...
```

You can also query a vector of time series. In this example we read **3** time series from the KOF labor market index into R. 

```
labor <- get_time_series(
  c(
    "ch.kof.ie.retro.ch_total.ind.d11",
    "ch.kof.ie.retro.ch_total.ass.d11",
    "ch.kof.ie.retro.ch_total.exp.d11"
    )
  )
  
str(labor)  
# List of 3
# $ ch.kof.ie.retro.ch_total.ind.d11: Time-Series [1:117] from 1992 to 2022: -19.4 -26.4 # -22.2 -20.4 -17.8 ...
#  $ ch.kof.ie.retro.ch_total.ass.d11: Time-Series [1:117] from 1992 to 2022: -25.5 -33.5 # -30.5 -30.1 -28.8 ...
# $ ch.kof.ie.retro.ch_total.exp.d11: Time-Series [1:117] from 1992 to 2022: -13.19 -19.31 # -13.96 -10.56 -6.86 ...
  

```

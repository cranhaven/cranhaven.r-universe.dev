
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sgat

<!-- badges: start -->

<!-- badges: end -->

The goal of sgat package is to retrieve information from Google’s
searches’ “Popular Times” and geolocation.

## Installation

You can install the released version of sgat package from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("sgat")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("matiaspoullain/sgat")
```

## Example

Always start by initializing the driver, once per session:

``` r
library(sgat)
initialization_sgat()
#> [1] "Connecting to remote server"
#> $acceptInsecureCerts
#> [1] FALSE
#> 
#> $browserName
#> [1] "firefox"
#> 
#> $browserVersion
#> [1] "85.0.2"
#> 
#> $`moz:accessibilityChecks`
#> [1] FALSE
#> 
#> $`moz:buildID`
#> [1] "20210208133944"
#> 
#> $`moz:geckodriverVersion`
#> [1] "0.28.0"
#> 
#> $`moz:headless`
#> [1] FALSE
#> 
#> $`moz:processID`
#> [1] 57600
#> 
#> $`moz:profile`
#> [1] "C:\\Users\\Matias\\AppData\\Local\\Temp\\rust_mozprofileLtDB4f"
#> 
#> $`moz:shutdownTimeout`
#> [1] 60000
#> 
#> $`moz:useNonSpecCompliantPointerOrigin`
#> [1] FALSE
#> 
#> $`moz:webdriverClick`
#> [1] TRUE
#> 
#> $pageLoadStrategy
#> [1] "normal"
#> 
#> $platformName
#> [1] "windows"
#> 
#> $platformVersion
#> [1] "10.0"
#> 
#> $rotatable
#> [1] FALSE
#> 
#> $setWindowRect
#> [1] TRUE
#> 
#> $strictFileInteractability
#> [1] FALSE
#> 
#> $timeouts
#> $timeouts$implicit
#> [1] 0
#> 
#> $timeouts$pageLoad
#> [1] 300000
#> 
#> $timeouts$script
#> [1] 30000
#> 
#> 
#> $unhandledPromptBehavior
#> [1] "dismiss and notify"
#> 
#> $webdriver.remote.sessionid
#> [1] "2c38b83d-cbc5-4f53-b942-cd79560428dd"
#> 
#> $id
#> [1] "2c38b83d-cbc5-4f53-b942-cd79560428dd"
```

This package was created to use it on restaurants, bars, etc… located in
Ushuaia city, Tierra del Fuego, Argentina. You can retrieve a vector of
them by using the next code:

``` r
restaurants <- ushuaia_restaurants()
head(restaurants)
#> [1] "Moustacchio, San Martín 298, Ushuaia"      
#> [2] "Prana, Av Maipu 505, Ushuaia"              
#> [3] "Kuar, Av Perito Moreno 2232, Ushuaia"      
#> [4] "Kuar 1900, San Martín 471, Ushuaia"        
#> [5] "Hostal del Bosque, Magallanes 709, Ushuaia"
#> [6] "Sur 54 Lodge, A 70 Km de Ushuaia, Ushuaia"
```

You can retrieve the information from some of these restaurants. The
searches are not restricted to Ushuaia, but remember to be specific in
your searches, specially if the location you are looking for is far
away:

``` r
bar_1 <- sgat("coutume, 47 rue de babylone, 75007 paris, france")
head(bar_1)
#>                                              lugar   dia hora concurrencia
#> 1 coutume, 47 rue de babylone, 75007 paris, france lunes    6         0.00
#> 2 coutume, 47 rue de babylone, 75007 paris, france lunes    7         0.00
#> 3 coutume, 47 rue de babylone, 75007 paris, france lunes    8        12.75
#> 4 coutume, 47 rue de babylone, 75007 paris, france lunes    9        22.50
#> 5 coutume, 47 rue de babylone, 75007 paris, france lunes   10        21.00
#> 6 coutume, 47 rue de babylone, 75007 paris, france lunes   11        15.00
#>    latitud longitud fecha.de.busqueda
#> 1 48.85166 2.318299        2021-02-12
#> 2 48.85166 2.318299        2021-02-12
#> 3 48.85166 2.318299        2021-02-12
#> 4 48.85166 2.318299        2021-02-12
#> 5 48.85166 2.318299        2021-02-12
#> 6 48.85166 2.318299        2021-02-12
```

Although, some other Google searches doesn’t have the wanted
information. In that case, NULL is returned:

``` r
bar_2 <- sgat("Moustacchio, San Martín 298, Ushuaia", tiempo.espera = 20)
bar_2
#> NULL
```

You also can search for the information from a single day of the week:

``` r
bar_3 <- sgat_day("coutume, 47 rue de babylone, 75007 paris, france", "martes")
head(bar_3)
#>                                              lugar    dia hora concurrencia
#> 1 coutume, 47 rue de babylone, 75007 paris, france martes    6         0.00
#> 2 coutume, 47 rue de babylone, 75007 paris, france martes    7         0.00
#> 3 coutume, 47 rue de babylone, 75007 paris, france martes    8         8.25
#> 4 coutume, 47 rue de babylone, 75007 paris, france martes    9        13.50
#> 5 coutume, 47 rue de babylone, 75007 paris, france martes   10        19.50
#> 6 coutume, 47 rue de babylone, 75007 paris, france martes   11        24.75
#>    latitud longitud fecha.de.busqueda
#> 1 48.85166 2.318299        2021-02-12
#> 2 48.85166 2.318299        2021-02-12
#> 3 48.85166 2.318299        2021-02-12
#> 4 48.85166 2.318299        2021-02-12
#> 5 48.85166 2.318299        2021-02-12
#> 6 48.85166 2.318299        2021-02-12
```

A new function was added which let’s the user to search for the most
popular places of a city or area according to Tripadvisor:

``` r
pinamar <- tripadvisor_places("Pinamar, Argentina", 10)
pinamar
#>  [1] "La Jirafa"              "Palote Pizza"           "Los troncos"           
#>  [4] "La Armonía"             "Paxapoga del Mar"       "Rada Beach Demuru"     
#>  [7] "Il Garda"               "Parador Barlovento"     "Pescaderia Di Costanzo"
#> [10] "Tante"
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# khisr <a href="https://khisr.damurka.com"><img src="man/figures/logo.png" align="right" height="139" alt="khisr website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/khisr)](https://CRAN.R-project.org/package=khisr)
[![R-CMD-check](https://github.com/damurka/khisr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/damurka/khisr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/damurka/khisr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/damurka/khisr?branch=main)
<!-- badges: end -->

The khisr package is designed to seamlessly integrate with DHIS2,
providing R users with a powerful interface for efficient data
retrieval. DHIS2 is a cornerstone in health information management for
many organisations, and khisr simplifies the process of accessing and
working with DHIS2 data directly within the R environment.

### Key Features

- ***Data Retrieval:*** Easily download and manage data from DHIS2.
- ***Flexible Queries:*** Customize data queries to retrieve specific
  data elements, periods, and organizational units.
- ***Secure Access:*** Manage credentials securely within your R
  environment.

### Use Cases

- Health data analysis for research.
- Monitoring and evaluation of health programs
- Generating reports and dashboards for health information systems.

## Installation

### Stable Release

You can install the release version of khisr from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("khisr")
```

### Development Version

And the development version of khisr like so:

``` r
#install.packages('pak')
pak::pak('damurka/khisr')
```

## Usage

### Load khisr package

``` r
library("khisr")
```

### Auth

The khisr package operates in authenticated mode by default. This means
you’ll need to provide credentials before using any functions that
interact with your DHIS2 instance to download data. To ensure secure
access, khisr offers a convenient way to store your credentials within
your R environment. Refer to the following resource for detailed
instructions on setting your credentials: [set you
credentials](https://khisr.damurka.com/articles/set-your-credentials.html)

``` r
# Option 1: Set credentials directly in R (less secure)

khis_cred(username = 'DHIS2 username', 
          password = 'DHIS2 password', 
          base_url = 'https://<dhis2 server instance>/api')

# Option 2: Set credentials from a secure configuration file (recommended)

khis_cred(config_path = 'path/to/secret.json')
```

Once you’ve established your credentials, you’re ready to leverage
khisr’s functions to download data from your DHIS2 instance.

For this overview, we’ve logged into DHIS2 as a specific user in a
hidden chunk.

## Basic Overview

This is a basic example which shows you how to solve a common problem:

``` r
# Retrieve the organisation units by county (level 2)
counties <- get_organisation_units(level %.eq% '2')
counties
#> # A tibble: 47 × 2
#>    name                   id         
#>    <chr>                  <chr>      
#>  1 Baringo County         vvOK1BxTbet
#>  2 Bomet County           HMNARUV2CW4
#>  3 Bungoma County         KGHhQ5GLd4k
#>  4 Busia County           Tvf1zgVZ0K4
#>  5 Elgeyo Marakwet County MqnLxQBigG0
#>  6 Embu County            PFu8alU2KWG
#>  7 Garissa County         uyOrcHZBpW0
#>  8 Homa Bay County        nK0A12Q7MvS
#>  9 Isiolo County          bzOfj0iwfDH
#> 10 Kajiado County         Hsk1YV8kHkT
#> # ℹ 37 more rows

# Retrieve organisation units by name (level included to ensure it refers to county)
kiambu_county <- get_organisation_units(level %.eq% '2', 
                                        name %.like% 'Kiambu')
kiambu_county
#> # A tibble: 1 × 2
#>   name          id         
#>   <chr>         <chr>      
#> 1 Kiambu County qKzosKQPl6G

# Retrieve all data elements by data element group for outpatient (data element group name MOH 705)
moh_705 <- get_data_elements(dataElementGroups.name %.like% 'moh 705')
moh_705
#> # A tibble: 96 × 2
#>    name                         id         
#>    <chr>                        <chr>      
#>  1 Abortion                     IrWSgk9GsUm
#>  2 All other diseases           KxT47tbKHsd
#>  3 Anaemia cases                kkUHOwGMawD
#>  4 Arthritis, Joint pains etc.  waNhWrS3HL6
#>  5 Asthma                       L82lvvxVaqt
#>  6 Autism                       L529r3Wvtcf
#>  7 Bilharzia  (Schistosomiasis) ojFSHMwbkHK
#>  8 Brucellosis                  nb9cfWgxYFc
#>  9 Burns                        dkEYL9Sous9
#> 10 Cardiovascular conditions    sZETzNe1To8
#> # ℹ 86 more rows

# Filter the data element to element that contain malaria
malaria <- get_data_elements(dataElementGroups.name %.like% 'moh 705', 
                             name %.like% 'malaria')
malaria
#> # A tibble: 4 × 2
#>   name                                    id         
#>   <chr>                                   <chr>      
#> 1 Confirmed Malaria (only Positive cases) OoakJhWiyZp
#> 2 Malaria in pregnancy                    gvZmXInRLuD
#> 3 MOH 705A Rev 2020_ Tested for Malaria   siOyOiOJpI8
#> 4 Suspected  Malaria                      Lt0FqtnHraW

# Retrieve data for malaria in Kiambu county in the outpatient data element groups
data <- get_analytics(
        dx %.d% malaria$id,
        pe %.d% 'LAST_YEAR',
        ou %.f% kiambu_county$id
    ) %>%
    left_join(malaria, by = c('dx'='id'))
data
#> # A tibble: 4 × 4
#>   dx          pe    value name                                   
#>   <chr>       <chr> <dbl> <chr>                                  
#> 1 Lt0FqtnHraW 2023  31101 Suspected  Malaria                     
#> 2 OoakJhWiyZp 2023   5092 Confirmed Malaria (only Positive cases)
#> 3 siOyOiOJpI8 2023  20554 MOH 705A Rev 2020_ Tested for Malaria  
#> 4 gvZmXInRLuD 2023    397 Malaria in pregnancy
```

## Where to learn more

[Get Started](https://khisr.damurka.com/articles/khisr.html) is a more
extensive general introduction to khisr.

Browse the [articles
index](https://khisr.damurka.com/articles/index.html) to find articles
that cover various topics in more depth.

See the [function index](https://khisr.damurka.com/reference/index.html)
for an organized, exhaustive listing.

## Code of Conduct

Please note that the khisr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

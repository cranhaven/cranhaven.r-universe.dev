
<!-- README.md is generated from README.Rmd. Please edit that file -->

# njtr1 <a href='https://gavinrozzi.github.io/njtr1/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/gavinrozzi/njtr1/workflows/R-CMD-check/badge.svg)](https://github.com/gavinrozzi/njtr1/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version-last-release/njtr1)](https://www.r-pkg.org/badges/version-last-release/njtr1)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/njtr1)](https://cranlogs.r-pkg.org/badges/grand-total/njtr1)
<!-- badges: end -->

[njtr1](https://gavinrozzi.github.io/njtr1/index.html) is an R package
that provides provides an interface to easily download car crash records
maintained by the New Jersey Department of Transportation. These records
are collected from police officers in New Jersey when they complete the
form NJTR-1 following a motor vehicle crash that is reported to law
enforcement.

This package allows for the download of multiple tables of crash records
and cleans and formats the data for analysis.

More details about the NJTR-1 crash reporting process is available
through the [New Jersey Department of
Transportation.](https://www.state.nj.us/transportation/refdata/accident/pdf/NJTR-1CrashReportManual12517.pdf)

This package currently supports the download of all crash table data
maintained by NJDOT from 2001-2020.

## Installation

You can install the released version of njtr1 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("njtr1")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gavinrozzi/njtr1")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(njtr1)

# Get crash data for the year 2020
acc_20 <- get_njtr1(year = 2020, type = "Accidents")
#> njtr1: downloading data

head(acc_20)
#>                      id county_name municipality_name crash_date
#> 1   202001012020-020639    ATLANTIC      ABSECON CITY 2020-08-03
#> 2   202001012020-021265    ATLANTIC      ABSECON CITY 2020-08-10
#> 3    202001012020005031    ATLANTIC      ABSECON CITY 2020-02-29
#> 4    202001012020025300    ATLANTIC      ABSECON CITY 2020-04-29
#> 5 20200101I-2020-000186    ATLANTIC      ABSECON CITY 2020-01-02
#> 6 20200101I-2020-000255    ATLANTIC      ABSECON CITY 2020-01-03
#>   crash_day_of_week crash_time police_dept_code police_department
#> 1                MO        231                1   ABSECON CITY PD
#> 2                MO       1747                1   ABSECON CITY PD
#> 3                SA       1654                1  PLEASANTVILLE PD
#> 4                WE       2330                1 EGG HARBOR TWP PD
#> 5                TH        835                1   ABSECON CITY PD
#> 6                FR        244                1   ABSECON CITY PD
#>   police_station total_killed total_injured pedestrians_killed
#> 1                           0             0                  0
#> 2                           0             0                  0
#> 3                           0             0                  0
#> 4       EHTPD HQ            0             0                  0
#> 5                           0             0                  0
#> 6                           0             0                  0
#>   pedestrians_injured severity intersection alcohol_involved hazmat_involved
#> 1                   0        P            B                Y               N
#> 2                   0        P            I                N               N
#> 3                   0        P            I                N               N
#> 4                   0        P            B                N               N
#> 5                   0        P            B                N               N
#> 6                   0        P            B                Y               N
#>   crash_type_code total_vehicles_involved      crash_location
#> 1               4                       2 ATLANTIC COUNTY 663
#> 2               3                       2               US 30
#> 3               3                       2                US 9
#> 4              11                       1 ATLANTIC COUNTY 685
#> 5              11                       1               US 30
#> 6              11                       1           ROUTE 585
#>   location_direction route route_suffix sri_std_rte_identifier milepost
#> 1                      663                          01000663__     1.34
#> 2                  E    30                          00000030__    52.30
#> 3                  S     9                          00000009__    41.87
#> 4                  N   685                          01000685__     0.23
#> 5                       30                          00000030__    54.26
#> 6                      585                          00000585__     9.44
#>   road_system road_character road_horizontal_alignment road_grade
#> 1           5             NA                         1          4
#> 2           2             NA                         2          6
#> 3           2             NA                         1          4
#> 4           5             NA                         1          4
#> 5           2             NA                         1          4
#> 6           5             NA                         1          4
#>   road_surface_type surface_condition light_condition environmental_condition
#> 1                 2                 1               7                       1
#> 2                 2                 1               1                       1
#> 3                 2                 1               1                       1
#> 4                 2                 1               6                       1
#> 5                 2                 1               1                       1
#> 6                 2                 1               6                       1
#>   road_divided_by temporary_traffic_control_zone distance_to_cross_street
#> 1               5                              1                       10
#> 2               1                              1                       NA
#> 3               5                              1                       NA
#> 4               5                              1                     1200
#> 5               1                              1                      882
#> 6               5                              1                      150
#>   unit_of_measurement directn_from_cross_street
#> 1                  FE                         E
#> 2                  AT                          
#> 3                  AT                          
#> 4                  FE                         N
#> 5                  FE                         E
#> 6                  FE                         S
#>                     cross_street_name is_ramp ramp_tofrom_route_name
#> 1  ROUTE 585 / N MAIN ST / S SHORE RD      NA                       
#> 2  CR 631 / ILLINOIS AVE / TURNER AVE      NA                       
#> 3 CR 663 / CALIFORNIA AVE / W CALIFOR      NA                       
#> 4                    CR 651 / MILL RD      NA                       
#> 5                 CR 646 / DELILAH RD      NA                       
#> 6                   CR 630 / OHIO AVE      NA                       
#>   ramp_tofrom_route_direction posted_speed posted_speed_cross_street
#> 1                                       25                        35
#> 2                                       45                        25
#> 3                                       30                        25
#> 4                                       45                        45
#> 5                                       50                        25
#> 6                                       35                        35
#>   first_harmful_event latitude longitude cell_phone_in_use_flag
#> 1                  26       NA        NA                      N
#> 2                  26       NA        NA                      N
#> 3                  26 39.41376  74.51296                      N
#> 4                  52 39.43580  74.53661                      N
#> 5                  19       NA        NA                      N
#> 6                  52       NA        NA                      N
#>   other_property_damage reporting_badge_no
#> 1                                      861
#> 2                                      861
#> 3                                     1745
#> 4                                     5191
#> 5                                      847
#> 6  UTILITY POLE #P19002                845

# Get vehicle data for the year 2020
veh_20 <- get_njtr1(year = 2020, type = "Vehicles")
#> njtr1: downloading data

head(veh_20)
#>                    id vehicle_number insurance_company_code owner_state
#> 1 202001012020-020639              1                    946          NJ
#> 2 202001012020-020639              2                    100          NJ
#> 3 202001012020-021265              1                    413          NJ
#> 4 202001012020-021265              2                     12          NJ
#> 5  202001012020005031              1                    962          NJ
#> 6  202001012020005031              2                    206          NJ
#>   make_of_vehicle model_of_vehicle color_of_vehicle year_of_vehicle
#> 1             KIA         FORTE FE               BK            2020
#> 2           LEXUS           GS 350               WT            2014
#> 3        CHRYSLER           TOWN &               BK            2006
#> 4           DODGE       CHALLENGER               RD            2019
#> 5            FORD              FUS               BK            2016
#> 6            FORD              E35               WT            2005
#>   license_plate_state vehicle_weight_rating towed removed_by
#> 1                  NJ                          NA          3
#> 2                  NJ                          NA         NA
#> 3                  NJ                          NA         NA
#> 4                  NJ                          NA         NA
#> 5                  NJ                          NA          1
#> 6                  NJ                          NA          2
#>   drivenleft_at_scenetowed initial_impact_location principal_damage_location
#> 1                        5                      11                        11
#> 2                        1                      12                         1
#> 3                        1                       1                         1
#> 4                        1                      10                        10
#> 5                        1                      11                        11
#> 6                        1                       3                         3
#>   extent_of_damage traffic_controls_present vehicle_type vehicle_use
#> 1                4                        8            1           1
#> 2                3                        8            1           1
#> 3                3                        3            1           1
#> 4                3                        3            1           1
#> 5                2                        3            1           1
#> 6                3                        3            3           2
#>   special_function_vehicles cargo_body_type contributing_circumstances_1
#> 1                        NA              NA                            5
#> 2                        NA              NA                           25
#> 3                        NA              NA                            4
#> 4                        NA              NA                           25
#> 5                        NA              NA                           19
#> 6                         1              NA                           25
#>   contributing_circumstances_2 direction_of_travel precrash_action
#> 1                            2                   2               1
#> 2                           NA                   4               7
#> 3                           NA                   1               3
#> 4                           NA                   2               2
#> 5                           NA                   2               2
#> 6                           NA                   3               1
#>   first_sequence_of_events second_sequence_of_events third_sequence_of_events
#> 1                       26                        NA                       NA
#> 2                       26                        NA                       NA
#> 3                       26                        NA                       NA
#> 4                       26                        NA                       NA
#> 5                       26                        NA                       NA
#> 6                       26                        NA                       NA
#>   fourth_sequence_of_events most_harmful_event oversizeoverweight_permit
#> 1                        NA                 26                        NA
#> 2                        NA                 26                        NA
#> 3                        NA                 26                        NA
#> 4                        NA                 26                        NA
#> 5                        NA                 26                        NA
#> 6                        NA                 26                        NA
#>   hazmat_status hazmat_class hazmat_placard usdot_number mcmx_number
#> 1                                                                   
#> 2                                                                   
#> 3                                                                   
#> 4                                                                   
#> 5                                                                   
#> 6                                                                   
#>   usdot__other_flag usdot__other_number carrier_name hit__run_driver_flag
#> 1                NA                  NA                                  
#> 2                NA                  NA                                  
#> 3                NA                  NA                                  
#> 4                NA                  NA                                  
#> 5                NA                  NA                                  
#> 6                NA                  NA

# Get driver data for the year 2020
driv_20 <- get_njtr1(year = 2020, type = "Drivers")
#> njtr1: downloading data

head(driv_20)
#>                    id vehicle_number    driver_city driver_state
#> 1 202001012020-020639              1   SOMERS POINT           NJ
#> 2 202001012020-020639              2 EGG HARBOR TWP           NJ
#> 3 202001012020-021265              1       BARNEGAT           NJ
#> 4 202001012020-021265              2  PLEASANTVILLE           NJ
#> 5  202001012020005031              1 EGG HARBOR TWP           NJ
#> 6  202001012020005031              2   SOMERS POINT           NJ
#>   driver_zip_code driver_license_state driver_dob driver_sex alcohol_test_given
#> 1           08244                   NJ 1994-01-21          F                  Y
#> 2           08234                   NJ 1995-06-14          F                  N
#> 3           08005                   NJ 1945-10-22          M                  N
#> 4           08232                   NJ 1946-07-17          M                  N
#> 5           08234                   NJ 2000-10-21          F                  N
#> 6           08244                   NJ 1983-11-10          M                  N
#>   alcohol_test_type alcohol_test_results charge_1  summons_1 charge_2
#> 1                BR                  .22  39:4-50 E20-000708  39:4-96
#> 2                                                                    
#> 3                                                                    
#> 4                                                                    
#> 5                                                                    
#> 6                                                                    
#>    summons_2 charge_3  summons_3 charge_4 summons_4 driver_physical_status_1
#> 1 E20-000709 39:4-88B E20-000710                                          NA
#> 2                                                                         NA
#> 3                                                                         NA
#> 4                                                                         NA
#> 5                                                                         NA
#> 6                                                                         NA
#>   driver_physical_status_2
#> 1                       NA
#> 2                       NA
#> 3                       NA
#> 4                       NA
#> 5                       NA
#> 6                       NA

# Get occupant data for the year 2020
occ_20 <- get_njtr1(year = 2020, type = "Occupants")
#> njtr1: downloading data

head(occ_20)
#>                    id vehicle_number occupant_number physical_condition
#> 1 202001012020-020639              1               1                 NA
#> 2 202001012020-020639              2               2                 NA
#> 3 202001012020-021265              1               1                 NA
#> 4 202001012020-021265              1               2                 NA
#> 5 202001012020-021265              1               3                 NA
#> 6 202001012020-021265              2               4                 NA
#>   position_in_on_vehicle ejection_code age sex location_of_most_severe_injury
#> 1                      1             1  26   F                             NA
#> 2                      1             1  25   F                             NA
#> 3                      1             1  74   M                             NA
#> 4                      3             1  71   F                             NA
#> 5                      6             1  44   M                             NA
#> 6                      1             1  74   M                             NA
#>   type_of_most_severe_phys_injury refused_medical_attention
#> 1                              NA                        NA
#> 2                              NA                        NA
#> 3                              NA                        NA
#> 4                              NA                        NA
#> 5                              NA                        NA
#> 6                              NA                        NA
#>   safety_equipment_available safety_equipment_used airbag_deployment
#> 1                         11                     4                NA
#> 2                         11                     4                NA
#> 3                         11                     4                NA
#> 4                         11                     4                NA
#> 5                         11                     4                NA
#> 6                         11                     4                NA

# Get pedestrian data for the year 2020
ped_20 <- get_njtr1(year = 2020, type = "Pedestrians")
#> njtr1: downloading data

head(ped_20)
#>                      id pedestrian_number physical_condition  address_city
#> 1 20200101I-2020-005330                 1                  2  PHILADELPHIA
#> 2 20200101I-2020-011440                 1                  4        CAMDEN
#> 3     202001022001-0032                 1                  4 ATLANTIC CITY
#> 4     202001022001-0062                31                 NA ATLANTIC CITY
#> 5     202001022001-0233                 1                  3 ATLANTIC CITY
#> 6     202001022001-0263                 1                  4      MACUNGIE
#>   address_state address_zip date_of_birth age sex alcohol_test_given
#> 1            PA       19121    1946-10-14  73   M                   
#> 2            NJ        8030    1980-07-03  39   M                   
#> 3            NJ        8401    1987-05-02  32   M                  N
#> 4            NJ        8401    2006-02-07  13   M                  N
#> 5            NJ        8401    2007-08-09  12                      N
#> 6            PA       18062    1978-12-04  41   M                  N
#>   alcohol_test_type alcohol_test_results charge_1 summons_1 charge_2 summons_2
#> 1                                     NA                                      
#> 2                                     NA                                      
#> 3                                     NA                                      
#> 4                                     NA                                      
#> 5                                     NA                                      
#> 6                                     NA                                      
#>   charge_3 summons_3 charge_4 summons_4 multi_charge_flag traffic_controls
#> 1                                                      NA                4
#> 2                                                      NA                4
#> 3                                                      NA                4
#> 4                                                      NA                3
#> 5                                                      NA                4
#> 6                                                      NA               11
#>   contributing_circumstances_1 contributing_circumstances_2 direction_of_travel
#> 1                           73                           NA                  NA
#> 2                           25                           NA                  NA
#> 3                           72                           NA                   3
#> 4                           29                           NA                   0
#> 5                           89                           NA                   4
#> 6                           85                           NA                   2
#>   precrash_action location_of_most_severe_injury
#> 1              33                              1
#> 2              34                              7
#> 3              42                              6
#> 4              29                             NA
#> 5              44                              7
#> 6              33                              7
#>   type_of_most_severe_phys_injury refused_medical_attention
#> 1                               2                         2
#> 2                               8                         2
#> 3                               8                         2
#> 4                              NA                        NA
#> 5                               7                         2
#> 6                               8                         1
#>   safety_equipment_used hospital_code physical_status_1 physical_status_2
#> 1                    NA          5101                 1                NA
#> 2                    NA          5101                 1                NA
#> 3                    NA          5101                 1                NA
#> 4                     1            NA                 0                NA
#> 5                    NA          5101                 0                NA
#> 6                    NA            NA                 1                NA
#>   is_bycyclist is_other
#> 1                      
#> 2                      
#> 3                      
#> 4            Y         
#> 5                      
#> 6
```

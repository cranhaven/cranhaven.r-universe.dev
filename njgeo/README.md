
<!-- README.md is generated from README.Rmd. Please edit that file -->

# njgeo <a href='https://gavinrozzi.github.io/njgeo/'><img src='man/figures/logo.png' align="right" height="175" width="150" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/gavinrozzi/njgeo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gavinrozzi/njgeo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Tools for geocoding addresses using the State of New Jersey’s official
geocoding service & accessing spatial data.

## Installation

You can install the development version of njgeo from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gavinrozzi/njgeo")
```

## Usage

### Geocoding

This package supports freely geocoding addresses in New Jersey. No API
keys are required and this does not depend on any commercial services.

## Find all matching address candidates for an address

``` r
geocode_address_candidates("33 Livingston Ave. New Brunswick, NJ")
#> njgeo: downloading data
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -74.47533 ymin: 40.46493 xmax: -74.44513 ymax: 40.49297
#> Geodetic CRS:  WGS 84
#>                                          address  score location.x location.y
#> 1 33 Livingston Avenue, New Brunswick, NJ, 08901 100.00  -74.44513   40.49297
#> 2    Livingston Avenue, New Brunswick, NJ, 08901  97.59  -74.45771   40.48024
#> 3  Livingston Avenue, North Brunswick, NJ, 08902  95.86  -74.47533   40.46493
#> 4   Livingston Avenue, East Brunswick, NJ, 08816  95.86  -74.44593   40.49154
#>   extent.xmin extent.ymin extent.xmax extent.ymax                   geometry
#> 1   -74.44613    40.49197   -74.44413    40.49397 POINT (-74.44513 40.49297)
#> 2   -74.45871    40.47924   -74.45671    40.48124 POINT (-74.45771 40.48024)
#> 3   -74.47633    40.46393   -74.47433    40.46594 POINT (-74.47533 40.46493)
#> 4   -74.44693    40.49054   -74.44493    40.49254 POINT (-74.44593 40.49154)
```

The geocoding output defaults to EPSG:4326 but another CRS can be
specified via arguments:

``` r
geocode_address_candidates("33 Livingston Ave. New Brunswick, NJ", crs = 3424)
#> njgeo: downloading data
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 498988.5 ymin: 594272.3 xmax: 507385.6 ymax: 604489.2
#> Projected CRS: NAD83 / New Jersey (ftUS)
#>                                          address  score location.x location.y
#> 1 33 Livingston Avenue, New Brunswick, NJ, 08901 100.00   507385.6   604489.2
#> 2    Livingston Avenue, New Brunswick, NJ, 08901  97.59   503889.1   599850.6
#> 3  Livingston Avenue, North Brunswick, NJ, 08902  95.86   498988.5   594272.3
#> 4   Livingston Avenue, East Brunswick, NJ, 08816  95.86   507163.8   603966.6
#>   extent.xmin extent.ymin extent.xmax extent.ymax                  geometry
#> 1    507107.3    604124.7    507664.0    604853.6 POINT (507385.6 604489.2)
#> 2    503610.8    599486.2    504167.5    600215.1 POINT (503889.1 599850.6)
#> 3    498710.2    593908.0    499266.8    594636.7 POINT (498988.5 594272.3)
#> 4    506885.4    603602.2    507442.1    604331.1 POINT (507163.8 603966.6)
```

### Batch Geocoding

It is possible to batch geocode up to 1000 addresses at once using the
two batch geocoding functions provided by the package.

The `batch_geocode_addresses()` and `batch_geocode_sl()` functions can
batch geocode up to 1000 addresses at a time. The first function expects
multiple columns of data to geocode the address, while the **sl**
version requires an address in single column format.

### Reverse Geocoding

Provide a point to get matching addresses:

``` r
reverse_geocode(-74.44513, 40.49297)
#> njgeo: downloading data
#>                Address Neighborhood          City Subregion Region Postal
#> 1 33 Livingston Avenue              New Brunswick Middlesex     NJ  08901
#>   PostalExt CountryCode                                     Match_addr
#> 1      1900             33 Livingston Avenue, New Brunswick, NJ, 08901
#>         Loc_name
#> 1 NJ_Geocode_Mul
```

## Shape and boundary files

You can easily obtain spatial boundary data for use in projects via this
package. All objects are returned as an `{sf}` object and a coordinate
reference system can be specified via arguments to repoject the shape
into a different CRS.

### State

``` r
get_state_bounds()
#> njgeo: downloading data
#> Simple feature collection with 1 feature and 9 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -75.56342 ymin: 38.85289 xmax: -73.89363 ymax: 41.35765
#> Geodetic CRS:  WGS 84
#>   OBJECTID       NAME           GNIS_NAME    GNIS   ACRES SQ_MILES
#> 1        1 New Jersey State of New Jersey 1779795 5549497 8671.089
#>                                 GLOBALID SHAPE_Length   SHAPE_Area
#> 1 {64BFC6D2-D0A8-418C-9E76-ADF18AA40F74}      2703088 241735115122
#>                         geometry
#> 1 POLYGON ((-74.67081 41.3463...
```

### Counties

``` r
get_county_bounds()
#> njgeo: downloading data
#> Simple feature collection with 21 features and 21 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -75.55957 ymin: 38.92852 xmax: -73.90245 ymax: 41.35765
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    FID     COUNTY      COUNTY_LABEL  CO            GNIS_NAME   GNIS FIPSSTCO
#> 1    1   ATLANTIC   Atlantic County ATL   County of Atlantic 882270    34001
#> 2    2     BERGEN     Bergen County BER     County of Bergen 882271    34003
#> 3    3 BURLINGTON Burlington County BUR County of Burlington 882272    34005
#> 4    4     CAMDEN     Camden County CAM     County of Camden 882273    34007
#> 5    5   CAPE MAY   Cape May County CAP   County of Cape May 882274    34009
#> 6    6 CUMBERLAND Cumberland County CUM County of Cumberland 882275    34011
#> 7    7      ESSEX      Essex County ESS      County of Essex 882276    34013
#> 8    8 GLOUCESTER Gloucester County GLO County of Gloucester 882277    34015
#> 9    9     HUDSON     Hudson County HUD     County of Hudson 882278    34017
#> 10  10  HUNTERDON  Hunterdon County HUN  County of Hunterdon 882228    34019
#>    FIPSCO     ACRES SQ_MILES POP2010 POP2000 POP1990 POP1980 POPDEN2010
#> 1       1 390815.40 610.6491  274549  252552  275372  204615        450
#> 2       3 153490.28 239.8286  905116  884118  829592  849843       3774
#> 3       5 524903.34 820.1615  448734  423394  395066  362542        547
#> 4       7 145598.49 227.4976  513657  508932  532498  471650       2258
#> 5       9 183126.57 286.1353   97265  102326   95089   82266        340
#> 6      11 321150.32 501.7974  156898  146438  138053  132866        313
#> 7      13  83034.86 129.7420  783969  793633  748281  850451       6043
#> 8      15 215073.16 336.0518  288288  254673  230082  199917        858
#> 9      17  32982.40  51.5350  634266  608975  553099  556972      12307
#> 10     19 279879.34 437.3115  128349  121989  107776   87361        293
#>    POPDEN2000 POPDEN1990 POPDEN1980       REGION SHAPE_Length SHAPE_Area
#> 1         414        451        335      COASTAL     2.054478 0.16559498
#> 2        3686       3459       3544 NORTHEASTERN     1.393879 0.06645191
#> 3         516        482        442     SOUTHERN     2.439422 0.22368243
#> 4        2237       2341       2073     SOUTHERN     1.553964 0.06197876
#> 5         358        332        288      COASTAL     1.589942 0.07723522
#> 6         292        275        265     SOUTHERN     2.213655 0.13586761
#> 7        6117       5767       6555 NORTHEASTERN     1.105311 0.03585682
#> 8         758        685        595     SOUTHERN     1.804883 0.09143512
#> 9       11817      10732      10808 NORTHEASTERN     1.225024 0.01423219
#> 10        279        246        200      CENTRAL     1.783708 0.12046630
#>                          geometry
#> 1  MULTIPOLYGON (((-74.67437 3...
#> 2  MULTIPOLYGON (((-73.90569 4...
#> 3  MULTIPOLYGON (((-74.69864 4...
#> 4  MULTIPOLYGON (((-75.03314 3...
#> 5  MULTIPOLYGON (((-74.85962 3...
#> 6  MULTIPOLYGON (((-75.06186 3...
#> 7  MULTIPOLYGON (((-74.32256 4...
#> 8  MULTIPOLYGON (((-75.12857 3...
#> 9  MULTIPOLYGON (((-74.16093 4...
#> 10 MULTIPOLYGON (((-74.86234 4...
```

### Municipalities

``` r
get_muni_bounds()
#> njgeo: downloading data
#> Simple feature collection with 565 features and 23 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -75.55957 ymin: 38.92852 xmax: -73.90245 ymax: 41.35765
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    OBJECTID                 MUN   COUNTY              MUN_LABEL MUN_TYPE
#> 1         1 CAPE MAY POINT BORO CAPE MAY Cape May Point Borough  Borough
#> 2         2  WEST CAPE MAY BORO CAPE MAY  West Cape May Borough  Borough
#> 3         3       CAPE MAY CITY CAPE MAY          Cape May City     City
#> 4         4 WILDWOOD CREST BORO CAPE MAY Wildwood Crest Borough  Borough
#> 5         5  WEST WILDWOOD BORO CAPE MAY  West Wildwood Borough  Borough
#> 6         6 NORTH WILDWOOD CITY CAPE MAY    North Wildwood City     City
#> 7         7           LOWER TWP CAPE MAY         Lower Township Township
#> 8         8   STONE HARBOR BORO CAPE MAY   Stone Harbor Borough  Borough
#> 9         9         AVALON BORO CAPE MAY         Avalon Borough  Borough
#> 10       10          MIDDLE TWP CAPE MAY        Middle Township Township
#>                      NAME                 GNIS_NAME   GNIS  SSN MUN_CODE
#> 1  Cape May Point Borough Borough of Cape May Point 885179 0503     0503
#> 2   West Cape May Borough  Borough of West Cape May 885435 0512     0512
#> 3                Cape May          City of Cape May 885178 0502     0502
#> 4  Wildwood Crest Borough Borough of Wildwood Crest 885445 0515     0515
#> 5   West Wildwood Borough  Borough of West Wildwood 885441 0513     0513
#> 6          North Wildwood    City of North Wildwood 885328 0507     0507
#> 7          Lower Township         Township of Lower 882044 0505     0505
#> 8    Stone Harbor Borough   Borough of Stone Harbor 885410 0510     0510
#> 9          Avalon Borough         Borough of Avalon 885146 0501     0501
#> 10        Middle Township        Township of Middle 882045 0506     0506
#>    CENSUS2010      ACRES   SQ_MILES POP2010 POP2000 POP1990 POP1980 POPDEN2010
#> 1  3400910330   192.0512  0.3000799     291     241     248     255        970
#> 2  3400978530   756.5388  1.1820919    1024    1095    1026    1091        866
#> 3  3400910270  1844.8312  2.8825488    3607    4034    4668    4853       1251
#> 4  3400981200   947.7268  1.4808231    3270    3980    3631    4149       2208
#> 5  3400980210   232.8413  0.3638145     603     448     453     360       1657
#> 6  3400953490  1593.6241  2.4900376    4041    4935    5017    4714       1623
#> 7  3400941610 19851.6960 31.0182749   22866   22945   20820   17105        737
#> 8  3400971010  1479.9542  2.3124285     866    1128    1025    1187        374
#> 9  3400902320  3179.4468  4.9678857    1334    2143    1809    2162        269
#> 10 3400945810 52934.9963 82.7109317   18911   16405   14771   11373        229
#>    POPDEN2000 POPDEN1990 POPDEN1980 SHAPE_Length   SHAPE_Area
#> 1         803        826        850   0.04154698 8.075899e-05
#> 2         926        868        923   0.08769263 3.181544e-04
#> 3        1399       1619       1684   0.20318471 7.758056e-04
#> 4        2688       2452       2802   0.10132480 3.987271e-04
#> 5        1231       1245        990   0.05201539 9.800047e-05
#> 6        1982       2015       1893   0.14039854 6.707966e-04
#> 7         740        671        551   0.52199887 8.352893e-03
#> 8         488        443        513   0.13930193 6.232645e-04
#> 9         431        364        435   0.22707767 1.339875e-03
#> 10        198        179        138   0.81069461 2.230602e-02
#>                          geometry
#> 1  MULTIPOLYGON (((-74.95983 3...
#> 2  MULTIPOLYGON (((-74.92585 3...
#> 3  MULTIPOLYGON (((-74.8765 38...
#> 4  MULTIPOLYGON (((-74.83331 3...
#> 5  MULTIPOLYGON (((-74.8189 39...
#> 6  MULTIPOLYGON (((-74.7797 39...
#> 7  MULTIPOLYGON (((-74.934 39....
#> 8  MULTIPOLYGON (((-74.75414 3...
#> 9  MULTIPOLYGON (((-74.7138 39...
#> 10 MULTIPOLYGON (((-74.7174 39...
```

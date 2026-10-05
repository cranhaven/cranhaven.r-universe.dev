
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bcRP

<!-- badges: start -->

<!-- badges: end -->

Grab the latest data from the Peruvian Central Bank using R.

## Installation

You can install the stable version of `bcRP` from CRAN with:

``` r
install.packages("bcRP")
```

You can install the development version of `bcRP` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("JulioCollazos64/bcRP")
```

## Example

Search data about inflation:

``` r
library(bcRP)
metadata_tbl <- get_bcrp_metadata()
metadata_tbl
#> # A tibble: 16,023 × 19
#>    `Código de serie` `Categoría de serie`     `Grupo de serie` `Nombre de serie`
#>    <chr>             <chr>                    <chr>            <chr>            
#>  1 PN00001MM         Sociedades creadoras de… Cuentas monetar… Activos Externos…
#>  2 PN00002MM         Sociedades creadoras de… Cuentas monetar… Activos Externos…
#>  3 PN00003MM         Sociedades creadoras de… Cuentas monetar… Activos Externos…
#>  4 PN00004MM         Sociedades creadoras de… Cuentas monetar… Activos Externos…
#>  5 PN00005MM         Sociedades creadoras de… Cuentas monetar… Activos Externos…
#>  6 PN00006MM         Sociedades creadoras de… Cuentas monetar… Activos Externos…
#>  7 PN00007MM         Sociedades creadoras de… Cuentas monetar… Activos Internos…
#>  8 PN00008MM         Sociedades creadoras de… Cuentas monetar… Activos Internos…
#>  9 PN00009MM         Sociedades creadoras de… Cuentas monetar… Activos Internos…
#> 10 PN00010MM         Sociedades creadoras de… Cuentas monetar… Activos Internos…
#> # ℹ 16,013 more rows
#> # ℹ 15 more variables: `Descripción de la serie` <chr>,
#> #   `Metodología de la serie` <chr>, `Unidad de medida` <lgl>, Escala <lgl>,
#> #   `Geografía (País, Región de Perú)` <chr>, Fuente <chr>, Frecuencia <chr>,
#> #   `Fecha de creación` <date>, `Grupo de publicación` <chr>,
#> #   `Área que publica` <chr>, `Fecha de actualización` <date>,
#> #   `Fecha de inicio` <chr>, `Fecha de fin` <chr>, Memo <lgl>, ...19 <lgl>
subset(metadata_tbl, grepl("Inflación", `Nombre de serie`))
#> # A tibble: 21 × 19
#>    `Código de serie` `Categoría de serie` `Grupo de serie`     `Nombre de serie`
#>    <chr>             <chr>                <chr>                <chr>            
#>  1 PN01250PM         Tipo de cambio real  Inflación de socios… Inflación USA    
#>  2 PN01258PM         Tipo de cambio real  Inflación de socios… Inflación Multil…
#>  3 PN01296PM         Inflación            Índice de precios a… Inflación Subyac…
#>  4 PN01297PM         Inflación            Índice de precios a… Inflación Subyac…
#>  5 PN01298PM         Inflación            Índice de precios a… Inflación Subyac…
#>  6 PN01299PM         Inflación            Índice de precios a… Inflación Subyac…
#>  7 PN01300PM         Inflación            Índice de precios a… Inflación Subyac…
#>  8 PN01301PM         Inflación            Índice de precios a… Inflación Subyac…
#>  9 PN01302PM         Inflación            Índice de precios a… Inflación Subyac…
#> 10 PN01303PM         Inflación            Índice de precios a… Inflación Subyac…
#> # ℹ 11 more rows
#> # ℹ 15 more variables: `Descripción de la serie` <chr>,
#> #   `Metodología de la serie` <chr>, `Unidad de medida` <lgl>, Escala <lgl>,
#> #   `Geografía (País, Región de Perú)` <chr>, Fuente <chr>, Frecuencia <chr>,
#> #   `Fecha de creación` <date>, `Grupo de publicación` <chr>,
#> #   `Área que publica` <chr>, `Fecha de actualización` <date>,
#> #   `Fecha de inicio` <chr>, `Fecha de fin` <chr>, Memo <lgl>, ...19 <lgl>
```

Let’s look at some statistics:

``` r
inflation_data <- get_bcrp_data(
  codes = "PN01296PM",
  from = "2010-01",
  to = "2025-01"
)
inflation_data$values <- as.numeric(inflation_data$values)
inflation_data
#> # A tibble: 181 × 4
#>    name      values series_name                                            code 
#>    <chr>      <dbl> <chr>                                                  <chr>
#>  1 Ene.2010  0.270  Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  2 Feb.2010  0.226  Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  3 Mar.2010  0.153  Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  4 Abr.2010  0.0960 Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  5 May.2010  0.0480 Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  6 Jun.2010  0.126  Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  7 Jul.2010  0.0322 Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  8 Ago.2010  0.242  Índice de precios al consumidor Lima Metropolitana: c… PN01…
#>  9 Sep.2010 -0.0545 Índice de precios al consumidor Lima Metropolitana: c… PN01…
#> 10 Oct.2010  0.117  Índice de precios al consumidor Lima Metropolitana: c… PN01…
#> # ℹ 171 more rows
summary(inflation_data)
#>      name               values        series_name            code          
#>  Length:181         Min.   :-0.1384   Length:181         Length:181        
#>  Class :character   1st Qu.: 0.1355   Class :character   Class :character  
#>  Mode  :character   Median : 0.2076   Mode  :character   Mode  :character  
#>                     Mean   : 0.2427                                        
#>                     3rd Qu.: 0.3015                                        
#>                     Max.   : 0.7494
```

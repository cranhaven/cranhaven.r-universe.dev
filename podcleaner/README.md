
<!-- README.md is generated from README.Rmd. Please edit that file -->

# podcleaner

<!-- badges: start -->

<!-- badges: end -->

The Scottish Post Office directories are annual directories that provide
an alphabetical list of a town’s or county’s inhabitants including their
forename, surname, occupation and address(es); they provide a solid
basis for researching Scotland’s family, trade, and town history. A
large number of these, covering most of Scotland and dating from 1773 to
1911, can be accessed in digitised form from the [National Library of
Scotland](https://digital.nls.uk/directories/). `podcleaner` attempts to
clean optical character recognition (OCR) errors in directory records
after they’ve been parsed and saved to “csv” files using a third party
tool\[1\]. The package further attempts to match records from trades and
general directories. See the tests folder for examples running
unexported functions.

## Load

Load general and trades directory samples in memory from “csv” files:

  - **Globals**

<!-- end list -->

``` r
library(podcleaner)

directories <- c("1861-1862")

progress <- TRUE; verbose <- FALSE
```

  - **General directories**

<!-- end list -->

``` r
path_directories <- utils_make_path("data", "general-directories")

general_directory <- utils_load_directories_csv(
  type = "general", directories, path_directories, verbose
)

print.data.frame(general_directory)
#>   directory page    surname forename
#> 1 1861-1862   71       ABOT      Wm.
#> 2 1861-1862   71 ABRCROMBIE     Alex
#>                                                occupation
#> 1 Wine and spirit mercht — See Advertisement in Appendix.
#> 2                                                        
#>                                                    addresses
#> 1                           1S20 Londn rd; ho. 13<J Queun sq
#> 2 Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st.
```

  - **Trades directories**

<!-- end list -->

``` r
path_directories <- utils_make_path("data", "trades-directories")

trades_directory <- utils_load_directories_csv(
  type = "trades", directories, path_directories, verbose
)

print.data.frame(trades_directory)
#>   directory page rank                                              occupation
#> 1 1861-1862   71  135 Wine and spirit mercht — See Advertisement in Appendix.
#> 2 1861-1862   71  326                                                     Bkr
#> 3 1861-1862   71  586                                               Victualer
#>          type    surname forename address.trade.body address.trade.number
#> 1 OWN ACCOUNT       ABOT      Wm.          Londn rd.                 1S20
#> 2 OWN ACCOUNT ABRCROMBIE     Alex           Dixen pl                   I2
#> 3 OWN ACCOUNT       BLAI  Jon Hug           High St.                  2S0
```

## Clean

Clean records from both datasets:

  - **General directories**

<!-- end list -->

``` r
general_directory <-
  general_clean_directory(general_directory, progress, verbose)

print.data.frame(general_directory)
#>   directory page    surname  forename               occupation
#> 1 1861-1862   71     Abbott   William Wine and spirit merchant
#> 2 1861-1862   71 Abercromby Alexander                    Baker
#> 3 1861-1862   71 Abercromby Alexander                    Baker
#>   address.trade.number address.trade.body address.house.number
#> 1               18, 20       London Road.                  136
#> 2                   12      Dixon Street.                  265
#> 3                   29    Anderston Quay.                  265
#>   address.house.body
#> 1      Queen Square.
#> 2     Argyle Street.
#> 3     Argyle Street.
```

  - **Trades directories**

<!-- end list -->

``` r
trades_directory <-
  trades_clean_directory(trades_directory, progress, verbose)

print.data.frame(trades_directory)
#>   directory page rank    surname  forename               occupation        type
#> 1 1861-1862   71  135     Abbott   William Wine and spirit merchant OWN ACCOUNT
#> 2 1861-1862   71  326 Abercromby Alexander                    Baker OWN ACCOUNT
#> 3 1861-1862   71  586      Blair John Hugh               Victualler OWN ACCOUNT
#>   address.trade.number address.trade.body
#> 1               18, 20       London Road.
#> 2                   12       Dixon Place.
#> 3                  280       High Street.
```

## Match

Match general to trades directory records:

``` r
distance <- TRUE; matches <- TRUE

directory <- combine_match_general_to_trades(
  trades_directory, general_directory, progress, verbose, distance, matches,
  method = "osa", max_dist = 5L
)

print.data.frame(directory)
#>   directory page rank    surname  forename               occupation        type
#> 1 1861-1862   71  135     Abbott   William Wine and spirit merchant OWN ACCOUNT
#> 2 1861-1862   71  326 Abercromby Alexander                    Baker OWN ACCOUNT
#> 3 1861-1862   71  586      Blair John Hugh               Victualler OWN ACCOUNT
#>   address.trade.number address.trade.body address.house.number
#> 1               18, 20       London Road.                  136
#> 2                   12       Dixon Place.                  265
#> 3                  280       High Street.                     
#>                       address.house.body distance
#> 1                          Queen Square.        0
#> 2                         Argyle Street.        5
#> 3 Failed to match with general directory       NA
#>                                     match
#> 1    Abbott William - 18, 20, London Road
#> 2 Abercromby Alexander - 12, Dixon Street
#> 3                                    <NA>
```

Directory records are compared and eventually matched using a distance
metric calculated with the method and corresponding parameters specified
in arguments. Under the hood the
[fuzzyjoin](https://www.rdocumentation.org/packages/fuzzyjoin/versions/0.1.6)
package and the
[stringdist\_left\_join](https://www.rdocumentation.org/packages/fuzzyjoin/versions/0.1.6/topics/stringdist_join)
function in particular, help with the matching operations.

## Save

``` r
utils_IO_write(directory, "dev", "post-office-directory")
```

1.  See for example the python
    [podparser](https://pythonhosted.org/podparser/) library.

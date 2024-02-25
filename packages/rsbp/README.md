<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- NOTE: knit using rmarkdown::render("README.Rmd", "md_document")-->

rsbp
====

<!-- badges: start -->
<!-- badges: end -->

Provides an R interface to the Registry of Standard Biological Parts API
maintained by the [iGEM Foundation](https://igem.org/Main_Page).
Facilitates retrieval of the part number, authorship, date of entry,
url, short description, type, and sequence following the [Registry
guidelines](http://parts.igem.org/Registry_API/Guidelines). All Registry
content falls under [Creative Commons
Attribution-ShareAlike](https://creativecommons.org/licenses/by-sa/4.0/).

Users can enter the name of a part and will receive a tibble containing
information about that part. Multiple parts can be accessed
simultaneously using other R functions to apply getPart() over a vector
containing the part names. Examples for single-part and multiple-part
retrieval are provided below. The provided multi-part retrieval example
requires the use of the map\_df function from the purrr library.

Installation
------------

Install the released version of rsbp from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("rsbp")
    #> Error in install.packages : Updating loaded packages

Loading
-------

Load the package with:

    library(rsbp)

Example
-------

For retrieval of information for single parts:

    #retrieve and store information for a single part
    result<-getPart("BBa_R0040")

    #examine info
    head(result)
    #> # A tibble: 1 x 10
    #>      id name    shortName seq                     type    results url             entered    desc         author               
    #>   <dbl> <chr>   <chr>     <chr>                   <chr>   <chr>   <chr>           <date>     <chr>        <chr>                
    #> 1   187 BBa_R0~ R0040     tccctatcagtgatagagattg~ Regula~ Works   http://parts.i~ 2003-01-31 TetR repres~ " June Rhee, Connie ~

For retrieval of information for multiple parts:

    #load purrr to get map_df()
    library(purrr)

    #define vector of part names
    parts<-c("BBa_B0034","BBa_B0035","BBa_B0036")

    #apply function to each input with map_df
    result<-map_df(parts, getPart)

    #examine info
    head(result)
    #> # A tibble: 3 x 10
    #>      id name    shortName seq      type  results url           entered    desc             author                              
    #>   <dbl> <chr>   <chr>     <chr>    <chr> <chr>   <chr>         <date>     <chr>            <chr>                               
    #> 1   151 BBa_B0~ B0034     aaagagg~ RBS   Works   http://parts~ 2003-01-31 RBS (Elowitz 19~ "Vinay S Mahajan, Voichita D. Marin~
    #> 2  3978 BBa_B0~ B0035     attaaag~ RBS   Works   http://parts~ 2004-01-27 RBS (B0030 deri~ "Jason Kelly"                       
    #> 3  4699 BBa_B0~ B0036     gtgtg    RBS   None    http://parts~ 2004-06-08 Specialized RBS  "Barry Canton"


# MIMER

<!-- badges: start -->

[![R-CMD-check](https://github.com/CAMO-NET-LIV/MIMER/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CAMO-NET-LIV/MIMER/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

MIMER is an R package designed for analyzing the MIMIC-IV dataset, a
repository of pseudonymized electronic health records. It offers a suite
of data wrangling functions tailored specifically for preparing the
dataset for research purposes, particularly in antimicrobial resistance
(AMR) studies. MIMER simplifies complex data manipulation tasks,
allowing researchers to focus on their primary inquiries without being
bogged down by wrangling complexities. It integrates seamlessly with the
AMR package and is ideal for R developers working in AMR research

## Usages

    MIMER::ndc_to_antimicrobial(ndc, class)   

    MIMER::ndc_is_antimicrobial(ndc, class)  

    MIMER::is_systemic_route(route, class)  

    MIMER::check_previous_events(df, cols, sort_by_col, patient_id_col,
                                    event_indi_value="R", new_col_prefix="pr_event_", 
                                    time_period_in_days = 0, minimum_prev_events = 0)
      

    MIMER::transpose_microbioevents(df, key_columns, required_columns, transpose_key_column,
                                          transpose_value_column, fill = "N/A")  
                                          
    #not recommended to use                                      
    MIMER::clean_antibiotics(
      x ,
     ... 
      )

## Installation

You can install the development version of MIMER from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("CAMO-NET-LIV/MIMER")
```

or install from CRAN using:

``` r
install.packages("MIMER")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(MIMER)
```

    ## Warning: package 'MIMER' was built under R version 4.3.3

``` r
## basic example code
MIMER::ndc_to_antimicrobial(ndc='65649030303', class='antibacterial')
```

    ## Class 'ab'
    ## [1] RFX

``` r
library(MIMER)
## basic example code
MIMER::ndc_is_antimicrobial(ndc='65649030303')
```

    ## [1] TRUE

``` r
library(MIMER)
## basic example code
MIMER::is_systemic_route(route='PO/NG')
```

    ## [1] TRUE

``` r
library(MIMER)
## basic example code
df <- data.frame(subject_id=c('90916742','90916742','90916742','90916742',
                              '90916742','90938332','90938332','90938332',
                              '90938332','90938332','90938332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-08-01',
                                       '2178-08-01','2178-09-25','2164-07-31',
                                       '2164-12-22','2164-12-22','2165-01-07',
                                       '2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','R','R','R','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

    MIMER::check_previous_events(df,
                                 cols = c('CEFTAZIDIME'), 
                                 sort_by_col = 'chartdate',
                                 patient_id_col = 'subject_id',
                                 event_indi_value='R')
```

    ## Checking Previous Events for

    ## CEFTAZIDIME

    ## Total Antibiotics Column (Events) Added :  1

    ## # A tibble: 11 × 5
    ##    subject_id chartdate  CEFEPIME CEFTAZIDIME pr_event_CEFTAZIDIME
    ##    <chr>      <chr>      <chr>    <chr>       <lgl>               
    ##  1 90938332   2164-07-31 R        S           FALSE               
    ##  2 90938332   2164-12-22 R        S           FALSE               
    ##  3 90938332   2164-12-22 R        S           FALSE               
    ##  4 90938332   2165-01-07 S        R           FALSE               
    ##  5 90938332   2165-04-17 S        R           TRUE                
    ##  6 90938332   2165-05-05 S        S           TRUE                
    ##  7 90916742   2178-07-03 R        S           FALSE               
    ##  8 90916742   2178-08-01 R        R           FALSE               
    ##  9 90916742   2178-08-01 R        S           FALSE               
    ## 10 90916742   2178-08-01 R        R           FALSE               
    ## 11 90916742   2178-09-25 S        R           TRUE

``` r
## example with 'minimum_prev_events' parameter
 df <- data.frame(subject_id=c('90916742','90916742','90916742','90916742',
                               '90916742','90938332','90938332','90938332',
                               '90938332','90938332','90938332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-07-22',
                                       '2178-08-03','2178-09-25','2164-07-31',
                                       '2164-12-22','2164-12-22','2165-01-07',
                                       '2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','S','R','S','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

  MIMER::check_previous_events(df, 
                               cols = c('CEFEPIME'), 
                               sort_by_col = 'chartdate', 
                               patient_id_col = 'subject_id', 
                               minimum_prev_events = 2)
```

    ## Checking Previous Events for

    ## CEFEPIME

    ## Total Antibiotics Column (Events) Added :  1

    ## # A tibble: 11 × 5
    ##    subject_id chartdate  CEFEPIME CEFTAZIDIME pr_event_CEFEPIME
    ##    <chr>      <chr>      <chr>    <chr>       <lgl>            
    ##  1 90938332   2164-07-31 R        S           FALSE            
    ##  2 90938332   2164-12-22 R        S           FALSE            
    ##  3 90938332   2164-12-22 R        S           FALSE            
    ##  4 90938332   2165-01-07 S        R           TRUE             
    ##  5 90938332   2165-04-17 S        R           TRUE             
    ##  6 90938332   2165-05-05 S        S           TRUE             
    ##  7 90916742   2178-07-03 R        S           FALSE            
    ##  8 90916742   2178-07-22 R        S           FALSE            
    ##  9 90916742   2178-08-01 S        R           TRUE             
    ## 10 90916742   2178-08-03 S        R           TRUE             
    ## 11 90916742   2178-09-25 S        R           TRUE

``` r
## example with 'time_period_in_days' parameter
df <- data.frame(subject_id=c('90916742','90916742','90916742','90916742',
                              '90916742','90938332','90938332','90938332',
                              '90938332','90938332','90938332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-07-22',
                                       '2178-08-03','2178-09-25','2164-07-31',
                                       '2164-12-22','2164-12-22','2165-01-07',
                                       '2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','S','R','S','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))

MIMER::check_previous_events(df, 
                             cols = c('CEFTAZIDIME'), 
                             sort_by_col = 'chartdate',
                             patient_id_col = 'subject_id', 
                             time_period_in_days = 25)
```

    ## Checking Previous Events for

    ## CEFTAZIDIME

    ## Total Antibiotics Column (Events) Added :  1

    ## # A tibble: 11 × 5
    ##    subject_id chartdate  CEFEPIME CEFTAZIDIME pr_event_CEFTAZIDIME
    ##    <chr>      <chr>      <chr>    <chr>       <lgl>               
    ##  1 90938332   2164-07-31 R        S           FALSE               
    ##  2 90938332   2164-12-22 R        S           FALSE               
    ##  3 90938332   2164-12-22 R        S           FALSE               
    ##  4 90938332   2165-01-07 S        R           FALSE               
    ##  5 90938332   2165-04-17 S        R           FALSE               
    ##  6 90938332   2165-05-05 S        S           TRUE                
    ##  7 90916742   2178-07-03 R        S           FALSE               
    ##  8 90916742   2178-07-22 R        S           FALSE               
    ##  9 90916742   2178-08-01 S        R           FALSE               
    ## 10 90916742   2178-08-03 S        R           TRUE                
    ## 11 90916742   2178-09-25 S        R           FALSE

``` r
## example with 'time_period_in_days' & 'minimum_prev_events' parameters
df <- data.frame(subject_id=c('90916742','90916742','90916742','90916742',
                              '90916742','90938332','90938332',
                              '90938332','90938332','90938332','90938332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-08-01',
                                       '2178-08-01','2178-09-25','2164-07-31',
                                       '2164-12-22','2164-12-22','2165-01-07',
                                       '2165-04-17','2165-05-05'),
                          CEFEPIME=c('R','R','R','R','S','R','R','R','S','S','S'),
                          CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))
                          
MIMER::check_previous_events(df, 
                             cols = c('CEFEPIME'),
                             sort_by_col = 'chartdate',
                             patient_id_col = 'subject_id',
                             time_period_in_days = 62,
                             minimum_prev_events = 2)
```

    ## Checking Previous Events for

    ## CEFEPIME

    ## Total Antibiotics Column (Events) Added :  1

    ## # A tibble: 11 × 5
    ##    subject_id chartdate  CEFEPIME CEFTAZIDIME pr_event_CEFEPIME
    ##    <chr>      <chr>      <chr>    <chr>       <lgl>            
    ##  1 90938332   2164-07-31 R        S           FALSE            
    ##  2 90938332   2164-12-22 R        S           FALSE            
    ##  3 90938332   2164-12-22 R        S           FALSE            
    ##  4 90938332   2165-01-07 S        R           TRUE             
    ##  5 90938332   2165-04-17 S        R           FALSE            
    ##  6 90938332   2165-05-05 S        S           FALSE            
    ##  7 90916742   2178-07-03 R        S           FALSE            
    ##  8 90916742   2178-08-01 R        R           FALSE            
    ##  9 90916742   2178-08-01 R        S           FALSE            
    ## 10 90916742   2178-08-01 R        R           FALSE            
    ## 11 90916742   2178-09-25 S        R           TRUE

``` r
##example for transpose_microbioevents
test_data <- data.frame(subject_id=c('90916742','90916742','90916742','90916742',
                                     '90916742','90938332','90938332','90938332',
                                     '90938332','90938332','90938332'),
                          chartdate= c('2178-07-03','2178-08-01','2178-08-01',
                                       '2178-08-01','2178-09-25','2164-07-31',
                                       '2164-12-22','2164-12-22','2165-01-07',
                                       '2165-04-17','2165-05-05'),
                          ab_name=c('CEFEPIME','CEFTAZIDIME','CEFEPIME',
                                    'CEFEPIME','CEFTAZIDIME','CEFTAZIDIME',
                                    'CEFEPIME','CEFEPIME','CEFTAZIDIME',
                                    'CEFTAZIDIME','CEFEPIME'),
                          interpretation=c('S','R','S','R','R','S','S','S','R','R','S'))

MIMER::transpose_microbioevents(test_data,
                                key_columns = c('subject_id','chartdate','ab_name') , 
                                required_columns =c('subject_id','chartdate'), 
                                transpose_key_column = 'ab_name',
                                transpose_value_column = 'interpretation', 
                                fill = "N/A",
                                non_empty_filter_column='subject_id')
```

    ##   subject_id  chartdate CEFEPIME CEFTAZIDIME
    ## 1   90916742 2178-07-03        S         N/A
    ## 2   90916742 2178-08-01      N/A           R
    ## 3   90916742 2178-09-25      N/A           R
    ## 4   90938332 2164-07-31      N/A           S
    ## 5   90938332 2165-01-07      N/A           R
    ## 6   90938332 2165-04-17      N/A           R
    ## 7   90938332 2165-05-05        S         N/A

``` r
library(MIMER)
## basic example code
MIMER::clean_antibiotics(c("Amoxicilli"))
```

    ## [1] "Amoxicillin"

``` r
library(MIMER)
## basic example code
df <- data.frame(drug = c("Amoxicilln","moxicillin","Paracetamol") )
MIMER::clean_antibiotics(df, drug_col = drug)
```

    ##          drug    abx_name    synonyms is_abx
    ## 1  Amoxicilln Amoxicillin Amoxicillin   TRUE
    ## 2  moxicillin Amoxicillin Amoxicillin   TRUE
    ## 3 Paracetamol        <NA>        <NA>  FALSE

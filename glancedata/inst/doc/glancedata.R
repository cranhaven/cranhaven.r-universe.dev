## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, message=FALSE---------------------------------------------

library(dplyr)

pkg_list <- list(

    ## skim
  c(
    pkg = "`skimr`",
    fnct = "`skim`",
    des = "Alternative to `summary`. Friendly with `dplyr::group_by()`."
  ),
  
  ## glance_data
  c(
    pkg = "`glancedata`",
    fnct = "`glance_data`",
    des = "Alternative to `summary`. Emphasizes missing data and binary variables."
  ),
  
  ## glance_data_in_workbook
  c(
    pkg = "`glancedata`",
    fnct = "`glance_data_in_workbook`",
    des = "Similar to `glance_data`. Creates list of dataframes instead and saves XLSX file."
  ),
  
  ## plot_numerical_vars
  c(
    pkg = "`glancedata`",
    fnct = "`plot_numerical_vars`",
    des = "Creates a plot per numerical variable. It might be histogram, density plot, qqplot, violin plot or scatterplot."
  )
,
  
  ## plot_discrete_vars
  c(
    pkg = "`glancedata`",
    fnct = "`plot_discrete_vars`",
    des = "Creates a plot per variable with up to 20 different values. This limit can be adjusted.."
  )
,
  
  ## ggpairs
  c(
    pkg = "`GGally`",
    fnct = "`ggpairs`",
    des = "Creates plots for pairwise comparison of columns."
  )
) ## End list


pkg_list %>%
  purrr::map_df(~ tibble(Package = .x[1], 
                         Function = .x[2], 
                         Description = .x[3])) %>%
  knitr::kable()
  

## ----setup, message=FALSE, cache=TRUE-----------------------------------------
library(dplyr)
library(tidyr)
library(knitr)

sample_data <- 
  tibble(State = state.name,
         Region = state.region) %>%
  bind_cols(as_tibble(state.x77)) %>%
  bind_cols(USArrests)

kable(head(sample_data))



## ---- eval = FALSE, dependson= "setup"----------------------------------------
#  ## Load package
#  library(skimr)
#  
#  ## Call main function
#  skim(sample_data)
#  

## ---- dependson = "setup", echo = FALSE, results='asis', message=FALSE--------

library(glancedata)
library(dplyr)
library(tidyr)
library(knitr)
library(skimr)

skim(sample_data)




## ---- dependson= "setup"------------------------------------------------------

library(glancedata)

glance_data(sample_data)


## -----------------------------------------------------------------------------
library(glancedata)

glance_data_in_workbook(sample_data)


## ---- eval = FALSE------------------------------------------------------------
#  
#  library(readr)
#  library(dplyr)
#  
#  ## Read data with dates from FiveThirtyEight
#  polls <-
#    read_csv("https://github.com/fivethirtyeight/data/tree/master/polls")
#  
#  


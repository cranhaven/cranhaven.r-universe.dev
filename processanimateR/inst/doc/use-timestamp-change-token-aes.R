## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = TRUE, message = FALSE---------------------------------------------
# Libraries ---------------------------------------------------------------
library(dplyr)            ##pipes
library(tidyr)            ##tidy data, partcularly the crossing() function
library(lubridate)        ##date time manipulation
library(bupaR)            ##buisness process analytics
library(processanimateR)  ##animates process

# Create performance time flags ------------------------------------------------
my_flags <- data.frame(value = c(0,2,4,8,16)) %>% 
            mutate(day = days(value)) #convert numeric value into days


## ----echo = TRUE, message = FALSE---------------------------------------------
# Create timestamps of flags ----------------------------------------------

my_timeflags <- patients %>% 
                cases %>%
                crossing(my_flags) %>% ##similar to a SQL outer join
                mutate(time = start_timestamp + day) %>% 
                filter(time <= complete_timestamp) %>% 
                select("case" = patient,time,value) ##must be case, time, value

## ----echo = TRUE, message = FALSE---------------------------------------------
# Animate process ---------------------------------------------------------

patients %>%
  animate_process(mode ="absolute",
                  jitter=10,
                  legend = "color", 
                  mapping = token_aes(
                    color = token_scale(my_timeflags
                                        , scale = "ordinal"
                                        , domain = my_flags$value
                                        , range = rev(RColorBrewer::brewer.pal(5,"Spectral"))
                    )))



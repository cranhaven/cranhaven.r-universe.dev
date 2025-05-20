## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plot_names, echo=FALSE, cache=FALSE--------------------------------------
df <- structure(list(X1 = "Maria", X2 = "Ben", X3 = "Claudia", X4 = "Adam", 
    X5 = "Hannah", X6 = "Robert"), class = "data.frame", row.names = "**Customers:**")
knitr::kable(df, col.names = NULL)

## ----package structure, echo=FALSE, cache=FALSE-------------------------------
df <- structure(list(API = c("https://genderize.io", "https://agify.io", "https://nationalize.io"),
                     `R function` = c("`genderize(name)`", "`agify(name)`", "`nationalize(name)`"),
                     `Estimated variable` = c("Gender", "Age", "Nationality")),
                class = "data.frame", row.names = c(NA, -3L))

knitr::kable(df, row.names = FALSE)

## ----result, echo=FALSE, cache=FALSE------------------------------------------
df <- structure(list(X1 = c("Maria", "female", "21", "CY"), X2 = c("Ben", 
"male", "48", "AU"), X3 = c("Claudia", "female", "45", "CL"), 
    X4 = c("Adam", "male", "34", "PL"), X5 = c("Hannah", "female", 
    "27", "SL"), X6 = c("Robert", "male", "59", "US")), row.names = c("**Customers:**", 
"**Estimated gender:**", "**Estimated age:**", "**Estimated nationality:**"
), class = "data.frame")

knitr::kable(df, col.names = NULL)

## ----setup, cache=FALSE-------------------------------------------------------
library("DemografixeR")

## ----save API function, eval=FALSE--------------------------------------------
#  save_api_key(key = "__YOUR_API_KEY__")

## ----genderize, eval=TRUE, echo=TRUE, cache=FALSE-----------------------------
customers_names <- c("Maria", "Ben", "Claudia", 
                     "Adam", "Hannah", "Robert")
customers_predicted_gender <- genderize(name = customers_names)
customers_predicted_gender # Print results


## ----genderize_class, cache=FALSE---------------------------------------------
class(customers_predicted_gender)


## ----genderize_dataframe_print, eval=FALSE------------------------------------
#  gender_df <- genderize(name = customers_names, simplify = FALSE)
#  customers_names %>%
#    genderize(simplify = FALSE) %>%
#    knitr::kable(row.names = FALSE)
#  

## ----genderize_dataframe_calculate, cache=FALSE, echo=FALSE-------------------
df <- structure(list(name = c("Maria", "Ben", "Claudia", "Adam", "Hannah", "Robert"),
                     type = c("gender", "gender", "gender", "gender", "gender", "gender"),
                     gender = c("female", "male", "female", "male", "female", "male"), 
                     probability = c(0.98, 0.95, 0.98, 0.98, 0.97, 0.99), 
                     count = c(334287L, 77991L, 118604L, 116396L, 13198L, 177418L)), 
                row.names = c(5L, 2L, 3L, 1L, 4L, 6L), class = "data.frame")

knitr::kable(df, row.names = FALSE)

## ----agify_print, cache=FALSE, eval=FALSE-------------------------------------
#  customers_predicted_age <- agify(name = customers_names, simplify = FALSE)
#  
#  customers_names %>%
#    agify(simplify = FALSE) %>%
#    knitr::kable(row.names = FALSE)
#  

## ----agify_calculate, cache=FALSE, echo=FALSE---------------------------------
df <- structure(list(name = c("Maria", "Ben", "Claudia", "Adam", "Hannah", "Robert"), 
               type = c("age", "age", "age", "age", "age", "age"), 
               age = c(21L, 48L, 45L, 34L, 27L, 59L), 
               count = c(517258L, 75632L, 110105L, 110754L, 12843L, 160915L)), 
          row.names = c(5L, 2L, 3L, 1L, 4L, 6L), class = "data.frame")

knitr::kable(df, row.names = FALSE)

## ----nationality_print, cache=FALSE, eval=FALSE-------------------------------
#  customers_predicted_nationality <- nationalize(name = customers_names, simplify = FALSE)
#  
#  customers_names %>%
#    nationalize(simplify = FALSE) %>%
#    knitr::kable(row.names = FALSE)
#  

## ----nationality_calculate, cache=FALSE, echo=FALSE---------------------------
df <- structure(list(name = c("Maria", "Ben", "Claudia", "Adam", "Hannah", "Robert"), 
               type = c("nationality", "nationality", "nationality", "nationality", "nationality", "nationality"),
               country_id = c("CY", "AU", "CL", "PL", "SL", "US"), 
               probability = c(0.0550797623186088, 0.0665534047056411, 0.0559340459826196, 0.0905835974781003, 0.267325397602591, 0.0909442129069588)), 
          row.names = c(5L, 2L, 3L, 1L, 4L, 6L), class = "data.frame")

knitr::kable(df, row.names = FALSE)

## ----country_id, cache=FALSE--------------------------------------------------
us_customers_predicted_gender<-genderize(name = customers_names, 
                                         country_id = "US")
us_customers_predicted_gender

us_customers_predicted_age<-agify(name = customers_names,
                                  country_id = "US")
us_customers_predicted_age


## ----supported_countries_print, cache=FALSE, eval=FALSE-----------------------
#  supported_countries(type = "genderize") %>%
#    head(5) %>%
#    knitr::kable(row.names = FALSE)

## ----supported_countries_calculate, cache=FALSE, echo=FALSE-------------------
df <- structure(list(country_id = c("AD", "AE", "AF", "AG", "AI"), 
               name = c("Andorra", "United Arab Emirates", "Afghanistan", "Antigua and Barbuda", "Anguilla"),
               total = c(29783L, 145847L, 23531L, 1723L, 1081L)), 
          row.names = c(NA, 5L), class = "data.frame")

knitr::kable(df, row.names = FALSE)

## ----country_id_multi_print, cache=FALSE, eval=FALSE--------------------------
#  agify(name = c("Hannah", "Ben"),
#        country_id = c("US", "GB"),
#        simplify = FALSE) %>%
#    knitr::kable(row.names = FALSE)
#  

## ----country_id_multi_calculate, cache=FALSE, echo=FALSE----------------------
df <- structure(list(name = c("Hannah", "Ben"), 
                     type = c("age", "age"),
                     age = c(54L, 38L),
                     count = c(67L, 1980L), 
                     country_id = c("US", "GB")),
                row.names = 2:1, class = "data.frame") 

knitr::kable(df, row.names = FALSE)

## ----meta_parameter_print, cache=FALSE, eval=FALSE----------------------------
#  genderize(name = "Hannah",
#            simplify = FALSE,
#            meta = TRUE) %>%
#    knitr::kable(row.names = FALSE)

## ----meta_parameter_calculate, cache=FALSE, echo=FALSE------------------------
df <- structure(list(name = "Hannah",
                     type = "gender",
                     gender = "female", 
                     probability = 0.97, 
                     count = 13198L, 
                     api_rate_limit = 1000L, 
                     api_rate_remaining = 977L,
                     api_rate_reset = 7218L,
                     api_request_timestamp = 
                       structure(1588715982, class = c("POSIXct", "POSIXt"), tzone = "GMT")),
                row.names = 1L, class = "data.frame")
knitr::kable(df, row.names = FALSE)

## ----sliced_false_print, cache=FALSE, eval=FALSE------------------------------
#  nationalize(name = "Matthias",
#              simplify = FALSE,
#              sliced=FALSE) %>%
#    knitr::kable(row.names = FALSE)

## ----sliced_false_calculate, cache=FALSE, echo=FALSE--------------------------
df <- structure(list(name = c("Matthias", "Matthias", "Matthias"), 
                     type = c("nationality", "nationality", "nationality"),
                     country_id = c("DE", "AT", "CH"), 
                     probability = c(0.41616382151, 0.265062479571606, 0.1106921611552)), 
                row.names = c(NA, 3L), class = "data.frame")
knitr::kable(df, row.names = FALSE)

## ----customers_end_print, echo=TRUE, cache=FALSE, eval=FALSE------------------
#  library(dplyr)
#  
#  df<-data.frame("Customers:"=c("Maria", "Ben", "Claudia",
#                             "Adam", "Hannah", "Robert"),
#                 stringsAsFactors = FALSE,
#                 check.names = FALSE)
#  
#  df <- df %>% mutate(`Estimated gender:`= genderize(`Customers:`),
#                      `Estimated age:`= agify(`Customers:`),
#                      `Estimated nationality:`= nationalize(`Customers:`))
#  
#  df %>% t() %>% knitr::kable(col.names = NULL)
#  

## ----customers_end_calc, echo=FALSE, cache=FALSE------------------------------
df <- structure(
  c("Maria", "female", "21", "CY", "Ben", "male", "48", 
    "AU", "Claudia", "female", "45", "CL", "Adam", "male", "34", 
    "PL", "Hannah", "female", "27", "SL", "Robert", "male", "59", "US"),
  .Dim = c(4L, 6L), 
  .Dimnames = list(c("Customers:", "Estimated gender:", "Estimated age:", "Estimated nationality:"), NULL))

knitr::kable(df, col.names = NULL)


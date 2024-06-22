## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(visR)

## ----data_prep----------------------------------------------------------------
attrition <- visR::get_attrition(adtte,
   criteria_descriptions = c("1. Not in Placebo Group",
                             "2. Be 75 years of age or older.",
                             "3. White",
                             "4. Female"),
   criteria_conditions   = c("TRTP != 'Placebo'",
                             "AGE >= 75",
                             "RACE=='WHITE'",
                             "SEX=='F'"),
   subject_column_name   = "USUBJID")

## ----render1, fig.align='center', fig.width= 6, fig.height=6------------------
attrition %>%
  visR::visr("Criteria", "Remaining N")

## ---- data_control------------------------------------------------------------
attrition$Complement <- c("NA", "Placebo Group", "Younger than 75 years", "Non-White", "Male")

## ----render2, fig.align='center', fig.width= 6, fig.height=6------------------
attrition %>%
  visR::visr("Criteria", "Remaining N", "Complement")

## ----render3, fig.align='center', fig.width= 6, fig.height=6------------------
attrition %>%
  visR::visr("Criteria", "Remaining N", "Complement", fill = "lightblue", border="grey")


## ----render4,  fig.align='center', fig.width= 6, fig.height=6-----------------
attrition %>%
 visR::visr("Criteria", "Remaining N", font_size = 10)


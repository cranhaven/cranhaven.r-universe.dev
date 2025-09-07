## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
#  fig.path = "",
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(implicitMeasures)

## -----------------------------------------------------------------------------
data("raw_data")
# explore the dataframe
str(raw_data)

# explore the levels of the blockcode variable to identify the SC-IAT blocks
levels(raw_data$blockcode)

## -----------------------------------------------------------------------------
data("raw_data")
sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                         block_id = "blockcode",
                         latency_id = "latency",
                         accuracy_id = "correct",
                         block_sciat_1 = c("test.sc_dark.Darkbad",
                                           "test.sc_dark.Darkgood"),
                         block_sciat_2 = c("test.sc_milk.Milkbad",
                                           "test.sc_milk.Milkgood"),
                         trial_id  = "trialcode",
                         trial_eliminate = c("reminder",
                                             "reminder1"), 
                         demo_id = "blockcode", 
                         trial_demo = "demo")

## -----------------------------------------------------------------------------
str(sciat_data) # structure of the resulting List

## -----------------------------------------------------------------------------
sciat1 <- sciat_data[[1]] # extract first SC-IAT data
sciat2 <- sciat_data[[2]] # extract second SC-IAT data
demo_data <- sciat_data[[3]] # extract demographic information

head(sciat1)
head(demo_data)

## -----------------------------------------------------------------------------
# Compute the D score for the first SC-IAT
 d_sciat1 <- compute_sciat(sciat1,
                  mappingA = "test.sc_dark.Darkbad",
                  mappingB = "test.sc_dark.Darkgood",
                  non_response = "alert")

# dataframe containing the SC-IAT D score of the of the first SC-IAT
str(d_sciat1) 
 
# Compute D score for the second SC-IAT
 d_sciat2 <- compute_sciat(sciat2,
                  mappingA = "test.sc_milk.Milkbad",
                  mappingB = "test.sc_milk.Milkgood",
                  non_response = "alert")
 
 # dataframe containing the SC-IAT D score of the of the second SC-IAT
 head(d_sciat2)


## -----------------------------------------------------------------------------
descript_d(d_sciat1) # Data frame containing SC-IAT D scores

## -----------------------------------------------------------------------------
descript_d(d_sciat2, # Data frame containing IAT D scores
           latex = TRUE) # obtain the code for latex tables

## ----fig.align='center', fig.width=6, fig.cap="Default use of function d_point()"----
 d_point(d_sciat1) # Data frame containing SC-IAT D scores

## ----scpointSettings, fig.align='center', fig.width=6, fig.cap="\\label{fig:scpointSettings} Function d_point() with settings change"----
d_point(d_sciat1, # dataframe containing SC-IAT D scores
       order_sbj = "D-increasing", # change respondents' order
       x_values = FALSE,  # remove respondents' labels
       include_stats = TRUE, # include descriptive statistics
       col_point = "aquamarine3") # change points color

## ----fig.align='center', fig.width=6, fig.cap="Default use of function d_density() function"----
d_density(d_sciat1) # Data frame containing SC-IAT D scores

## ----sampleSettings, fig.align='center', fig.width=6, fig.cap="\\label{fig:sampleSettings}d_density() function with settings change"----
d_density(d_sciat1, # dataframe containing IAT Dscores
        graph = "density", # change graphical representation
        include_stats = TRUE) # include descriptive statistics

## ----fig.align='center', fig.width=6, fig.cap="Default results representation of function multi_dsciat()"----
multi_dsciat(d_sciat1, # dataframe containing the results of the first SC-IAT
             d_sciat2) # dataframe containing the results of the second SC-IAT


## ----fig.align='center', fig.width=6, fig.cap="Results representation of function multi_dsciat() with settings change"----
multi_dsciat(d_sciat1, # dataframe containing the results of the first SC-IAT
             d_sciat2, # dataframe containing the results of the second SC-IAT
             graph = "boxplot", # change graph type
       x_values = FALSE, # take out x values
       gcolors = "greens", # change color
       label_sc1 = "Dark SC-IAT",  # change label first SC-IAT
       label_sc2 = "Milk SC-IAT") # change label second SC-IAT


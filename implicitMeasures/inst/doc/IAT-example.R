## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  #fig.path = "",
  warning = FALSE, 
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(implicitMeasures)

## -----------------------------------------------------------------------------
data("raw_data")
# explore the dataframe
str(raw_data)
# explore the levels of the blockcode variable to identify the IAT blocks
levels(raw_data$blockcode)

## ----eval = T-----------------------------------------------------------------
iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                          block_id = "blockcode",
                          mapA_practice = "practice.iat.Milkbad",
                          mapA_test = "test.iat.Milkbad",
                          mapB_practice = "practice.iat.Milkgood",
                          mapB_test = "test.iat.Milkgood",
                          latency_id = "latency",
                          accuracy_id = "correct",
                          trial_id = "trialcode",
                          trial_eliminate = c("reminder", "reminder1"),
                          demo_id = "blockcode",
                          trial_demo = "demo")

## -----------------------------------------------------------------------------
str(iat_cleandata)

## -----------------------------------------------------------------------------
iat_data <- iat_cleandata[[1]]
head(iat_data)

## -----------------------------------------------------------------------------
dscore <- compute_iat(iat_data, Dscore = "d3")
str(dscore)

## -----------------------------------------------------------------------------
descript_d(dscore) # Data frame containing IAT Ds

## -----------------------------------------------------------------------------
descript_d(dscore, # Data frame containing IAT Ds
           latex = TRUE) # obtain the code for latex tables

## -----------------------------------------------------------------------------
IAT_rel(dscore)

## ----fig.align='center', fig.width=6, fig.cap="d_point() function with default settings"----
d_point(dscore) # Data frame containing IAT D scores

## ----fig.align='center', fig.width=6, fig.cap="d_point() function with   settings change"----
d_point(dscore, # dataframe containing IAT Ds
       order_sbj = "D-decreasing", # change respondents order
       x_values = FALSE, # remove respondents' labels
       include_stats = TRUE, # include descriptive statistics
       col_point = "lightskyblue") # change points color

## ----fig.align='center', fig.width=6, fig.cap="d_density() function with default settings"----
d_density(dscore) # dataframe containing IAT Dscores

## ----sampleSettings, fig.align='center', fig.width=6, fig.cap="\\label{fig:sampleSettings}d_density() function with settings change"----
d_density(dscore, # dataframe containing IAT Dscores
        graph = "boxplot", # change graphical representation
        include_stats = TRUE) # include descriptive statistics

## -----------------------------------------------------------------------------
multi_scores <- multi_dscore(iat_data, # object with class "iat_clean"
                             ds = "error-inflation") # string specifying the 
                                            # algorithms to compute

## -----------------------------------------------------------------------------
multi_d <- multi_scores[[1]]
head(multi_d)
str(multi_d)

## ----fig.align='center', fig.width=6, fig.cap="Multiple IAT D scores representation"----
multi_scores[[2]]


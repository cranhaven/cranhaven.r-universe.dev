## ----setup, include = FALSE----------------------------------------------
library(PRISMAstatement)
suppressPackageStartupMessages(library(DiagrammeR))
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 3
  )

## ----genericflowexamples, fig.cap = "Fictitious study data, presenting just counts"----
flow_exclusions(
  incl_counts = c(972, 132, 77, 14),
  total_label = "Total Screened",
  incl_labels = c("Consented", "Completed Study", "BMI <= 30"),
  excl_labels = c("Declined Consent", "Failed to Complete", "BMI > 30")
)

## ----percoftotal, fig.cap = "Percentages of the Total figure"------------
flow_exclusions(c(1000, 300, 150, 75, 38), percent_of_total = TRUE)

## ----percofprev, fig.cap = "Percentages of the previous box"-------------
flow_exclusions(c(100000, 3000, 1666, 411, 38),
                percent_of_prev = TRUE,
                show_count = FALSE)

## ----genericflowexamples2, fig.cap = "Demonstrating other options"-------
flow_exclusions(
  incl_counts = c(972, 132, 77, 14),
  total_label = "Total Screened",
  incl_labels = c("Consented", "Completed Study", "BMI <= 30"),
  excl_labels = c("Declined Consent", "Failed to Complete", "BMI > 30"), 
  percent_of_total = TRUE,
  percent_of_prev = FALSE,
  show_count = FALSE,
  font_size = 14)


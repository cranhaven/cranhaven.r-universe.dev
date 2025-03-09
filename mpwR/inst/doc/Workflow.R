## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(mpwR)
library(flowTraceR)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(flextable)

## ----import, eval = FALSE-----------------------------------------------------
#  files <- prepare_mpwR(path = "Path_to_Folder_with_files")

## ----get-example-data---------------------------------------------------------
files <- create_example()

## ----ID-Report----------------------------------------------------------------
ID_Reports <- get_ID_Report(input_list = files)

## ----show-ID-Report-----------------------------------------------------------
flextable::flextable(ID_Reports[["DIA-NN"]])

## ----plot-ID-barplot----------------------------------------------------------
ID_Barplots <- plot_ID_barplot(input_list = ID_Reports, level = "ProteinGroup.IDs")

## ----show-ID-barplot----------------------------------------------------------
ID_Barplots[["DIA-NN"]]

## ----plot-ID-boxplot----------------------------------------------------------
plot_ID_boxplot(input_list = ID_Reports, level = "ProteinGroup.IDs")

## ----DC-Report----------------------------------------------------------------
DC_Reports <- get_DC_Report(input_list = files, metric = "absolute")
DC_Reports_perc <- get_DC_Report(input_list = files, metric = "percentage")

## ----show-DC-Report-----------------------------------------------------------
flextable::flextable(DC_Reports[["DIA-NN"]])

## ----plot-DC-barplot----------------------------------------------------------
DC_Barplots <- plot_DC_barplot(input_list = DC_Reports, level = "ProteinGroup.IDs", label = "absolute")

## ----show-DC-barplot----------------------------------------------------------
DC_Barplots[["DIA-NN"]]

## ----show-DC-barplot-percentage-----------------------------------------------
plot_DC_barplot(input_list = DC_Reports_perc, level = "ProteinGroup.IDs", label = "percentage")[["DIA-NN"]]

## ----plot-DC-stacked-barplot--------------------------------------------------
plot_DC_stacked_barplot(input_list = DC_Reports, level = "ProteinGroup.IDs", label = "absolute")

## ----plot-DC-stacked-barplot-percentage---------------------------------------
plot_DC_stacked_barplot(input_list = DC_Reports_perc, level = "ProteinGroup.IDs", label = "percentage")

## ----MC-Report----------------------------------------------------------------
MC_Reports <- get_MC_Report(input_list = files, metric = "absolute")
MC_Reports_perc <- get_MC_Report(input_list = files, metric = "percentage")

## ----show-MC-Report-----------------------------------------------------------
flextable::flextable(MC_Reports[["Spectronaut"]])

## ----plot-MC-barplot----------------------------------------------------------
MC_Barplots <- plot_MC_barplot(input_list = MC_Reports, label = "absolute")

## ----show-MC-barplot----------------------------------------------------------
MC_Barplots[["Spectronaut"]]

## ----show-MC-barplot-percentage-----------------------------------------------
plot_MC_barplot(input_list = MC_Reports_perc, label = "percentage")[["Spectronaut"]]

## ----plot-MC-stacked-barplot--------------------------------------------------
plot_MC_stacked_barplot(input_list = MC_Reports, label = "absolute")

## ----plot-MC-stacked-barplot-percentage---------------------------------------
plot_MC_stacked_barplot(input_list = MC_Reports_perc, label = "percentage")

## ----CV-RT--------------------------------------------------------------------
CV_RT <- get_CV_RT(input_list = files)

## ----CV-RT-plot---------------------------------------------------------------
plot_CV_density(input_list = CV_RT, cv_col = "RT")

## ----CV-Pep-------------------------------------------------------------------
CV_LFQ_Pep <- get_CV_LFQ_pep(input_list = files)

## ----CV-Pep-plot--------------------------------------------------------------
plot_CV_density(input_list = CV_LFQ_Pep, cv_col = "Pep_quant")

## ----CV-PG--------------------------------------------------------------------
CV_LFQ_PG <- get_CV_LFQ_pg(input_list = files)

## ----CV-PG-plot---------------------------------------------------------------
plot_CV_density(input_list = CV_LFQ_PG, cv_col = "PG_quant")

## ----prepare-Upset------------------------------------------------------------
Upset_prepared <- get_Upset_list(input_list = files, level = "ProteinGroup.IDs")

## ----plot-Upset---------------------------------------------------------------
plot_Upset(input_list = Upset_prepared, label = "ProteinGroup.IDs")

## ----Upset-flowTraceR-off-----------------------------------------------------
get_Upset_list(input_list = files, level = "Peptide.IDs") %>% #prepare Upset
  plot_Upset(label = "Peptide.IDs") #plot

## ----Upset-flowTraceR-on------------------------------------------------------
get_Upset_list(input_list = files, level = "Peptide.IDs", flowTraceR = TRUE) %>% #prepare Upset
  plot_Upset(label = "Peptide.IDs") #plot

## ----summary-report, eval = FALSE---------------------------------------------
#  Summary_Report <- get_summary_Report(input_list = files)

## ----plot-radarchart, eval = FALSE--------------------------------------------
#  plot_radarchart(input_df = Summary_Report)

## ----plot-radarchart-DC, eval = FALSE-----------------------------------------
#  #Focus on Data Completeness
#  Summary_Report %>%
#    dplyr::select(Analysis, contains("Full")) %>% #Analysis column and at least one category column is required
#    plot_radarchart()


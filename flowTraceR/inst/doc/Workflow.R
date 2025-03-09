## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(flowTraceR)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(data.table)
library(kableExtra)

## ----import, eval=FALSE, include=TRUE-----------------------------------------
#  diann <- data.table::fread("DIRECTORY/dia-nn_file.tsv")
#  spectronaut <- data.table::fread("DIRECTORY/spectronaut_file.tsv")
#  mq_evidence <- data.table::fread("DIRECTORY/maxquant_evidence.txt")
#  mq_proteinGroups <- data.table::fread("DIRECTORY/maxquant_proteinGroups.txt")
#  pd_psm <- data.table::fread("DIRECTORY/pd_PSMs.txt")

## ----get example data---------------------------------------------------------
#DIA-NN
diann <- flowTraceR::get_example("DIA-NN")

#Spectronaut
spectronaut <- flowTraceR::get_example("Spectronaut")

#MaxQuant
mq_evidence <- flowTraceR::get_example("MaxQuant")[["evidence"]]

mq_proteinGroups <- flowTraceR::get_example("MaxQuant")[["proteinGroups"]]

#PD
pd_psm <- flowTraceR::get_example("PD")

## ----precursor----------------------------------------------------------------
diann_precursor_converted <- convert_precursor(input_df = diann, software = "DIA-NN")
spectronaut_precursor_converted <- convert_precursor(input_df = spectronaut, software = "Spectronaut")
mq_precursor_converted <- convert_precursor(input_df = mq_evidence, software = "MaxQuant")
pd_precursor_converted <- convert_precursor(input_df = pd_psm, software = "PD")

## ----modified peptides--------------------------------------------------------
diann_peptides_converted <- convert_modified_peptides(input_df = diann, software = "DIA-NN")
spectronaut_peptides_converted <- convert_modified_peptides(input_df = spectronaut, software = "Spectronaut")
mq_peptides_converted <- convert_modified_peptides(input_df = mq_evidence, software = "MaxQuant")
pd_peptides_converted <- convert_modified_peptides(input_df = pd_psm, software = "PD")

## ----proteinGroup-------------------------------------------------------------
diann_proteinGroups_converted <- convert_proteingroups(input_df = diann, software = "DIA-NN")
spectronaut_proteinGroups_converted <- convert_proteingroups(input_df = spectronaut, software = "Spectronaut")
mq_proteinGroups_converted <- convert_proteingroups(input_df = mq_proteinGroups, software = "MaxQuant")
pd_proteinGroups_converted <- convert_proteingroups(input_df = pd_psm, software = "PD")

## ----all levels---------------------------------------------------------------
diann_all_converted <- convert_all_levels(input_df = diann, software = "DIA-NN")
spectronaut_all_converted <- convert_all_levels(input_df = spectronaut, software = "Spectronaut")
mq_all_converted <- convert_all_levels(input_df = mq_evidence, input_MQ_pg = mq_proteinGroups, software = "MaxQuant")
pd_all_converted <- convert_all_levels(input_df = pd_psm, software = "PD")

## ----analyzing conversion-----------------------------------------------------
#For one software example - equivalent for others.

#Proteome Discoverer
#Reports
pd_precursor_report_unknown_mods <- analyze_unknown_mods(input_df = pd_precursor_converted, level = "precursor", plot = FALSE)
pd_peptides_report_unknown_mods <- analyze_unknown_mods(input_df = pd_peptides_converted, level = "modified_peptides", plot = FALSE)

#Plots
pd_precursor_plot_unknown_mods <- analyze_unknown_mods(input_df = pd_precursor_converted, level = "precursor", plot = TRUE, plot_characteristic = "absolute")
pd_peptides_plot_unknown_mods <- analyze_unknown_mods(input_df = pd_peptides_converted, level = "modified_peptides", plot = TRUE, plot_characteristic = "relative")

## -----------------------------------------------------------------------------
kableExtra::kable(pd_precursor_report_unknown_mods)

## ----conversion-plot----------------------------------------------------------
pd_precursor_plot_unknown_mods

## ----trace individual level---------------------------------------------------
#Binary Comparison - DIA-NN vs. Spectronaut

#ProteinGroup level
traced_proteinGroups <- trace_level(input_df1 = diann_all_converted , input_df2 = spectronaut_all_converted, analysis_name1 = "DIA-NN", analysis_name2 = "Spectronaut", level = "proteinGroups", filter_unknown_mods = TRUE)

#Peptide level
traced_peptides <- trace_level(input_df1 = diann_all_converted, input_df2 = spectronaut_all_converted, analysis_name1 = "DIA-NN", analysis_name2 = "Spectronaut", level = "modified_peptides", filter_unknown_mods = TRUE)

#Precursor level
traced_precursor <- trace_level(input_df1 = diann_all_converted, input_df2 = spectronaut_all_converted, analysis_name1 = "DIA-NN", analysis_name2 = "Spectronaut", level = "precursor", filter_unknown_mods = TRUE)

## ----trace all levels---------------------------------------------------------
#Binary Comparison - DIA-NN vs. Spectronaut

#trace all levels in one step
traced_all <- trace_all_levels(input_df1 = diann_all_converted, input_df2 = spectronaut_all_converted, analysis_name1 = "DIA-NN", analysis_name2 = "Spectronaut", filter_unknown_mods = TRUE)

## ----Connect flowTraceR levels------------------------------------------------
#ProteinGroup level
DIANN_connected_proteinGroup <- connect_traceR_levels(input_df = traced_all[["DIA-NN"]], level = "proteinGroups")
Spectronaut_connected_proteinGroup <- connect_traceR_levels(input_df = traced_all[["Spectronaut"]], level = "proteinGroups")

#Peptide level
DIANN_connected_peptides <- connect_traceR_levels(input_df = traced_all[["DIA-NN"]], level = "modified_peptides")
Spectronaut_connected_peptides <- connect_traceR_levels(input_df = traced_all[["Spectronaut"]], level = "modified_peptides")

## ----analyze connected levels-------------------------------------------------
#Example for proteinGroup level

#*Plots*
#upper level - proteinGroup level - how many proteingroups have a specific categorization
DIANN_plot_proteinGroups_upper <- analyze_connected_levels(input_df = DIANN_connected_proteinGroup, connected_levels = "proteinGroup_precursor",count_level = "upper", plot = TRUE, plot_characteristic = "absolute")

Spectronaut_plot_proteinGroups_upper <- analyze_connected_levels(input_df = Spectronaut_connected_proteinGroup, connected_levels = "proteinGroup_precursor", count_level = "upper", plot = TRUE, plot_characteristic = "absolute")

#lower level - precursor level - how many precursor have a specific categorization
DIANN_plot_proteinGroups_lower <- analyze_connected_levels(input_df = DIANN_connected_proteinGroup, connected_levels = "proteinGroup_precursor",count_level = "lower", plot = TRUE, plot_characteristic = "absolute")

Spectronaut_plot_proteinGroups_lower <- analyze_connected_levels(input_df = Spectronaut_connected_proteinGroup, connected_levels = "proteinGroup_precursor", count_level = "lower", plot = TRUE, plot_characteristic = "absolute")


#*Reports*
#ProteinGroup level
DIANN_report_proteinGroups <- analyze_connected_levels(input_df = DIANN_connected_proteinGroup, connected_levels = "proteinGroup_precursor",count_level = "upper", plot = FALSE)

Spectronaut_report_proteinGroups <- analyze_connected_levels(input_df = Spectronaut_connected_proteinGroup, connected_levels = "proteinGroup_precursor",count_level = "lower", plot = FALSE)

## -----------------------------------------------------------------------------
kableExtra::kable(DIANN_report_proteinGroups)

## ----proteingroups-connected--------------------------------------------------
DIANN_plot_proteinGroups_upper

## ----get software difference--------------------------------------------------
#with string_analysis = TRUE - if protein denotation is mentioned in both proteinGroups of input_df1/_df2 are filtered out - only distinct protein denotations remain

Difference_proteinGroup <- trace_unique_common_pg(input_df1 = DIANN_connected_proteinGroup, input_df2 = Spectronaut_connected_proteinGroup, analysis_name1 = "DIA-NN", analysis_name2 = "Spectronaut", string_analysis = FALSE)

Difference_proteinGroup_reduced <- trace_unique_common_pg(input_df1 = DIANN_connected_proteinGroup, input_df2 = Spectronaut_connected_proteinGroup, analysis_name1 = "DIA-NN", analysis_name2 = "Spectronaut", string_analysis = TRUE)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(Difference_proteinGroup, format = "pipe", caption = "Difference in proteinGroup denotation - string_analysis = FALSE")

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kable(Difference_proteinGroup_reduced, format = "pipe", caption = "Difference in proteinGroup denotation - string_analysis = TRUE")


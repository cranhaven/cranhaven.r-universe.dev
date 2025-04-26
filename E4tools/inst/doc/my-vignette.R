## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(E4tools)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  E4_EDA_Process.part1.ExtractRawEDA(participant_list=c(1001),
#                                     ziplocation="~/extdata/E4_demo_data/",
#                                     rdslocation.EDA="~/extdata/output/raw_EDA/",
#                                     summarylocation="~/extdata/output/summaries/",
#                                     EDA_low_cut=0.001,LowPctCutoff=.75,
#                                     EDA_high_cut=25,HighPctCutoff=.75)
#  

## ----eval=FALSE, echo=FALSE----------------------------------------------
#  E4_EDA_Process.part1.ExtractRawEDA(participant_list=c(1001),
#                                     ziplocation=paste(system.file(package="E4tools"),"/extdata/E4_demo_data/",sep=""),
#                                     rdslocation.EDA=paste(tempdir(),"/extdata/output/raw_EDA/",sep=""),
#                                     summarylocation=paste(tempdir(),"/extdata/output/summaries/",sep=""),
#                                     EDA_low_cut=0.001,LowPctCutoff=.75,
#                                     EDA_high_cut=25,HighPctCutoff=.75)
#  

## ---- echo=FALSE,eval=FALSE----------------------------------------------
#  options(scipen=999)
#  Table0<-readRDS("EDA_presses_COMBINED.RDS")
#  Table0a<-head(Table0)
#  knitr::kable(Table0a, format = "html")

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  E4_EDA_Process.part2.ExtractButtonPresses(participant_list=c(1001),
#                                            ziplocation="~/extdata/E4_demo_data/",
#                                            rdslocation.buttonpress="~/extdata/output/presses/",
#                                            summarylocation="~/extdata/output/summaries/",
#                                            cutoff.ends=2,
#                                            cutoff.overlap=20)

## ----eval=FALSE, echo=FALSE----------------------------------------------
#  E4_EDA_Process.part2.ExtractButtonPresses(participant_list=c(1001),
#                                            ziplocation=paste(system.file(package="E4tools"),"/extdata/E4_demo_data/",sep=""),
#                                            rdslocation.buttonpress=paste(tempdir(),"/extdata/output/presses/",sep=""),
#                                            summarylocation=paste(tempdir(),"/extdata/output/summaries/",sep=""),
#                                            cutoff.ends=2,
#                                            cutoff.overlap=20)

## ---- echo=FALSE,eval=FALSE----------------------------------------------
#  Table1<-readRDS("1002_EDA.RDS")
#  Table1a<-head(Table1)
#  knitr::kable(Table1a, format = "html")

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  E4_EDA_Process.part3.MatchPressesToEDA(participant_list=c(1001),
#                                         rdslocation.buttonpress="~/extdata/output/presses/",
#                                          rdslocation.MatchedEDA="~/extdata/output/matched_EDA/",
#                                        rdslocation.EDA="~/extdata/output/raw_EDA/",
#                                         min.before=20,min.after=20,control=T)

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  E4_EDA_Process.part3.MatchPressesToEDA(participant_list=c(1001),
#                                         rdslocation.buttonpress=paste(tempdir(),"/extdata/output/presses/",sep=""),
#                                          rdslocation.MatchedEDA=paste(tempdir(),"/extdata/output/matched_EDA/",sep=""),
#                                        rdslocation.EDA=paste(tempdir(),"/extdata/output/raw_EDA/",sep=""),
#                                         min.before=20,min.after=20,control=T)

## ---- echo=FALSE,eval=FALSE----------------------------------------------
#  Table2<-readRDS("EDA_presses_COMBINED.RDS")
#  Table2a<-head(Table2)
#  knitr::kable(Table2a, format = "html")

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  E4_EDA_Process.part4.BinMatchedEDA(participant_list=c(1001),
#                                      rdslocation.MatchedEDA="~/extdata/output/matched_EDA/",
#                                      rdslocation.BinnedMatchedEDA="~/extdata/output/binned_matched_EDA/",
#                                     min.after = 20,min.before = 20)

## ----eval=FALSE, echo=FALSE----------------------------------------------
#  E4_EDA_Process.part4.BinMatchedEDA(participant_list=c(1001),
#                                        rdslocation.MatchedEDA=paste(tempdir(),"/extdata/output/matched_EDA/",sep=""),
#                                      rdslocation.BinnedMatchedEDA=paste(tempdir(),"/extdata/output/binned_matched_EDA/",sep=""),
#                                     min.after = 20,min.before = 20)

## ---- echo=FALSE,eval=FALSE----------------------------------------------
#  load(paste(system.file(package="E4tools"),"/data/EDA_Vignette4.rda",sep=""))
#  EDA_Vignette4a<-head(EDA_Vignette4)
#  knitr::kable(EDA_Vignette4a, format = "html")


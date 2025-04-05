## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----xcms faaKO, eval=FALSE, include=TRUE-------------------------------------
#  library(BiocManager)
#  library(xcms)
#  install.packages("faahKO")
#  library(faahKO)
#  cdfpath <- system.file("cdf", package = "faahKO")
#  cdffiles <- list.files(cdfpath, recursive = TRUE, full.names = TRUE)
#  # to point to your own directory
#  # cdffiles <- list.files(utils::choose.dir(), recursive = TRUE, full.names = TRUE, pattern = ".cdf")
#  # note: choose.dir() will bring up a window to browse to your directory
#  # the pattern argument is case sensitive, ensure it matches your file type in a case sensitive
#  # manner
#  # see vignette('xcms') for xcms use and guidance
#  xset <- xcmsSet(cdffiles)  # detect features
#  xset <- group(xset)  # group features across samples by retention time and mass
#  xset <- retcor(xset, family = "symmetric", plottype = NULL)  # correct for drive in retention time
#  xset <- group(xset, bw = 10)  # regroup following rt correction
#  xset <- fillPeaks(xset)  # 'fillPeaks' to remove missing values in final dataset

## ----view xcms object summary, eval=FALSE, include=TRUE-----------------------
#  xset

## ----ramclustR installation, eval=FALSE, include=TRUE-------------------------
#  install.packages("devtools", repos="http://cran.us.r-project.org", dependencies=TRUE)
#  library(devtools)
#  install_github("cbroeckl/RAMClustR", build_vignettes = TRUE, dependencies = TRUE)
#  library(RAMClustR)

## ----ramclustR of xcms processed faaKO, eval=FALSE, include=TRUE--------------
#  experiment <- defineExperiment(csv = TRUE) # experiment <- defineExperiment(force.skip = TRUE)
#  RC <- ramclustR(xcmsObj = xset, ExpDes=experiment)

## ----export csv, eval=FALSE, include=TRUE-------------------------------------
#  write.csv(RC$SpecAbund, file="SpecAbund.csv", row.names=TRUE)

## ----csv input, eval=FALSE, include=TRUE--------------------------------------
#  # make csv files - outcsv1 for real MS data, outcsv2 for 'fake' idMSMS data after adding some noise.
#  outcsv1<-RC$MSdata
#  outcsv2<-abs(jitter(outcsv1, factor = 0.1))
#  write.csv(outcsv1, file = paste0(getwd(), "/msdata.csv"), row.names = TRUE)
#  write.csv(outcsv2, file = paste0(getwd(), "/msmsdata.csv"), row.names = TRUE)
#  
#  # run ramclustR on those csv files
#  # first the MS data only
#  
#  RC1 <- ramclustR(ms = paste0(getwd(), "/msdata.csv"),
#                   featdelim = "_",
#                   st = 5,
#                   ExpDes=experiment,
#                   sampNameCol = 1)
#  
#  # then the MS and MSMS data.
#  # first we need to redefine our experiment, make sure to enter 'LC-MS' for plaform and '2' for the LC-MS MSlevs
#  experiment <- defineExperiment(csv = TRUE)
#  RC2 <- ramclustR(ms = paste0(getwd(), "/msdata.csv"),
#                   idmsms = paste0(getwd(), "/msmsdata.csv"),
#                   featdelim = "_",
#                   timepos = 2,
#                   st = 5,
#                   ExpDes=experiment,
#                   sampNameCol = 1)

## ----do.findmain, eval=FALSE, include=TRUE------------------------------------
#  RC <- do.findmain(RC, mode = "positive", mzabs.error = 0.02, ppm.error = 10)


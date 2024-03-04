## ----Installing and loading packages,eval=FALSE--------------------------
#  install.packages("GENEAread",repos = "http://cran.us.r-project.org")
#  install.packages("devtools",repos = "http://cran.us.r-project.org")
#  install.packages("changepoint",repos = "http://cran.us.r-project.org")
#  install.packages("signal",repos = "http://cran.us.r-project.org")
#  install.packages("mmap",repos = "http://cran.us.r-project.org")
#  install.packages("misc3d",repos = "http://cran.us.r-project.org")
#  install.packages("rgl",repos = "http://cran.us.r-project.org")
#  install.packages("mapproj",repos = "http://cran.us.r-project.org")
#  
#  library(GENEAread)
#  library(devtools)
#  library(changepoint)
#  library(signal)
#  library(mmap)
#  library(misc3d)
#  library(rgl)
#  library(mapproj)

## ----Installing GENEAclassify,eval=FALSE---------------------------------
#  setwd("/Users/owner/Documents/GENEActiv")
#  # You will need to change this to the directory where you saved the tar.gz file
#  install.packages("GENEAclassify_1.4.1.tar.gz", repos=NULL, type="source")
#  
#  #' Or using a GitHub authentication key which will go in the brackets of auth_token
#  
#  install_github("https://github.com/JossLangford/GENEAclassify_1.41.git",
#                 auth_token = "7f0051aaca453eaabf0e60d49bcf752c0fea0668")
#  
#  #' Once the package has been installed load in the library
#  library(GENEAclassify)

## ----Installing GENEAsphere,eval=FALSE-----------------------------------
#  # Again install GENEAsphere either from source or GitHub
#  setwd("/Users/owner/Documents/GENEActiv")
#  # You will need to change this to the directory where you saved the tar.gz file
#  install.packages("GENEAsphere_1.0.tar.gz", repos=NULL, type="source")
#  
#  ## If installing from GitHub please run these lines
#  install_github("https://github.com/JossLangford/GENEAsphere.git",
#                 auth_token = "7f0051aaca453eaabf0e60d49bcf752c0fea0668")
#  
#  library(GENEAsphere)

## ----Reading in the data,eval=FALSE--------------------------------------
#  setwd("/Users/owner/Documents/GENEActiv/GENEAsphereDemo")
#  # You will need to change this to the directory containing the data file.
#  # Here I have analysed the first day for.
#  AccData = read.bin("jl_left wrist_010094_2012-01-30 20-39-54.bin", start = "3:00", end = "1 3:00")
#  SegData = getSegmentedData("jl_left wrist_010094_2012-01-30 20-39-54.bin", start = "3:00", end = "1 3:00")

## ----viewing data objects,eval=FALSE-------------------------------------
#  names(AccData)
#  head(AccData)
#  head(AccData$data.out) # Raw data output
#  names(SegData)
#  head(SegData)

## ----Plot of acceleration against Time,eval=FALSE------------------------
#  plot(AccData$data.out[1:1000,1],AccData$data.out[1:1000,2],
#       title="Time against X acceleration",
#       xlab="Time",ylab="X Acceleration",type="l")

## ---- eval = FALSE-------------------------------------------------------
#  plot.AccData(x, what = ("sd"))
#  plot.AccData(x, what = ("mean"))
#  plot.AccData(x, what = ("temperature"))
#  plot.AccData(x, what = ("light"))
#  plot.AccData(x, what = ("voltage"))

## ---- eval = FALSE-------------------------------------------------------
#  plotTLM(x, start = NULL, end = NULL)

## ----STFT plot,eval=FALSE------------------------------------------------
#  
#  stft(AccData, start=0.45, end=0.5, plot.it=TRUE)
#  
#  stft(AccData, start=0.45, end=0.5, plot.it=TRUE,reassign = TRUE)
#  
#  stft(AccData, start=0.45, end=0.5, plot.it=TRUE, type = "mv")
#  
#  stft(AccData, start=0.45, end=0.5, plot.it=TRUE, type = "sum")
#  # Changing the window size
#  stft(AccData, start=0.45, end=0.5, plot.it=TRUE, win=100)
#  
#  stft(AccData, start=0.45, end=0.5, plot.it=TRUE, win=1000)
#  

## ----postionals plot,eval=FALSE------------------------------------------
#  positionals(AccData, start=0.45, end= 0.5, length = NULL, filter=2
#              ,bw = TRUE , legend = TRUE, max.points = 1e6, density = FALSE)

## ----plotsphere plot,eval=FALSE------------------------------------------
#  plotSphere(AccData, start=0, end= 0.5, length = NULL, time.format = "auto",
#             density = F, arrow = T, add= F)

## ----loading in the segmentation csv,eval=FALSE--------------------------
#  segmentationCSV="~/GENEAclassification/jl_left wrist_010094_2012-01-30 20-39-54_segmented.csv"
#  
#  # I find it useful to load in the csv to the workspace so that the rows I'm going to plot can be seen.
#  csv=read.table(segmentationCSV,sep=",")

## ----running plotSegmentSphere,eval=FALSE--------------------------------
#  plotRows=c(1:5) # Segments 1 and 5.
#  plotSegmentSphere(segmentationCSV, plotRows, levels = c(0.9, 0.75, 0.5, 0.25, 0.1), singlePlot = TRUE, col = heat.colors(5),
#                    alpha = c(0.03, 0.05, 0.1, 0.2, 0.3), arrow = FALSE, nsims = 1000)

## ----plotSegmentFlat,eval=FALSE------------------------------------------
#  plotSegmentFlat(segmentationCSV, plotRows,
#                  col = c("red",heat.colors(5, alpha = c(0.3, 0.2, 0.1, 0.05, 0.03))),
#                  singlePlot = TRUE, nsims= 1000)

## ----plotSegmentProjection,eval=FALSE------------------------------------
#  plotSegmentProjection(segmentationCSV, plotRows, projection = "aitoff",
#                        col = "red", singlePlot = TRUE, nsims = 1000)

## ----plotSegmentEllipse,eval=FALSE---------------------------------------
#  plotSegmentEllipse(segmentationCSV, plotRows, projection = "aitoff",
#                     col = "red", singlePlot = TRUE, confidenceLevel = 0.05,
#                     alpha = thresholds, wrap = FALSE, greyGrid = FALSE)


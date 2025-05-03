#'Sample of Experimental Data for Forecast Verification In Function Of 
#'Latitudes And Depths
#'
#'This data set provides data in function of latitudes and depths for the 
#'variable 'tos', i.e. sea surface temperature, from the decadal climate 
#'prediction experiment run at IC3 in the context of the CMIP5 project.\cr 
#'Its name within IC3 local database is 'i00k'.
#'
#'@usage data(sampleDepthData)
#'@format The data set provides with a variable named 'sampleDepthData'.\cr\cr
#'
#'sampleDepthData$exp is an array that contains the experimental data and the 
#'dimension meanings and values are:\cr
#'  c(# of experimental datasets, # of members, # of starting dates, 
#'    # of lead-times, # of depths, # of latitudes)\cr
#'  c(1, 5, 3, 60, 7, 21)\cr\cr
#'
#'sampleDepthData$obs should be an array that contained the observational data 
#'but in this sample is not defined (NULL).\cr\cr
#'
#'sampleDepthData$depths is an array with the 7 longitudes covered by the data.\cr\cr
#'
#'sampleDepthData$lat is an array with the 21 latitudes covered by the data.\cr\cr
#'@name sampleDepthData
#'@docType data
sampleDepthData <- function(){}

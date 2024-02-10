#' Compute MaxEnt model
#'
#'@description Compute species distribution models with MaxEnt (Maximum Entropy)
#'
#'@usage
#'compute.maxent(x, proj.predictors)
#'
#'
#'@param x \link{SDMtab} object or dataframe that contains id, longitude, latitude and values of environmental descriptors at corresponding locations.
#'
#'@param proj.predictors RasterStack of environmental descriptors on which the model will be projected

#'
#'@details
#'MaxEnt species distribution model minimizes the relative entropy between environmental descriptors and presence data. Further information are provided in the references below.
#'
#'compute.maxent uses the functionalities of the \link[dismo]{maxent} function. This function uses MaxEnt species distribution software, which is a java program that could be downloaded at \url{https://github.com/charleneguillaumot/SDMPlay}. In order to run compute.maxent, put the 'maxent.jar' file downloaded at this address in the 'java' folder of the dismo package (path obtained with the system.file('java', package='dismo') command).
#'
#'
#'@note
#'To implement MaxEnt models, Phillips & Dudik (2008) advice a large number of background data. You can also find further information about background selection in Barbet Massin et al. (2012).
#'
#'@return
#'\itemize{
#'A list of 4
#'\item \emph{model$algorithm} "maxent" string character
#'\item \emph{model$data} x dataframe that was used to implement the model
#'\item \emph{model$response} Parameters returned by the model object
#'\item \emph{model$raster.prediction} Raster layer that predicts the potential species distribution}
#'
#'
#'@references
#'Barbet Massin M, F Jiguet, C Albert & W Thuiller (2012) Selecting pseudo absences for species distribution models: how, where and how many? \emph{Methods in Ecology and Evolution}, 3(2): 327-338.
#'
#'Elith J, S Phillips, T Hastie, M Dudik, Y Chee &  C Yates (2011) A statistical explanation of MaxEnt for ecologists. \emph{Diversity and Distributions} 17:43-57.
#'
#'Phillips S, M Dudik & R Schapire (2004) A maximum entropy approach to species distribution modeling. \emph{Proceedings of the Twenty-First International Conference on Machine Learning} : 655-662
#'
#'Phillips S, R Anderson & R Schapire (2006) Maximum entropy modeling of species geographic distributions. \emph{Ecological Modelling} 190:231-259.
#'
#'Phillips S and M Dudik (2008) Modeling of species distributions with MaxEnt: new extensions and a comprehensive evaluation. \emph{Ecography} 31(2): 161-175.
#'
#'@seealso
#'\link[dismo]{maxent}
#'
#'@examples
#'#Download the presence data
#'data('ctenocidaris.nutrix')
#'occ <- ctenocidaris.nutrix
#'# select longitude and latitude coordinates among all the information
#'occ <- ctenocidaris.nutrix[,c('decimal.Longitude','decimal.Latitude')]
#'
#'#Download some environmental predictors
#'data(predictors2005_2012)
#'envi <- predictors2005_2012
#'envi
#'
#'#Create a SDMtab matrix
#'SDMtable_ctenocidaris <- SDMPlay:::SDMtab(xydata=occ,
#'                                          predictors=predictors2005_2012,
#'                                          unique.data=FALSE,
#'                                          same=TRUE)
#'
#' #only run if the maxent.jar file is available, in the right folder
#' #jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

#'# Check first if maxent can be run (normally not part of your script)
#'# (file.exists(jar) & require(rJava)) == TRUE ??
#'# rJava may pose a problem to load automatically within this package
#'# please load it manually using eventually the archives available from CRAN
#'
#'# Run the model
#'#model <- SDMPlay:::compute.maxent(x=SDMtable_ctenocidaris, proj.predictors=envi)
#'
#'# Plot the map prediction
#'library(grDevices) # add nice colors
#' #palet.col <- colorRampPalette(c('deepskyblue','green','yellow','red'))(80)
#'#'raster::plot(model$raster.prediction, col=palet.col)
#'data('worldmap')
#'# add data
#'points(worldmap, type="l")
#'#points(occ, col='black',pch=16)
#'
#'# Get the partial dependance curves
#'#dismo::response(model$response)
#'
#'# Get the percentage of contribution of each variable to the model
#'#plot(model$response)
#'
#'# Get all the information provided by the model on a html document
#'#model$response
#'


compute.maxent <- function(x, proj.predictors) {

  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
  if (!file.exists(jar)) {
    stop('file missing:\n', jar, '.\nPlease download it here: https://github.com/charleneguillaumot/SDMPlay')
  }

    if (!requireNamespace("dismo")) {
        stop("you need to install the dismo package to run this function")
    }

    model <- dismo::maxent(x[, 4:ncol(x)], x[, 1], args = "-J")
    prediction <- raster::predict(model, proj.predictors)
    return(list(algorithm = "maxent", data = x, response = model, raster.prediction = prediction))
}

#' scalpel: A package for processing calcium imaging data.
#'
#' This package is called scalpel for "Segmentation, Clustering, and Lasso Penalties",
#' which is a method for processing neuronal calcium imaging data that identifies the locations of
#' neurons, and estimates their calcium concentrations over time.
#' The main function is \code{\link{scalpel}}, which runs the entire SCALPEL pipeline.
#' The pipeline involves several steps, each of which is described briefly in its corresponding
#' function. See \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}},
#' \code{\link{scalpelStep3}} for more details. Results can be summarized using \code{\link{summary}}
#' and the main plotting function is \code{\link{plotResults}}, which displays the estimated spatial and temporal components.
#' Full details for the SCALPEL method are provided in Petersen, Ashley; Simon, Noah; Witten, Daniela. 
#' SCALPEL: Extracting neurons from calcium imaging data. Ann. Appl. Stat. 12 (2018), no. 4, 2430--2456. 
#' doi:10.1214/18-AOAS1159. https://projecteuclid.org/euclid.aoas/1542078051
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#'  #general example illustrating some of the main functions
#'  #see the vignette for additional direction on using all of the functions
#'  #and the help pages for the specific functions for details on using each function
#'
#'  #existing folder to save results (update this to an existing folder on your computer)
#'  outputFolder = "scalpelResults"
#'  #location on computer of raw data in R package to use
#'  rawDataFolder = gsub("Y_1.rds", "", system.file("extdata", "Y_1.rds", package = "scalpel"))
#'  #video height of raw data in R package
#'  videoHeight = 30
#'  #run SCALPEL pipeline
#'  scalpelOutput = scalpel(outputFolder = outputFolder, rawDataFolder = rawDataFolder,
#'                          videoHeight = videoHeight)
#'  #summarize each step
#'  summary(scalpelOutput, step = 0)
#'  summary(scalpelOutput, step = 1)
#'  summary(scalpelOutput, step = 2)
#'  summary(scalpelOutput, step = 3)
#'
#'  #plot the spatial and temporal components
#'  plotResults(scalpelOutput = scalpelOutput)
#'  #plot a summary of the video with the found neurons outlined
#'  plotVideoVariance(scalpelOutput = scalpelOutput, neuronSet = "Afilter")
#'  #plot the frames with the most fluorescence for each found neuron
#'  plotBrightest(scalpelOutput = scalpelOutput, AfilterIndex = 1)
#'  plotBrightest(scalpelOutput = scalpelOutput, AfilterIndex = 2)
#'  plotBrightest(scalpelOutput = scalpelOutput, AfilterIndex = 3)
#'
#'  #if you want to use results from a previous session,
#'  #use "getScalpel" to read in previous results
#'  scalpelOutputCopy = getScalpel(outputFolder = outputFolder)
#'
#' }
#'
#' @docType package
#' @useDynLib scalpel, .registration=T
#' @name scalpel-package
#' @aliases scalpel-package
NULL

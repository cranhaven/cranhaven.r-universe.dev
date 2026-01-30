#' Phenological parameters estimation in mass
#' 
#' Estimation of phenological parameters from a set of numeric vectors stored in a RData file. 
#' Output is saved as a RData file at the destination specified by \code{dirToSave}
#'
#' @param               path character with full path of RData file containing numeric vectors to analyze.
#' @param            product character specifying whether dataset is the \code{MOD13Q1} product (default) or 
#'                           a different one (\code{independent}).
#' @param               data matrix with dataset to analyze. Pertinent when \code{product="independent"} only.                           
#' @param          frequency integer giving number of observations per season. Default, 23.
#' @param             method character. Should \code{OLS} or \code{WLS} be used for smoothing each
#'                           numeric vector in RData file specified in \code{path}?
#' @param              sigma numeric vector of length equal to \code{frequency}. Each entry gives the 
#'                           standard deviation of observations acquired at same day of the year. 
#'                           Pertinent when \code{method=WLS}.
#' @param            numFreq integer specifying number of frequencies to use in harmonic regression
#'                           model.
#' @param              delta numeric. Default, 0. When regression problem is ill-posed, this
#'                           parameter allows a simple regularization.
#' @param           distance character indicating what distance to use in hierarchical clustering.
#'                           All distances in \code{\link[dtwclust]{tsclust}} are allowed.
#' @param            samples integer with number of samples to draw from smoothed version of numeric
#'                           vector to analyze. Used exclusively in Functional Principal Components Analysis (FPCA)-based
#'                           regression.
#' @param              basis list giving numeric basis used in FPCA-based regression. See details.
#' @param               corr Default \code{NULL}. Object defining correlation structure, can be
#'                           numeric vector, matrix or function.
#' @param                  k integer, number of principal components used in FPCA-based regression.
#' @param              trace logical. If \code{TRUE}, progress on the hierarchical clustering
#'                           is printed on console. Default, \code{FALSE}.
#' @param           numCores integer. How many processing cores can be used?
#' @param          dirToSave character. In which directory to save analysis results?
#' @param reportFileBaseName character. What base name should be given to a progress report file? 
#'                           Default, \code{phenopar_progress}.
#' @param outputFileBaseName character. What base name should be given to the output file? 
#'                           Default, \code{polygon}.                            
#'
#' @export
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom parallel stopCluster
#' @importFrom utils globalVariables
#' @importFrom dtwclust tsclust
#' @importFrom geoTS haRmonics
#' @importFrom geoTS hetervar
#' @importFrom eBsc drbasis
#' @importFrom rootSolve uniroot.all
#'
#' @examples 
#' \donttest{
#' dirOUTPUT <- system.file("data", package = "sephora")
#' BASIS <- drbasis(n=100, q=2)
#' 
#' polygon_deciduous <- deciduous_polygon
#' for(i in 1:nrow(polygon_deciduous)){
#'   polygon_deciduous[i,] <- vecFromData(data=deciduous_polygon, numRow=i)$vec
#' }
#' 
#' # --- In the following example 'numCores=2' for CRAN
#' # --- testing purposes only. In a real life example
#' # --- users are encouraged to set 'numCores' to a number
#' # --- that reflects the size of their data set as well
#' # --- as the number of available cores
#' 
#' phenopar_polygon(data=polygon_deciduous,
#'                  product="independent",
#'                  numFreq = 3, distance = "dtw2",
#'                  samples=100, basis=BASIS,
#'                  k=3, numCores=2,
#'                  dirToSave=dirOUTPUT,
#'                  outputFileBaseName = "deciduous")
#'                  
#' # --- Auxiliary function to read phenopar_polygon output,
#' # --- used below to define deciduous_params object                   
#' LoadToEnvironment <- function(RData, env = new.env()){
#'                               load(RData, env)
#'                               return(env)}
#'                  
#' # --- colors used in spiralPlot below
#' cgu <- rgb(173/255,221/255,142/255)
#' csos <- rgb(120/255,198/255,121/255)
#' cmat <- rgb(49/255, 163/255,84/255)
#' csen <- rgb(217/255, 95/255, 14/255)
#' ceos <- rgb(254/255, 153/255, 41/255)
#' cdor <- rgb(208/255, 209/255, 230/255)
#' 
#' colores <- c(cgu,csos,cmat,csen,ceos,cdor)
#' 
#' # --- how to get a SpiralPlot
#' listRDatas <- list.files(path=dirOUTPUT,
#'                          pattern=".RData",
#'                          full.names=TRUE)
#'                          
#' deciduous_params <- LoadToEnvironment(listRDatas[1])
#' 
#' getSpiralPlot(MAT=deciduous_params$output, 
#'               LABELS=month.name,
#'               vp_param=list(width=0.5, height=0.7))
#' vcd::grid_legend(x=1.215, y=0.125, pch=18, col=colores,
#'                 frame=FALSE,
#'                 labels=c("GU","SoS","Mat","Sen","EoS","Dor"),
#'                 title="Params")
#'             
#' # --- cleaning up after work
#' unlink(paste0(dirOUTPUT, "/deciduous_phenoParams.RData"))
#' unlink(paste0(dirOUTPUT, "/phenopar_progress.txt"))}
#'
#' @return At the location specified by \code{dirToSave}, a file containing a matrix
#' with \code{nrow} equal to the number of numeric vectors analyzed and 6 columns, is saved. 
#' The name of this file is:
#' 
#' \code{paste0(tools::file_path_sans_ext(basename(path)), "_phenoparams.RData")}.
#'
#' @seealso \code{\link[sephora]{phenopar}}, \code{\link[sephora]{getSpiralPlot}}, 
#' \code{\link[dtwclust]{tsclust}}.
#'
phenopar_polygon <- function(path=NULL, product=c("MOD13Q1", "independent"), data,
                             frequency=23, method=c("OLS", "WLS"), sigma=NULL,
                             numFreq, delta=0, distance, samples, basis, corr=NULL,
                             k, trace=FALSE, numCores=20, dirToSave,
                             reportFileBaseName="phenopar_progress",
                             outputFileBaseName="polygon"){
  
  if(missing(dirToSave)){
    stop('dirToSave must be provided')
  }
  
  product <- match.arg(product)
  
  method <- match.arg(method)
  
  if(product == "MOD13Q1"){
    if(is.null(path)){
      stop('path must be provided')
    }
    
    # data <- LoadToEnvironment(RData=path)$poly[[1]] * 1e-4
    
    # primera prueba
    # data <- get(ls(LoadToEnvironment(RData=path)))
    
    #segunda prueba
    dataEnv <- LoadToEnvironment(RData=path)
    data <- get(x="poly", envir=dataEnv)[[1]]
    
  } else {
    if(missing(data)){
      stop('data must be provided')
    }
  }
  
  output <- phenopar_polygon_auxiliar(data=data, product=product, frequency=frequency, method=method,
                                      sigma=sigma, numFreq=numFreq, delta=delta, distance=distance, 
                                      samples=samples, basis=basis, corr=corr, k=k, trace=trace,
                                      numCores=numCores, dirToSave=dirToSave, reportFileBaseName=reportFileBaseName)
  
  
  save(output, file = paste0(dirToSave, "/",
                             ifelse( !is.null(path), 
                                     tools::file_path_sans_ext(basename(path)), 
                                     outputFileBaseName ),
                             "_phenoParams.RData"))
  
}

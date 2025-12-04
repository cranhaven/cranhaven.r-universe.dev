#' Gapfilling for Earth Observation datasets
#' 
#' Making use of parallel computing this function allows for the application
#' of \code{\link[gapfill]{Gapfill}} to a set of satellite images. It is mandatory
#' that these images have been configured for processing according to the
#' workflow of this package previously.
#' 
#' @param       inputDir character. Full path name of directory containing files configured
#'                       to be processed with \code{\link[gapfill]{Gapfill}}. See \bold{Note}.
#' @param      outputDir character. Full path name of directory where output will be saved.
#'                       See \bold{Note}.  
#' @param    progressDir character. Full path name of directory where a file reporting on the
#'                       status of the process will be saved. See \bold{Note}.
#' @param            lat character vector. See \code{\link[igapfill]{get_LAT}}.
#' @param            lon character vector. See \code{\link[igapfill]{get_LON}}. 
#' @param           days numeric vector indicating what days are being considered. See \code{\link[igapfill]{get_4Darray}}.
#' @param          years integer vector indicating what years are being considered. See \code{\link[igapfill]{get_4Darray}}.
#' @param       numCores numeric. How many cores should be employed in parallel computing?
#' @param          scale numeric. See \code{\link[gapfill]{Gapfill}}. Default is \code{1e-4}. See \bold{Note}.
#' @param      clipRange numeric vector of length 2. See \code{\link[gapfill]{Gapfill}}. Default is
#'                       \code{c(-1,1)}. See \bold{Note}.
#' @param addArgToReport logical. Should a copy of the input arguments be passed onto the progress
#'                       report file? Default is \code{TRUE}.
#' 
#' @export
#' 
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom gapfill Gapfill
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster
#' @importFrom gtools mixedsort
#' 
#' @note Within the workflow of this package, \code{inputDir}, \code{outputDir} and
#' \code{progressDir} must be equal to the sub-directories \emph{/splits}, 
#' \emph{/output}, and \emph{/progressReports}, created by \code{\link[igapfill]{create_dirs}},
#' respectively. Many satellite products come with a scale factor of \code{1e4}, using
#' \code{scale=1e-4} maps pixel values to the interval \code{(-1,1)} which is the default for
#' argument \code{clipRange}.
#' 
#' @details Should the users have not yet created it, this function allows to create the 
#' directory/folders structure employed by this package workflow. When users opt for creating such
#' a directory structure, additional arguments are required at the console. When users acknowledge 
#' the existence of such directory structure or when they decide to create such structure independently,
#' this function returns the message \emph{"Try again later passing the required parameters"}.
#' 
#' @seealso \code{\link[igapfill]{create_dirs}}, \code{\link[foreach]{foreach}}, \code{\link[gapfill]{Gapfill}}, 
#' \code{\link[parallel]{makeCluster}}, \code{\link[doParallel]{registerDoParallel}}
#' 
#' 
#' @return Should the user decide to employ this function, at the output directory (\code{outputDir})
#' there will be \code{.RData} files containing the output of parallel-based calls to \code{\link[gapfill]{Gapfill}}.
#' There will be as many \code{.RData} files as files are in any of the sub-directories indicated by
#' \code{inputDir}.
#' 
applyGapfill <- function(inputDir, outputDir, progressDir, 
                         lat, lon, days, years, 
                         numCores = 6, scale = 1e-4, 
                         clipRange = c(-1,1),
                         addArgToReport=TRUE){

  if(missing(inputDir)){
    stop("'inputDir' must be provided")
  }
  
  if(missing(outputDir)){
    stop("'outputDir' must be provided")
  }
  
  if(missing(progressDir)){
    stop("'progressDir' must be provided")
  }
  
  if(missing(lat)){
    stop("'lat' must be provided")
  }
  
  if(missing(lon)){
    stop("'lon' must be provided")
  }
  
  if(missing(days)){
    stop("'days' must be provided")
  }
  
  if(missing(years)){
    stop("'years' must be provided")
  }
  
  message(colorText("Check out " , 216), 
          colorText(progressDir, 159), 
          colorText(" to monitor gap-filling progress", 216))
  
  get_applyGapfill(inputDir=inputDir,
                   outputDir=outputDir,
                   progressDir=progressDir,
                   lat=lat, lon=lon, days=days, years=years,
                   numCores=numCores, scale=scale, 
                   clipRange=clipRange,
                   addArgToReport=addArgToReport) 
  
}

#' Console-based interface for filling missing values of Earth Observation datasets
#' 
#' Command-line user-friendly application that allows the application of 
#' \code{\link[gapfill]{Gapfill}} to a set of satellite images.
#' 
#' This function is a wrap-up of \code{\link[igapfill]{create_dirs}}, 
#' \code{\link[igapfill]{sort_split}} and \code{\link[igapfill]{applyGapfill}}
#' allowing users to provide some of the arguments employed by these functions.
#' 
#' @param saveArguments logical. Should the arguments defined during the execution of
#'                      this function be added to a progress report file? Default is \code{TRUE}.
#' 
#' @export
#' 
#' @importFrom raster stack
#' @importFrom raster raster
#' @importFrom raster mosaic
#' @importFrom raster writeRaster
#' @importFrom raster rasterOptions
#' @importFrom raster ymin
#' @importFrom raster ymax
#' @importFrom raster xmin
#' @importFrom raster xmax
#' @importFrom raster nrow 
#' @importFrom terra rast
#' @importFrom terra ncol
#' @importFrom terra nrow
#' @importFrom terra add<-
#' @importFrom terra ncell
#' @importFrom terra nlyr
#' @importFrom terra as.polygons
#' @importFrom terra ext
#' @importFrom terra writeRaster
#' @importFrom terra datatype
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils globalVariables
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom gapfill Gapfill
#' @importFrom gtools mixedsort 
#' @importFrom geoTS matrixToRaster
#' @importFrom parallel stopCluster
#' @importFrom itertools isplitVector
#' @importFrom iterators icount
#' 
#' @seealso \code{\link[igapfill]{sort_split}}, \code{\link[parallel]{detectCores}},
#' \code{\link[igapfill]{applyGapfill}}
#' 
#' @return At the specified location there will be \code{.RData} files containing the output of 
#' parallel-based calls to \code{\link[gapfill]{Gapfill}}.
#' 
igapfill <- function(saveArguments=TRUE){
  
  START_YEAR <- as.numeric(readline(justColorText("First year to process: ", 220)))
  
  END_YEAR <- as.numeric(readline(justColorText("Last year to process: ", 220)))
  
  PATH <- readline(justColorText("Full path (directory containing images to process): ", 220))
  
  if(!dir.exists(PATH)){
    stop(paste0(PATH, " does not exists"))
  }
  
  pathFILES <- mixedsort(list.files(path=PATH,
                                    pattern = ".tif",
                                    full.names = TRUE))
  
  if( length(pathFILES) == 0 ){
    stop(paste0(PATH, " must have some images to process"))
  }
  
  question_i <- readline(justColorText("Have you set up this directory according with my documentation? [Y/n] ", 220))
  
  if(question_i == "n" | question_i == "no"){
    question_ii <- readline(justColorText("Do you want me to set up your working directory now? [Y/n] ", 220))
    
    if(question_ii == "n" | question_ii == "no"){
      return("Process stopped with no action taken")
    } else {

      create_dirs(path=PATH, startYear = START_YEAR, endYear = END_YEAR)
      
    }
  }
  
  question_iii <- readline(justColorText("Have you split your original images? [Y/n] ", 220))
  
  if(question_iii == "n" | question_iii == "no"){
    question_iv <- readline(justColorText("Do you want me to take care of the splitting process? [Y/n] ", 220))
    
    if(question_iv=="n" | question_iv == "no"){
      return("Process stopped with no action taken")
    } else { # --- commented code below will be relevant when terra allows parallel computing
      # PARALLEL <- as.character(readline(justColorText("Should I do some parallel computing for this process? [Y/n] ", 220)))
      # PARALLEL <- ifelse(PARALLEL=="n", FALSE, TRUE)
      # 
      # if(PARALLEL) {
      #   NUMCORES_sort_split <- as.numeric(readline(justColorText("How many CPU cores should I use? ", 220)))
      # } else {
      #   NUMCORES_sort_split <- 1
      # }
      
      output_split <- sort_split(path = PATH, startYear = START_YEAR, endYear = END_YEAR)
      # parallelProcessing = PARALLEL, 
      # numCores = NUMCORES_sort_split)
    }
  } 
  
  if(is.null(output_split)){
    return("Process stopped with no action taken")
  } else {
    
    dirInput <- paste0(PATH, "/gapfill/splits")
    
    dirOutput <- paste0(PATH, "/gapfill/output")
    
    dirProgress <- paste0(PATH, "/gapfill/progressReports")
    
    
    message( colorText("gapfill application starts here", 82) )
    
    dirSPLITS <- list.dirs(path = dirInput)[-1]
    
    genPath_firstSplit <- list.files(path = dirSPLITS[1],
                                     pattern = ".tif$", 
                                     full.names = TRUE)
    
    STACK <- stack(genPath_firstSplit[1])
    
    LONGITUDE <- get_LON(stack=STACK)
    
    LATITUDE <- get_LAT(stack=STACK)
    
    YEARS <- START_YEAR:END_YEAR
    
    DAYS <- numeric(length(YEARS))
    for(i in 1:length(DAYS)){
      DAYS[i] <- as.numeric(readline(justColorText(paste0(i, "-th Day: "), 220)))
    }
    
    NUMCORES <- as.numeric(readline(justColorText("How many cores of your system do you want me to use? ", 220)))
    question_scale <- readline(justColorText("By default, the scale is 1, do you want me to re-scale the observations in your images? [Y/n] ", 220))
    
    if(question_scale=="no" | question_scale == "n"){
      SCALE <- 1
    } else {
      SCALE <- as.numeric(readline(justColorText("Scale: ", 220)))
    }
    
    question_clip <- readline(justColorText("By default, clip range is (-1,1), do you want me to change the clip range? [Y/n] ", 220))
    
    if(question_clip=="no" | question_clip=="n"){
      CLIP_RANGE <- c(-1,1)
    } else {
      CLIP_min <- as.numeric(readline(justColorText("Lower limit: ", 220)))
      CLIP_max <- as.numeric(readline(justColorText("Upper limit: ", 220)))
      CLIP_RANGE <- c(CLIP_min, CLIP_max)
    }
    
    message( colorText("gapfilling is on", 82) )
    
    applyGapfill(inputDir = dirInput,
                 outputDir = dirOutput,
                 progressDir = dirProgress,
                 lat = LATITUDE, lon = LONGITUDE,
                 days = DAYS, years = YEARS,
                 numCores = NUMCORES,
                 scale = SCALE, clipRange = CLIP_RANGE,
                 addArgToReport = saveArguments)
    
    message( colorText("Rasterization/mosaicking follows now", 82) )
    
    allDIRS <- list.dirs(path = PATH,
                         full.names = TRUE)#[-1]
    
    question_datatype <- readline(justColorText("By default, datatype is INT4S, do you want me to change it? [Y/n] ", 220))
    
    if(question_datatype=="no" | question_datatype == "n"){
      DATA_TYPE <- "INT4S"
    } else {
      DATA_TYPE <- readline(justColorText("Datatype: ", 220))
    }
    
    message(colorText("---> Number of cores should not exceed number of splits <---", 200))
    
    NUM_CORES <- as.numeric(readline(justColorText("How many cores of your system do you want me to use? ", 220)))
    
    parallel_mosaic(inputDirImages = PATH,
                    inputDirRData = allDIRS[5],
                    inputDirMaster = allDIRS[4],
                    outputDir = allDIRS[3],
                    progressReportDir = allDIRS[6],
                    scaleFactor = SCALE,
                    dataType = DATA_TYPE,
                    numCores = NUM_CORES) # numCores must not exceed number of splits
    
    
  }
  
}

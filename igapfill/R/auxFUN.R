# --- Setting color of console text
# --- USED in: create_dirs; waysToSplit
colorText <- function(text, foreground=1, background){
  
  out <- "\033[38;5;"
  
  if( base::missing(background) ){
    out <- paste0(out, foreground)
  } else {
    out <- paste0(out, foreground, ";48;5;", background)
  }
  
  out <- paste0(out, ";m", text, "\033[0m")
  
  cat(out)
}


# --- USED in: get_4Darray
# @param listLayer list, containing lists with names of files to be assembled as a 4D array.
# @param       lon character, vector whose entries indicate longitude coordinates.
# @param       lat character, vector whose entries indicate latitude coordinates. 
# @param      days numeric, vector indicating what DoYs are being considered. Length of this
#                  object must be equal to length of \code{listLayer}. 
# @param     years integer, vector indicating what years are being considered. Length of this
#                  object must be equal to length of \code{listLayer}.
# 
# @return An array of 4 dimensions: longitud, latitude, days and years
# 
# @seealso \code{get_LON}, \code{get_LAT}, \code{getListFiles}
# 
get_array_lat_lon_day_year <- function(listLayer, lon, lat, days, years){
  
  array_lat_lon_days_years <- array(NA, 
                                    dim = c(length(lat), length(lon), length(listLayer), length(listLayer)),
                                    dimnames = list(lat, lon, days, years))
  
  for(i in 1:length(listLayer)){
    for(j in 1:length(listLayer)){
      array_lat_lon_days_years[,,j,i] <- listLayer[[i]][,,j]
    }
  }
  
  array_lat_lon_days_years
}

# --- REQUIRED: The following functions are required by minmaxBlock

# --- Added on March 27, 2025

getMinBlock2 <- function(row, col, sieve){
  
  TOP <- (row-1):row
  LEFT <- (col-1):col
  RIGHT <- col:(col+1)
  BOTTOM <- row:(row+1)
  
  if( row == 1 ){
    TOP <- BOTTOM
    if ( col == 1 ){
      LEFT <- RIGHT
    }
    if ( col == ncol(sieve) ){
      RIGHT <- LEFT
    }
  }
  
  if( row == nrow(sieve) ){
    BOTTOM <- TOP
    if( col == 1 ){
      LEFT <- RIGHT
    }
    if( col == ncol(sieve) ){
      RIGHT <- LEFT
    }
  }
  
  if( col == 1 ){
    LEFT <- RIGHT
    if ( row == 1 ){
      TOP <- BOTTOM
    }
    if ( row == nrow(sieve) ){
      BOTTOM <- TOP
    }
  }
  
  if( col == ncol(sieve) ){
    RIGHT <- LEFT
    if( row == 1 ){
      TOP <- BOTTOM
    }
    if( row == nrow(sieve) ){
      BOTTOM <- TOP
    }
  }
  
  blocksGrid <- vector(mode = "list", length = 4)
  
  blocksGrid[[1]] <- sieve[ TOP, LEFT ]
  
  blocksGrid[[2]] <- sieve[ TOP, RIGHT ]
  
  blocksGrid[[3]] <- sieve[ BOTTOM, LEFT ]
  
  blocksGrid[[4]] <- sieve[ BOTTOM, RIGHT ]
  
  # blockProducts <- lapply(1:length(blocksGrid), 
  #                         function(s) as.numeric(cumprod( blocksGrid[[s]] ) ))
  
  blockProducts <- lapply(1:length(blocksGrid), 
                          function(s) as.numeric(log(cumsum( blocksGrid[[s]] ) )))
  
  validBlocks <- lapply(1:length(blockProducts), 
                        function(s) sum(!is.na(blockProducts[[s]])))
  
  if( length(which(unlist(validBlocks)==4)) != 0 ){
    BLOCK <- which.min(lapply(1:length(blocksGrid), 
                              function(s) blockProducts[[s]][length(blockProducts[[s]])] ))
    
    if(BLOCK == 1){
      ROWS <- TOP
      COLS <- LEFT
    }
    
    if(BLOCK == 2){
      ROWS <- TOP
      COLS <- RIGHT
    }
    
    if(BLOCK == 3){
      ROWS <- BOTTOM
      COLS <- LEFT
    }
    
    if(BLOCK == 4){
      ROWS <- BOTTOM
      COLS <- RIGHT
    }
    
  } else {
    ROWS <- NA
    COLS <- NA
  }
  
  # BLOCK <- which.min(lapply(1:length(blocksGrid), 
  #                           function(s) blockProducts[[s]][length(blockProducts[[s]])] ))
  
  # if(BLOCK == 1){
  #   ROWS <- TOP
  #   COLS <- LEFT
  # }
  # 
  # if(BLOCK == 2){
  #   ROWS <- TOP
  #   COLS <- RIGHT
  # }
  # 
  # if(BLOCK == 3){
  #   ROWS <- BOTTOM
  #   COLS <- LEFT
  # }
  # 
  # if(BLOCK == 4){
  #   ROWS <- BOTTOM
  #   COLS <- RIGHT
  # }
  
  if( length(which(unlist(validBlocks)==4)) != 0 ){
    out_block <- blocksGrid[[BLOCK]]
    out_blockMissingness <- blockProducts[[BLOCK]][length(blockProducts)]
  } else {
    out_block <- NA
    out_blockMissingness <- NA
  }
  
  # if( !is.na(ROWS) ){
  #   out_block <- blocksGrid[[BLOCK]]
  # } else {
  #   out_block <- NA
  # }
  
  list(rows = ROWS, cols = COLS,      
       block=out_block,
       blockMissingness = out_blockMissingness)
}


getMaxBlock2 <- function(row, col, sieve){
  
  TOP <- (row-1):row
  LEFT <- (col-1):col
  RIGHT <- col:(col+1)
  BOTTOM <- row:(row+1)
  
  if( row == 1 ){
    TOP <- BOTTOM
    if ( col == 1 ){
      LEFT <- RIGHT
    }
    if ( col == ncol(sieve) ){
      RIGHT <- LEFT
    }
  }
  
  if( row == nrow(sieve) ){
    BOTTOM <- TOP
    if( col == 1 ){
      LEFT <- RIGHT
    }
    if( col == ncol(sieve) ){
      RIGHT <- LEFT
    }
  }
  
  if( col == 1 ){
    LEFT <- RIGHT
    if ( row == 1 ){
      TOP <- BOTTOM
    }
    if ( row == nrow(sieve) ){
      BOTTOM <- TOP
    }
  }
  
  if( col == ncol(sieve) ){
    RIGHT <- LEFT
    if( row == 1 ){
      TOP <- BOTTOM
    }
    if( row == nrow(sieve) ){
      BOTTOM <- TOP
    }
  }
  
  blocksGrid <- vector(mode = "list", length = 4)
  
  blocksGrid[[1]] <- sieve[ TOP, LEFT ]
  
  blocksGrid[[2]] <- sieve[ TOP, RIGHT ]
  
  blocksGrid[[3]] <- sieve[ BOTTOM, LEFT ]
  
  blocksGrid[[4]] <- sieve[ BOTTOM, RIGHT ]
  
  blockProducts <- lapply(1:length(blocksGrid), 
                          function(s) as.numeric(log(cumsum( blocksGrid[[s]] ) )))
  
  validBlocks <- lapply(1:length(blockProducts), 
                        function(s) sum(!is.na(blockProducts[[s]])))
  
  if( length(which(unlist(validBlocks)==4)) != 0 ){
    BLOCK <- which.max(lapply(1:length(blocksGrid), 
                              function(s) blockProducts[[s]][length(blockProducts[[s]])] ))
    
    if(BLOCK == 1){
      ROWS <- TOP
      COLS <- LEFT
    }
    
    if(BLOCK == 2){
      ROWS <- TOP
      COLS <- RIGHT
    }
    
    if(BLOCK == 3){
      ROWS <- BOTTOM
      COLS <- LEFT
    }
    
    if(BLOCK == 4){
      ROWS <- BOTTOM
      COLS <- RIGHT
    }
    
  } else {
    ROWS <- NA
    COLS <- NA
  }
  
  if( length(which(unlist(validBlocks)==4)) != 0 ){
    out_block <- blocksGrid[[BLOCK]]
    out_blockMissingness <- blockProducts[[BLOCK]][length(blockProducts)]
  } else {
    out_block <- NA
    out_blockMissingness <- NA
  }
  
  list(rows = ROWS, cols = COLS,      
       block=out_block,
       blockMissingness = out_blockMissingness)
}

# ---- PENDING:
# --- In getBlock3 there is missing to check for the cases in which the 3x3 block
# --- is located in the corners of the sieve matrix
getBlock3 <- function(row, col, sieve){
  TOP <- (row-2):row
  LEFT <- (col-2):col
  RIGHT <- col:(col+2)
  BOTTOM <- row:(row+2)
  SIDErow <- (row-1):(row+1)
  SIDEcol <- (col-1):(col+1)
  
  blocksGrid <- vector(mode = "list", length = 9)
  
  blocksGrid[[1]] <- sieve[ TOP, LEFT ]
  
  blocksGrid[[2]] <- sieve[ TOP, RIGHT ]
  
  blocksGrid[[3]] <- sieve[ BOTTOM, LEFT ]
  
  blocksGrid[[4]] <- sieve[ BOTTOM, RIGHT ]
  
  blocksGrid[[5]] <- sieve[ SIDErow, LEFT ]
  
  blocksGrid[[6]] <- sieve[ SIDErow, RIGHT ]
  
  blocksGrid[[7]] <- sieve[ TOP, SIDEcol ]
  
  blocksGrid[[8]] <- sieve[ BOTTOM, SIDEcol ]
  
  blocksGrid[[9]] <- sieve[ SIDErow, SIDEcol ]
  
  # blockProducts <- lapply(1:length(blocksGrid), 
  #                         function(s) as.numeric(cumprod( blocksGrid[[s]] ) ))
  
  blockProducts <- lapply(1:length(blocksGrid), 
                          function(s) as.numeric(log(cumsum( blocksGrid[[s]] ) )))
  
  
  BLOCK <- which.min(lapply(1:length(blocksGrid), 
                            function(s) blockProducts[[s]][length(blockProducts[[s]])] ))
  
  if(BLOCK == 1){
    ROWS <- TOP
    COLS <- LEFT
  }
  
  if(BLOCK == 2){
    ROWS <- TOP
    COLS <- RIGHT
  }
  
  if(BLOCK == 3){
    ROWS <- BOTTOM
    COLS <- LEFT
  }
  
  if(BLOCK == 4){
    ROWS <- BOTTOM
    COLS <- RIGHT
  }
  
  if(BLOCK == 5){
    ROWS <- SIDErow
    COLS <- LEFT
  }
  
  if(BLOCK == 6){
    ROWS <- SIDErow
    COLS <- RIGHT
  }
  
  if(BLOCK == 7){
    ROWS <- TOP
    COLS <- SIDEcol
  }
  
  if(BLOCK == 8){
    ROWS <- BOTTOM
    COLS <- SIDEcol
  }
  
  if(BLOCK==9){
    ROWS <- SIDErow
    COLS <- SIDEcol
  }
  
  list(rows = ROWS, cols = COLS,      
       block=blocksGrid[[BLOCK]],
       blockMissingness = blockProducts[[BLOCK]][length(blockProducts)])
}

getMinMaxBlock <- function(sieve, type=c("min", "max"), row, col, blockSize=2){
  
  if( !inherits(sieve, "matrix" ) ){
    stop("sieve must be a squared matrix")
  }
  
  type <- match.arg(type)
  
  if( blockSize >= 4 ){
    stop("methods not implemented for this argument value")
  }
  
  if(blockSize==2){
    if( type == "min" ){
      out <- getMinBlock2(row=row, col=col, sieve=sieve)
    } else {
      out <- getMaxBlock2(row=row, col=col, sieve=sieve)
    }
  }
  
  if(blockSize==3){
    out <- getBlock3(row=row, col=col, sieve=sieve)
  }
  
  list(rows = out$rows,
       cols = out$cols,
       block = out$block, #blocksGrid[[BLOCK]],
       blockMissingness = out$blockMissingness) # blockProducts[[BLOCK]][length(blockProducts)])
}

getLocalMinBlock <- function(sorted, sieve){
  indicesTEMP <- which(sieve == sorted, arr.ind = TRUE)
  
  minBlockMissingness <- c()
  for(j in 1:nrow(indicesTEMP)){
    
    minblockTEMP <- getMinMaxBlock(sieve = sieve,
                                   row = indicesTEMP[j,1], 
                                   col = indicesTEMP[j,2],
                                   blockSize = 2)
    
    minBlockMissingness <- c(minBlockMissingness, minblockTEMP$blockMissingness)
  }
  
  J <- which.min(minBlockMissingness)
  
  localMinBlock <- getMinMaxBlock(sieve = sieve,
                                  row = indicesTEMP[J,1], 
                                  col = indicesTEMP[J,2],
                                  blockSize = 2)
  
  localMinBlock
}

getLocalMaxBlock <- function(sorted, sieve){
  
  indicesTEMP <- which(sieve == sorted, arr.ind = TRUE)
  
  maxBlockMissingness <- c()
  for(j in 1:nrow(indicesTEMP)){
    
    maxblockTEMP <- getMinMaxBlock(sieve = sieve,
                                   type="max",
                                   row = indicesTEMP[j,1], 
                                   col = indicesTEMP[j,2],
                                   blockSize = 2)
    
    maxBlockMissingness <- c(maxBlockMissingness, maxblockTEMP$blockMissingness)
  }
  
  J <- which.max(maxBlockMissingness)
  
  localMaxBlock <- getMinMaxBlock(sieve = sieve,
                                  type="max",
                                  row = indicesTEMP[J,1], 
                                  col = indicesTEMP[J,2],
                                  blockSize = 2)
  
  localMaxBlock
} 

getGlobalMinMaxBlock <- function(sorted, sieve, type=c("min", "max")){
  
  type <- match.arg(type)
  
  out <- list()
  
  if( type == "min" ){
    globalMinBlock <- getLocalMinBlock(sorted = sorted[1], sieve = sieve)
    
    for(i in 2:length(sorted)){
      
      partialMinBlock <- getLocalMinBlock(sorted = sorted[i], sieve = sieve)
      
      if( partialMinBlock$blockMissingness < globalMinBlock$blockMissingness ){
        globalMinBlock <- partialMinBlock
      } 
      
    }
    
    out <- globalMinBlock
    
  } else {
    globalMaxBlock <- getLocalMaxBlock(sorted = sorted[1], sieve = sieve)
    
    for(i in 2:length(sorted)){
      
      partialMaxBlock <- getLocalMaxBlock(sorted = sorted[i], sieve = sieve)
      
      if( partialMaxBlock$blockMissingness > globalMaxBlock$blockMissingness ){
        globalMaxBlock <- partialMaxBlock
      } 
      
    }
    
    out <- globalMaxBlock
    
  }
  
  out
  
}

# --- REQUIRED: following functions are required by applyGapfill

# --- Added on April 28,2025

# Lists of filenames of images
# 
# An application of \code{\link[base]{list.files}}, this function returns
# a list containing ordered names of files to be processed with \code{\link[igapfill]{get_4Darray}}.
# 
# @param pattern character indicating file extension of interest
# @param     ... additional parameters to be passed to \code{\link[base]{list.files}}
#
# @importFrom gtools mixedsort
# 
# @note Outside the scope of the current package,
# we recommend to employ \code{\link[base]{list.files}} in combination with
# \code{\link[gtools]{mixedsort}} to obtain similar results.
# 
# @seealso \code{\link[igapfill]{create_dirs}}, \code{\link[igapfill]{get_4Darray}}
# 
# @return An ordered list
# 
getListFiles <- function(pattern, ...){
  myFiles <- list.files(pattern = pattern, ...)
  mixedsort(myFiles)
}


get_applyGapfill <- function(inputDir, outputDir, progressDir, 
                             lat, lon, days, years, 
                             numCores=6, scale=1e-4, 
                             clipRange=c(-1,1),
                             addArgToReport=TRUE){
  
  yearsToProcess <- list.dirs(path = inputDir)[-1]
  
  TEMP <- as.integer(sapply( 1:length(yearsToProcess),
                             function(s) {temp <- unlist(strsplit(yearsToProcess[[s]], "/"));
                             temp[length(temp)]} ))
  
  if( !base::identical(x=TEMP, y=years) ){
    text1 <- paste("Folder names in 'inputDir' must coincide with 'years' entries.", "\n")
    text2 <- "You can check out create_dirs() for a possible solution."
    # text2 <- paste("You can check out", 
    #                textColor(text="create_dirs()", color = "blue"), "for a possible solution.")
    stop(paste(text1, text2))
  }
  
  totalListFILES <- vector(mode="list", length=length(yearsToProcess))
  for(i in 1:length(yearsToProcess)){
    totalListFILES[[i]] <- getListFiles(pattern = ".tif", 
                                        path=yearsToProcess[i], 
                                        full.names=TRUE)
  }
  
  layersToProcess <- length(totalListFILES[[1]])
  
  cluster <- parallel::makeCluster(numCores, outfile="/dev/null")
  registerDoParallel(cluster)
  name_RData_output <- paste0( outputDir, "/gapfill_output_cell_" )
  
  logFILE <- paste0(progressDir, "/gapfill_progress.txt")
  
  write("===============================",
        file = logFILE, append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = logFILE, append = TRUE)
  write("===============================",
        file = logFILE, append = TRUE)
  
  output <- foreach( i = 1:layersToProcess,
                     .export = c("get_3Darray", "get_array_lat_lon_day_year", "get_4Darray"),
                     .packages = c("gapfill", "raster") ) %dopar% {
                       
                       ARRAY_LAT_LONG_DAYS_YEARS <- get_4Darray(listPath = totalListFILES, i = i,
                                                                lat = lat, lon = lon,
                                                                days = days, years = years)
                       
                       output_gapFill <- Gapfill(data = ARRAY_LAT_LONG_DAYS_YEARS * scale,
                                                 clipRange = clipRange)
                       
                       save(output_gapFill, file = paste0(name_RData_output, i, ".RData"))
                       if(i %% numCores == 0){
                         text <- paste0("Working on cell ", i)
                         write(text, file = logFILE, append = TRUE)
                       }
                     }
  stopCluster(cluster)
  write("===============================",
        file = logFILE, append = TRUE)
  write(paste0("Ended at: ", as.character(Sys.time()[1])),
        file = logFILE, append = TRUE)
  write("===============================",
        file = logFILE, append = TRUE)
  
  if(addArgToReport){
    write("==========",
          file = logFILE, append = TRUE)
    TEXT <- "Arguments: "
    write(TEXT,
          file = logFILE, append = TRUE)
    write("==========",
          file = logFILE, append = TRUE)
    
    write(paste0("Input directory: ", inputDir),
          file = logFILE, append = TRUE)
    write(paste0("Output directory: ", outputDir),
          file = logFILE, append = TRUE)
    write(paste0("Progress Report directory: ", progressDir),
          file = logFILE, append = TRUE)
    write(paste0("lat (as vector): first & last entries: ", lat[1], 
                 ", ", lat[length(lat)], 
                 "; length equal to ", length(lat)),
          file = logFILE, append = TRUE)
    write(paste0("lon (as vector): first & last entries: ", lon[1], 
                 ", ", lon[length(lon)],
                 "; length equal to ", length(lon)),
          file = logFILE, append = TRUE)
    write(paste0("days (as vector, only first day of each year): first entry ", days[1], 
                 ", last entry ", days[length(days)],
                 ", length equal to ", length(days)),
          file = logFILE, append = TRUE)
    write(paste0("years: ", years[1], ":", years[length(years)]),
          file = logFILE, append = TRUE)
    write(paste0("Number of cores in used: ", numCores),
          file = logFILE, append = TRUE)
    write(paste0("Scale factor: ", scale),
          file = logFILE, append = TRUE)
    write(paste0("Clip range: (", clipRange[1], ",", clipRange[2], ")"),
          file = logFILE, append = TRUE)
  }
  
  message( colorText("Done, check output at ", 216), 
           colorText(outputDir, 159) )
  
}


# REQUIRED: following functions are required by parallel_mosaic

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  env
}

# --- Added on November 13, 2023
# --- Full modification on December 10, 2024: Erased old comments
# --- Update on April 23, 2025

getMosaicList <- function(i,j,imagesToProcess,
                          RDataToProcess, masterToProcess,
                          progressReportDir, numCores){
  
  # Modified on Nov 28, 2024
  nameOriginalImage <- basename(imagesToProcess[sqrt(length(imagesToProcess)) * (i-1) + j])
  nameOriginalImage <- strsplit(nameOriginalImage, ".tif")[[1]][1]
  # logFILE <- paste0( progressReportDir, "/mosaicList_progress_",
  #                    names(originalImage), ".txt" )
  logFILE <- paste0( progressReportDir, "/mosaicList_progress_",
                     nameOriginalImage, ".txt" )
  
  message(colorText("Working on file ", 220), colorText(nameOriginalImage, 159))
  message(colorText("More details in", 220), colorText(logFILE, 159))
  
  write("===============================",
        file = logFILE,
        append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = logFILE,
        append = TRUE)
  write("===============================",
        file = logFILE,
        append = TRUE)
  
  
  write("-------------------------",
        file = logFILE,
        append = TRUE)
  write(paste0("Working on file ", nameOriginalImage),
        file = logFILE,
        append = TRUE)
  write("-------------------------",
        file = logFILE,
        append = TRUE)
  
  cluster <- parallel::makeCluster(numCores, outfile = "/dev/null")
  registerDoParallel(cluster)
  
  rasterList <- foreach(k = 1:length(RDataToProcess),
                        .export = c("LoadToEnvironment"),
                        .packages = c("raster","geoTS"))%dopar%{
                          
                          cell <- LoadToEnvironment(RData=RDataToProcess[k])
                          master <- raster(masterToProcess[k])
                          
                          master[is.na(master)] <- 1L
                          
                          TEMP <- matrixToRaster(matrix=cell$output_gapFill$fill[,,j,i],
                                                 raster=master)
                          
                          if(k %% 100 == 0){
                            text <- paste0("Working on cell ", k)
                            write(text,
                                  file = logFILE,
                                  append = TRUE)
                          }
                          
                          return(TEMP)
                        }
  stopCluster(cluster)
  
  write("===============================",
        file = logFILE,
        append = TRUE)
  write(paste0("Ended at: ", as.character(Sys.time()[1])),
        file = logFILE,
        append = TRUE)
  write("===============================",
        file = logFILE,
        append = TRUE)
  
  rasterList
}

globalVariables('k')

globalVariables('i')


# Modified on Nov 28, 2024
# names(original) was replaced by nameOriginalRaster

# Modified on Dec 10, 2024
# issue when raster has odd nrow or ncol, in this case mosaicking is done with
# a simple for()

# Modified on April 2025
# included used of colorText()
doMosaicking <- function(rlist, numCores=23,
                         originalRaster,
                         outputDir,
                         progressReportDir,
                         scaleFactor=1e+4,
                         dataType){
  
  message(colorText("Mosaicking continues ...", 82))
  original <- raster(originalRaster)
  nameOriginalRaster <- basename(originalRaster)
  nameOriginalRaster <- strsplit(nameOriginalRaster, ".tif")[[1]][1]
  
  res <- length(rlist) %% numCores
  if( (length(rlist)-res) / numCores %% 2 == 0 ){
    progressReportDirTemp <- paste0( progressReportDir, "/temp_", nameOriginalRaster )
    dir.create(progressReportDirTemp)
    
    dirOutput <- paste0( outputDir, "/temp_", nameOriginalRaster )
    dir.create(dirOutput)
  }
  
  if(length(rlist) > 1) {
    
    if( (length(rlist)-res) / numCores %% 2 == 0 ){
      
      blocks <- seq(0, length(rlist),
                    by = (length(rlist)-res) / numCores)
      leftCell <- blocks + 1
      rightCell <- c(blocks[-1], length(rlist))
      
      cluster <- parallel::makeCluster(numCores, outfile = "/dev/null")
      registerDoParallel(cluster)
      done <- foreach(k=isplitVector(x=rlist, chunkSize=(length(rlist)-res) / numCores),
                      i=icount(), #1:length(leftCell),
                      .export = "cellsMosaicking",
                      .packages = "raster") %dopar% {
                        cellsMosaicking(rasterList = k,
                                        cellNames = c(leftCell[i], rightCell[i]),
                                        dataType = dataType,
                                        saveName = paste0(dirOutput, "/",
                                                          nameOriginalRaster),
                                        scaleFactor=scaleFactor,
                                        progressReportDir = progressReportDirTemp)
                      }
      
      stopCluster(cluster)
      
      cellsTIF <- mixedsort(list.files(path = dirOutput,
                                       pattern = ".tif",
                                       full.names = TRUE))
      
      finalMOSAIC <- raster(cellsTIF[1])
      for(j in 2:length(cellsTIF)){
        AUX <- raster::raster(cellsTIF[j])
        finalMOSAIC <- raster::mosaic(x=finalMOSAIC, y = AUX,
                              fun=mean)
      }
      
    } else {
      
      finalMOSAIC <- rlist[[1]] * scaleFactor
      for(j in 2:length(rlist)){
        AUX <- rlist[[j]] * scaleFactor
        finalMOSAIC <- raster::mosaic(x=finalMOSAIC, y = AUX, fun = mean)
      }
      
    }
    
  } else {
    finalMOSAIC <- rlist[[1]] * scaleFactor
  }
  
  message( colorText("Saving ", 220), 
           colorText(nameOriginalRaster, 159) )
  
  raster::writeRaster(finalMOSAIC,
              filename = paste0(outputDir, "/", nameOriginalRaster),
              format = "GTiff",
              datatype= dataType(original),
              overwrite = TRUE)
  
}

cellsMosaicking <- function(rasterList, cellNames,
                            dataType, saveName, scaleFactor=1e4,
                            progressReportDir){
  
  fileNAME <- paste0( progressReportDir, "/cellsMosaicking_cells_",
                      cellNames[1], "_", cellNames[2], "_progress.txt")
  
  write("===============================",
        file = fileNAME,
        append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = fileNAME,
        append = TRUE)
  write("===============================",
        file = fileNAME,
        append = TRUE)
  
  final_mosaic <- rasterList[[1]] * scaleFactor
  
  for(i in 2:length(rasterList)){
    
    if(i %% 100 == 0){
      text <- paste0("Working on cell ", i)
      write(text,
            file = fileNAME,
            append = TRUE)
    }
    
    aux <- rasterList[[i]] * scaleFactor
    final_mosaic <- raster::mosaic(x=final_mosaic, y=aux, fun=mean)
  }
  
  write("-------------------------------",
        file=fileNAME,
        append = TRUE)
  TXT <- "   Saving partial mosaic..."
  write(TXT,
        file = fileNAME,
        append = TRUE)
  write("-------------------------------",
        file = fileNAME,
        append = TRUE)
  
  writeRaster(final_mosaic,
              filename = paste0(saveName, "_cells_",
                                cellNames[1], "_", cellNames[2]),
              format="GTiff",
              datatype=dataType,
              overwrite=TRUE)
  
  write("===============================",
        file = fileNAME,
        append = TRUE)
  write(paste0("Ended at: ", as.character(Sys.time()[1])),
        file = fileNAME,
        append = TRUE)
  write("===============================",
        file = fileNAME,
        append = TRUE)
}

# --- REQUIRED: following functions are required by sort_split

justColorText <- function(text, foreground=1, background){
  
  out <- "\033[38;5;"
  
  if(missing(background)){
    out <- paste0(out, foreground)
  } else {
    out <- paste0(out, foreground, ";48;5;", background)
  }
  
  out <- paste0(out, ";m", text, "\033[0m")
  
  out
}


# --- Added on Abril 15, 2025

sort_split_core <- function(path, startYear, endYear, nrow_split, ncol_split){
  
  yearsToFill <- startYear:endYear
  
  pathFILES <- mixedsort(list.files(path=path,
                                    pattern = ".tif",
                                    full.names = TRUE))
  
  stackLIST <- list()
  p <- length(yearsToFill)
  
  for(k in 1:p){
    stackLIST[[k]] <- rast( pathFILES[1:p + (k-1)*p] )
  }
  
  v <- nrow_split 
  h <- ncol_split 
  
  colCELL <- terra::ncol(stackLIST[[1]])/h
  rowCELL <- terra::nrow(stackLIST[[1]])/v
  
  output_dir <- paste0(path, "/gapfill/splits")
  output_dir_master <- paste0(path, "/gapfill/master")
  
  # message("Splitting is on:")
  message( colorText("Splitting is on", 118) )
  
  outputPath_split <- list.dirs(path=output_dir)[-1]
  for(k in 1:p){
    name_split <- yearsToFill[k] # readline(k_text)
    split_replace_terra(raster=stackLIST[[k]],
                        h=colCELL,
                        v=rowCELL,
                        outputPath=outputPath_split[k],
                        name=name_split,
                        dataType = terra::datatype(stackLIST[[1]])[1])
  }
  
  message( colorText("Next, splitting aux master", 118) )
  
  MASTER <- rast(pathFILES[1])
  MASTER[is.na(MASTER)] <- 1L
  split_replace_terra(raster=MASTER, 
                      h=colCELL, v=rowCELL,
                      outputPath=output_dir_master,
                      name = "master", 
                      dataType="INT2S")
  
}

split_replace_terra <- function(raster, partPerSide, h, v, outputPath, 
                                name, save = TRUE, replace = FALSE, 
                                valToReplace, replacedBy, 
                                fileType = "GTiff", dataType,
                                parallelProcessing = FALSE,
                                numCores = 20,
                                cellsToProcess, ...){
  
  if(missing(raster)){
    stop("raster must be provided")
  }
  
  if(missing(outputPath)){
    stop("outputPath must be provided")
  }
  
  if(missing(name)){
    stop("name must be provided")
  }
  
  if(replace){
    if(missing(valToReplace) | missing(replacedBy)){
      stop("When replace = TRUE, valToReplace and replacedBy must be specified")
    }
  }
  
  if(missing(partPerSide)){
    if(missing(h) | missing(v)){
      stop("h and v must be provided")
    } else {
      if(missing(cellsToProcess)){
        cellsToProcess <- 1:( terra::ncell( raster ) /(h*v) )
      }
    }
  } else {
    h <- ceiling(terra::ncol(raster)/partPerSide)
    
    v <- ceiling(terra::nrow(raster)/partPerSide)
    
    if(missing(cellsToProcess)){
      cellsToProcess <- 1:(partPerSide^2)
    }
  }
  
  if(missing(dataType)){
    dataType <- terra::datatype(raster)[1]
  }
  
  agg <- getAggregate_terra(spRaster = raster, 
                            h = h, v = v)
  
  agg[] <- 1:terra::ncell(agg)
  
  agg_poly <- terra::as.polygons(agg) #rasterToPolygons(agg)
  
  names(agg_poly) <- "polis"
  
  start_message <- paste0("Started at: ", as.character(Sys.time()[1]))
  message( colorText(start_message, 220) )
  
  pb <- txtProgressBar(min = 0, max = length(cellsToProcess), style = 3, 
                       char = '\033[38;5;159;m=\033[0m' ) # justColorText("=", 159)
  
  for(i in cellsToProcess){
    Sys.sleep(0.1)
    
    extent_polygon <- terra::ext(agg_poly[agg_poly$polis == i,])
    
    # --- create temp dir
    dir.create(path = paste0(outputPath, "/temp_", name, "_", i),
               showWarnings = F)
    raster::rasterOptions(tmpdir = paste0(outputPath, "/temp_", name, "_", i))
    # ---
    
    temp_r <- rast()  
    
    if( parallelProcessing ){
      
      output <- parallel_crop(raster = raster, numCores = numCores, 
                              polygon_extent = extent_polygon)
      
      for(k in 1:nlyr(raster)){
        add(temp_r) <- output[[k]]
        # temp_r <- addLayer(temp_r, )
      }
      
    } else {
      for(k in 1:nlyr(raster)){
        aux_r <- terra::crop(raster[[k]], extent_polygon)
        add(temp_r) <- aux_r
      }
    }
    
    # if(replace == TRUE){
    #   temp_r <- reclassify( temp_r, cbind( valToReplace, replacedBy ) )
    # }
    
    if(save){
      terra::writeRaster(x=temp_r, 
                         filename = paste0(outputPath, "/", name, "_", i, ".tif"),
                         filetype = fileType, datatype = dataType, 
                         overwrite = TRUE, ...)  
    }
    
    unlink(paste0(outputPath, "/temp_", name, "_", i), recursive = TRUE)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  fin_message <- paste0("Finished at: ", as.character(Sys.time()[1]))
  message( colorText(fin_message, 220) )
  
}

# --- Added on Aug 29, 2023

getAggregate_terra <- function(spRaster, h = 1, v = 1){
  
  if( terra::nlyr(spRaster) > 1 ){
    spRaster <- terra::subset(spRaster,1)
  }
  
  agg <- terra::aggregate(x=spRaster, 
                          fact=c(v,h))
  
  agg
}

# ---

globalVariables("j")
parallel_crop <- function(raster, numCores, polygon_extent){
  
  closter <- parallel::makeCluster(spec = numCores,
                                   outfile = "")
  doParallel::registerDoParallel(closter)
  
  output <- foreach::foreach(j = 1:nlyr(raster), .packages = c("terra")) %dopar% {
    s <- terra::crop(raster[[j]], polygon_extent)
    return(s)
  }
  
  stopCluster(closter)
  
  output
}

print_years_array <- function(p, years){
  what_to_print <- sapply(1:p^2, function(s) 
    paste0("(", s, ")"))
  
  for(k in 1:p){
    cat(colorText(paste0(years[k], " STACK_", k, ": "), 159),
        colorText(paste0(what_to_print[1:p + (k-1)*p]), 159), "\n")
  }
  
}






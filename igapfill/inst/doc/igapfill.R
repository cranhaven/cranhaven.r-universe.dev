## ----setup, include = FALSE---------------------------------------------------
library(terra)
library(igapfill)
library(heatmaply)
library(htmltools)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# WD <- "/home/series_tiempo/Escritorio/Rtest/igapfill/PNG"

## ----datasetUploading---------------------------------------------------------
ndvi_path <- system.file("extdata", "garnica_250m_16_days_NDVI.tif", 
                         package = "igapfill")
reliability_path <- system.file("extdata", 
                                "garnica_250m_16_days_pixel_reliability.tif", 
                                package = "igapfill")

spatNDVI <- rast(ndvi_path)
spatRELIABILITY <- rast(reliability_path)

## ----mSieve-prelim, echo=FALSE------------------------------------------------
garnicaNDVIQAdir <- paste0( getwd(), "/garnica/250m_16_days_NDVI_QA" )

dir.create( path = garnicaNDVIQAdir, recursive = TRUE )

YEARS <- 2000:2024
ndviNAMES <- names(spatNDVI)
reliabilityNAMES <- names(spatRELIABILITY)
ndviQANAMES <- sapply(ndviNAMES, 
                      function(s) paste0( strsplit(s, ".tif")[[1]][1], "_QA.tif" ))

for(i in 1:20){
  
  if(!dir.exists( paste0( garnicaNDVIQAdir, "/", YEARS[1] ) )){
    dir.create( paste0( garnicaNDVIQAdir, "/", YEARS[1] ) )
  }
  
  TEMPndvi <- subset(spatNDVI,i)
  
  TEMPreliability <- subset(spatRELIABILITY,i)
  
  TEMPndvi[ TEMPreliability >= 2] <- NA
  writeRaster(TEMPndvi, 
              filename = paste0( garnicaNDVIQAdir, "/", YEARS[1], "/",
                                 ndviQANAMES[i] ),
              datatype = "INT2S", overwrite = TRUE)
}

for (i in 2:length(YEARS)) {
  
  if(!dir.exists( paste0( garnicaNDVIQAdir, "/", YEARS[i] ) )){
    dir.create( paste0( garnicaNDVIQAdir, "/", YEARS[i] ) )
  }
  
  for(j in 1:23){
    count <- 20 + ((i-2)*23) + j
    TEMPndvi <- subset(spatNDVI,  count)
    TEMPreliability <- subset(spatRELIABILITY,count)
    TEMPndvi[ TEMPreliability >= 2 ] <- NA
    
    writeRaster(TEMPndvi, 
                filename = paste0( garnicaNDVIQAdir, "/", YEARS[i], "/",
                                   ndviQANAMES[count] ),
                datatype = "INT2S", overwrite = TRUE)
  }
}

## ----mSieve-usage, warning=FALSE, message=FALSE-------------------------------
garnicaNDVIQAdir <- paste0( getwd(), "/garnica/250m_16_days_NDVI_QA" )
dir.create( path = garnicaNDVIQAdir, recursive = TRUE )

DIRS <- list.dirs(path = garnicaNDVIQAdir)[-1]

COLNAMES <- paste0( "DoY-", seq(1,365,by=16) )

missingValueSieve <- mvSieve(dirs=DIRS[-1],
                             filesPerDir = 23,
                             startPeriod = 2001,
                             endPeriod = 2024,
                             colNames = COLNAMES)

## ----mSieve-heatmaply, echo=FALSE, fig.align='center'-------------------------
out <- heatmaply((missingValueSieve/max(missingValueSieve)) * 100,
          limits = c(0,100),
          colors = cool_warm,
          dendrogram = "none",
          xlab = "", ylab = "",
          width = 900,
          height = 650,
          main = "% of missing values in NDVI TS of Cerro de Garnica (2001-2024)",
          scale = "none",
          draw_cellnote=TRUE,
          cellnote_textposition = "middle center",
          cellnote_size = 10,
          margins = c(60, 100, 40, 20),
          titleX = FALSE,
          hide_colorbar = TRUE,
          labRow = rownames(missingValueSieve),
          labCol = colnames(missingValueSieve),
          heatmap_layers = theme(axis.line = element_blank()))

html_out <- as.tags(out)

scrollable_out <- tags$div(
  class = "scrollable",
  style = "width: 100%;",
  html_out
)

scrollable_out

## ----minmaxBlock--------------------------------------------------------------
SIEVE <- (missingValueSieve/max(missingValueSieve)) * 100

minmaxBlock(sieve=SIEVE, rank=10)

## ----block0-------------------------------------------------------------------
DIRS <- list.dirs(path = garnicaNDVIQAdir)[-1]

files2011 <- list.files(path = DIRS[12],
                        pattern = ".tif$",
                        full.names = TRUE)

files2012 <- list.files(path = DIRS[13],
                        pattern = ".tif$",
                        full.names = TRUE)

block0DIR <- paste0( getwd(), "/garnica/block0" )

if( !dir.exists(block0DIR) ){
  dir.create( block0DIR )
}

file.copy(from=files2011[8:9], to=block0DIR)

file.copy(from=files2012[8:9], to=block0DIR)

## ----create-dirs, message=FALSE, echo=FALSE-----------------------------------
# knitr::include_graphics(normalizePath(paste0(WD, "/create_dirs.png")))
knitr::include_graphics("figs/create_dirs.png")

## ----dirTREE, echo=FALSE------------------------------------------------------
# knitr::include_graphics(normalizePath(paste0(WD, "/garnica_block0_directoryTree.png")))
knitr::include_graphics("figs/garnica_block0_directoryTree.png")

## ----dimsReport, echo=FALSE---------------------------------------------------
# knitr::include_graphics(normalizePath(paste0(WD, "/dimsReport.png")))
knitr::include_graphics("figs/dimsReport.png")

## ----dimsReport-ignore, include=FALSE, eval=FALSE-----------------------------
# knitr::include_graphics(normalizePath(paste0(WD, "/dimsReport.png")))
# 
# html_out <- as.tags(out)
# 
# scrollable_out <- tags$div(
#   class = "scrollable",
#   style = "width: 100%;",
#   html_out
# )
# 
# scrollable_out

## ----sort_split, include=FALSE, eval=FALSE------------------------------------
# # knitr::include_graphics(normalizePath(paste0(WD, "/sort_split_3.png")))

## ----applyGapfill-aux, echo=FALSE, eval=FALSE---------------------------------
# block0DIR <- "~/garnica/block0" # made a copy in /home

## ----applyGapfill, eval=FALSE-------------------------------------------------
# allDIRS <- list.dirs(path = block0DIR, full.names = TRUE)#[-1]
# 
# block0FILES <- list.files(path = block0DIR, pattern = ".tif$",
#                           full.names = TRUE)
# 
# LAT <- get_LAT(stack = stack(block0FILES))
# 
# LON <- get_LON(stack = stack(block0FILES))
# 
# applyGapfill(inputDir = allDIRS[7],
#              outputDir = allDIRS[5],
#              progressDir = allDIRS[6],
#              lat = LAT,
#              lon = LON,
#              days = c(113,129),
#              years = 2011:2012,
#              numCores = 10,
#              scale = 1e0,
#              clipRange = c(-1e4,1e4))

## ----parallel-mosaic, eval=FALSE----------------------------------------------
# parallel_mosaic(inputDirImages = block0DIR,
#                 inputDirRData = allDIRS[5],
#                 inputDirMaster = allDIRS[4],
#                 outputDir = allDIRS[3],
#                 progressReportDir = allDIRS[6],
#                 scaleFactor = 1e0,
#                 dataType = "INT4S",
#                 numCores = 3) # numCores must not exceed number of splits

## ----before-igapfill, eval=FALSE----------------------------------------------
# initFILES <- list.files(path = block0DIR,
#                         pattern = ".tif$",
#                         full.names = TRUE)
# 
# gapfilledFILES <- list.files(path = allDIRS[3],
#                              pattern = ".tif$",
#                              full.names = TRUE)
# 
# initIMAGES <- rast(initFILES)
# gapfilledIMAGES <- rast(gapfilledFILES)
# 
# par(mfrow=c(2,2))
# plot(initIMAGES, cex.main=0.75,
#      main=sapply(c("Day113, 2011", "Day129, 2011", "Day113, 2012", "Day129, 2012"),
#                  function(s) paste("Original:", s)))
# 

## ----after-igapfill, eval=FALSE-----------------------------------------------
# plot(gapfilledIMAGES, cex.main=0.75,
#      main=sapply(c("Day113, 2011", "Day129, 2011", "Day113, 2012", "Day129, 2012"),
#                  function(s) paste("Gapfill:", s)))

## ----mSieve-usage-Appendix, eval=FALSE----------------------------------------
# # --- Auxiliary code used in mvSieve section ---
# 
# garnicaNDVIQAdir <- paste0( getwd(), "/garnica/250m_16_days_NDVI_QA" )
# 
# dir.create( path = garnicaNDVIQAdir, recursive = TRUE )
# 
# YEARS <- 2000:2024
# ndviNAMES <- names(spatNDVI)
# reliabilityNAMES <- names(spatRELIABILITY)
# ndviQANAMES <- sapply(ndviNAMES,
#                       function(s) paste0( strsplit(s, ".tif")[[1]][1], "_QA.tif" ))
# 
# for(i in 1:20){ #### Year 2000 has 20 images only
# 
#   if(!dir.exists( paste0( garnicaNDVIQAdir, "/", YEARS[1] ) )){
#     dir.create( paste0( garnicaNDVIQAdir, "/", YEARS[1] ) )
#   }
# 
#   TEMPndvi <- subset(spatNDVI,i)
# 
#   TEMPreliability <- subset(spatRELIABILITY,i)
# 
#   TEMPndvi[ TEMPreliability >= 2] <- NA
#   writeRaster(TEMPndvi,
#               filename = paste0( garnicaNDVIQAdir, "/", YEARS[1], "/",
#                                  ndviQANAMES[i] ),
#               datatype = "INT2S", overwrite = TRUE)
# }
# 
# for (i in 2:length(YEARS)) {
# 
#   if(!dir.exists( paste0( garnicaNDVIQAdir, "/", YEARS[i] ) )){
#     dir.create( paste0( garnicaNDVIQAdir, "/", YEARS[i] ) )
#   }
# 
#   for(j in 1:23){
#     count <- 20 + ((i-2)*23) + j
#     TEMPndvi <- subset(spatNDVI,  count)
#     TEMPreliability <- subset(spatRELIABILITY,count)
#     TEMPndvi[ TEMPreliability >= 2 ] <- NA
# 
#     writeRaster(TEMPndvi,
#                 filename = paste0( garnicaNDVIQAdir, "/", YEARS[i], "/",
#                                    ndviQANAMES[count] ),
#                 datatype = "INT2S", overwrite = TRUE)
#   }
# }

## ----mSieve-heatmaply-Appendix, eval=FALSE------------------------------------
# # --- Auxiliary code used in mSieve section ---
# out <- heatmaply((missingValueSieve/max(missingValueSieve)) * 100,
#           limits = c(0,100),
#           colors = cool_warm,
#           dendrogram = "none",
#           xlab = "", ylab = "",
#           width = 900,
#           height = 650,
#           main = "% of missing values in NDVI TS of Cerro de Garnica (2001-2024)",
#           scale = "none",
#           draw_cellnote=TRUE,
#           cellnote_textposition = "middle center",
#           cellnote_size = 6,
#           margins = c(60, 100, 40, 20),
#           titleX = FALSE,
#           hide_colorbar = TRUE,
#           labRow = rownames(missingValueSieve),
#           labCol = colnames(missingValueSieve),
#           heatmap_layers = theme(axis.line = element_blank()))
# 
# html_out <- as.tags(out)
# 
# scrollable_out <- tags$div(
#   class = "scrollable",
#   style = "width: 100%;",
#   html_out
# )
# 
# scrollable_out


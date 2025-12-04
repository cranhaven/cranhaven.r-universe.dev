#' Mosaic from gapfilled objects
#' 
#' Rasterizes the output of \code{\link[igapfill]{applyGapfill}} and
#' the resulting raster objects are glued together in the manner of a mosaic. 
#' 
#' This function may be useful when employing \code{\link[gapfill]{Gapfill}}
#' independently of the current package.
#' 
#' @param    inputDirImages character. Full path name of directory containing files to be 
#'                          gap-filled. 
#' @param     inputDirRData character. Full path name of directory containing \code{RData} files 
#'                          obtained as the result of using \code{\link[gapfill]{Gapfill}}.
#'                          See \bold{Note}.
#' @param    inputDirMaster character. Full path name of directory containing splits of a
#'                          \emph{master} file. See \bold{Details}.
#' @param         outputDir character. Full path name of directory where output will be saved.
#'                          See \bold{Note}.
#' @param progressReportDir character. Full path name of directory where a file reporting on the
#'                          See \bold{Note}.
#' @param          numCores numeric. How many cores should be employed in parallel computing?
#' @param       scaleFactor integer. Default is \code{1e4}. See \bold{Note}.
#' @param          dataType character. See \code{\link[raster]{writeRaster}} for further details.
#'                          Default is \code{INT4S}. See \bold{Note}.
#' 
#' @export
#' 
#' @importFrom gtools mixedsort
#' @importFrom raster raster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom geoTS matrixToRaster
#' @importFrom parallel stopCluster
#' @importFrom itertools isplitVector
#' @importFrom iterators icount
#' @importFrom raster mosaic
#' @importFrom raster writeRaster
#' 
#' @note Within the workflow of this package, \code{inputDirRData},
#' \code{inputDirMaster}, \code{outputDir} and \code{progressReportDir} must be equal to the sub-directories 
#' \emph{/output}, \emph{/master}, \emph{/filled}, and \emph{/progressReports}, respectively. These
#' folders can be created by \code{\link[igapfill]{create_dirs}}. Many satellite products
#' come with a scale factor of \code{1e4} and are distributed in formats equivalent to \code{INT4S}.
#' After \code{\link[igapfill]{applyGapfill}} the objects to rasterize/mosaic must be scaled back,
#' therefore the default values for arguments \code{scaleFactor} and \code{dataType}.
#' 
#' @details The term \emph{master} refers to a raster with no missing values
#' and whose coordinate reference system is used to rasterize objects such as matrices.
#' 
#' @seealso \code{\link[raster]{mosaic}}, \code{\link[igapfill]{applyGapfill}}.
#' 
#' @return At \code{outputDir} the user will find \code{n} \emph{Gtiff}
#' files, where \code{n} is equal to the number of files in \code{inputDirImages}.
#' 
parallel_mosaic <- function(inputDirImages,
                            inputDirRData,
                            inputDirMaster,
                            outputDir, 
                            progressReportDir,
                            numCores,
                            scaleFactor=1e+4,
                            dataType="INT4S"){

  message( colorText("Loading arguments ", 220),
           colorText("(original TIF files, RData files, etc.)", 80) )
  
  message( colorText("More details in ", 220), 
           colorText("progressReportDir", 159) )
  
  imagesToProcess <- mixedsort(list.files(path=inputDirImages,
                                          pattern = ".tif",
                                          full.names = TRUE))
  
  RDataToProcess <- mixedsort(list.files(path = inputDirRData,
                                         pattern = ".RData",
                                         full.names = TRUE))
  
  masterToProcess <- mixedsort(list.files(path = inputDirMaster,
                                          pattern = ".tif",
                                          full.names = TRUE))
  
  totalListFILES <- base::sqrt(length(imagesToProcess))
  
  for(i in 1:totalListFILES){
    for(j in 1:totalListFILES){
      listRaster <- getMosaicList(i=i,j=j,
                                  imagesToProcess = imagesToProcess,
                                  RDataToProcess = RDataToProcess,
                                  masterToProcess = masterToProcess,
                                  progressReportDir = progressReportDir,
                                  numCores = numCores)
      
      doMosaicking(rlist=listRaster, numCores=numCores,
                   originalRaster=imagesToProcess[totalListFILES*(i-1)+j],
                   outputDir=outputDir,
                   progressReportDir=progressReportDir, scaleFactor=scaleFactor,
                   dataType=dataType)
    }
  }
  
  message(colorText("Done, check output at ", 216), 
          colorText(outputDir, 159))
  
}

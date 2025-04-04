#' @title Download GEO object
#' @description This function downloads the GEO object using getGEO function.
#' This function is used internally by downloadGEO.
#' @param GEOID The GEO dataset under query, ex. GSE14762.
#' @param platform The platform of selected GEO dataset, ex. GPL4866.
#' @param destDir The user path to save the downloaded files.
#' @return A GEO object with class ExpressionSet.
#' @details This function is used internally by downloadGEO.
#' @importFrom Biobase annotation
#' @importFrom GEOquery getGEO
#' @noRd
.downloadGEOObject <- function(GEOID, platform, destDir) {
  
  if (!dir.exists(destDir)) {
    stop("The destination directory does not exist.")
  }
  
  oldTimeout <- options("timeout")
  on.exit({options(timeout = oldTimeout)})
  options(timeout = 3600)
  
  gsets <- getGEO(GEOID, GSEMatrix = TRUE, getGPL = TRUE, destdir = destDir)
  platforms <- sapply(gsets, annotation)
  
  if (!platform %in% platforms) {
    stop("The platform is not available in the GEO dataset.")
  }
  
  gsets[[which(platforms == platform)]]
}

#' @title Get Supplemental Files from GEO
#' @description Download supplemental files to be attached to GEO Series (GSE), GEO platforms (GPL), and GEO samples (GSM).
#' This function is used internally by .downloadSamples.
#' @param ... A list of arguments passed to getGEOSuppFiles. See getGEOSuppFiles function.
#' @return A dataframe returned by getGEOSuppFiles
#' @details This function is used internally by .downloadSamples.
#' @importFrom GEOquery getGEOSuppFiles
#' @noRd
.getGEOSuppFiles <- function(...){
  res <- try({getGEOSuppFiles(...)}, silent = TRUE)
  if (inherits(res, "try-error")) {
    warning("The data source is temporarily unvailable. Please try it again later or contact the maintainer(s) to solve this issue.")
    rlang::interrupt()
  } else {
    return(res)
  }
}

#' @title Download samples from GEO object
#' @description This function downloads the corresponding samples from queried GEO object. This function is only used if the protocol is affymetrix or agilent.
#' This function is used internally by downloadGEO.
#' @param sampleIDs A vector of GEO accession sample IDs to be downloaded.
#' @param destDir The user path to save the downloaded files.
#' @return TRUE
#' @details This function is used internally by downloadGEO.
#' @importFrom GEOquery getGEOSuppFiles
#' @importFrom dplyr %>%
#' @importFrom utils URLdecode
#' @importFrom httr HEAD
#' @noRd
.downloadSamples <- function(sampleIDs, protocol = c("affymetrix", "agilent"), destDir) {
  
  if (!dir.exists(destDir)) {
    stop("The destination directory does not exist.")
  }
  
  protocol <- match.arg(protocol)
  
  for (id in sampleIDs) {
    
    if (protocol == "affymetrix") {
      
      if (file.exists(file.path(destDir, paste0(id, ".CEL.gz")))) next()
      
      filesURL <- .getGEOSuppFiles(id, baseDir = destDir, makeDirectory = FALSE, fetch_files = FALSE)
      
      actualInfo <- lapply(1:nrow(filesURL), function(curRow) {
        actualFileLength <- httr::HEAD(filesURL[curRow, 2])$
          headers$
          `content-length`
        data.frame(
          fname = filesURL[curRow, 1],
          fsize = actualFileLength,
          stringsAsFactors = FALSE
        )
      }) %>% do.call(what = rbind)
      
      downloadedInfo <- .getGEOSuppFiles(id, baseDir = destDir, makeDirectory = FALSE)
      downloadedFiles <- downloadedInfo %>% rownames()
      
      isDeletedFlag <- FALSE
      for (i in 1:2) {
        lapply(nrow(downloadedInfo), function(curRow) {
          if (downloadedInfo[curRow, "size"] != actualInfo[curRow, "fsize"]) {
            file.remove(downloadedFiles[curRow])
            isDeletedFlag <- TRUE
          }
        })
        
        if (i == 2 & isDeletedFlag == TRUE) {
          stop(paste0("There is an error in downloading sample ", id))
        }else if (isDeletedFlag) {
          downloadedInfo <- .getGEOSuppFiles(id, baseDir = destDir, makeDirectory = FALSE)
          isDeletedFlag <- FALSE
        }else break
      }
      
      #downloadedFiles <- getGEOSuppFiles(id, baseDir = destDir, makeDirectory = FALSE) %>% rownames()
      
      if (is.null(downloadedFiles)) {
        stop("Check the specified samples IDs to be valid. No file is found.")
      }
      
      downloadedFiles <- sapply(downloadedFiles, function(fileName) {
        if (!file.exists(fileName)) {
          URLdecode(fileName)
        } else {
          fileName
        }
      }) %>% as.vector()
      
      downloadedFiles[grep(".cel.gz", downloadedFiles, ignore.case = TRUE)] %>% file.rename(paste0(destDir, "/", id, ".CEL.gz"))
      
    }else {
      
      if (file.exists(file.path(destDir, paste0(id, ".TXT.gz")))) next()
      
      downloadedFiles <- .getGEOSuppFiles(id, baseDir = destDir, makeDirectory = FALSE) %>% rownames()
      
      if (is.null(downloadedFiles)) {
        stop("Check the specified samples IDs to be valid. No file is found.")
      }
      
      downloadedFiles <- sapply(downloadedFiles, function(fileName) {
        if (!file.exists(fileName)) {
          URLdecode(fileName)
        } else {
          fileName
        }
      }) %>% as.vector()
      
      downloadedFiles[grep(".txt.gz", downloadedFiles, ignore.case = TRUE)] %>% file.rename(paste0(destDir, "/", id, ".TXT.gz"))
    }
  }
  
  return(TRUE)
}

#' @title Process and normalize affymetrix-based dataset
#' @description
#' This function process CEL files and normalize expression data
#' @param dir The path to the directory containing CEL files.
#' @param samples A vector of samples IDs. If NULL, the function will automatically detect the samples in the directory.
#' @return A matrix of normalized expression data. Rows are probes and columns are samples.
#' @details  Read and normalize expression data for affymetrix using RMA method
#' @importFrom dplyr %>%
#' @importFrom Biobase exprs
#' @importFrom stringr str_replace
#' @export
#' @examples
#' \donttest{
#' library(RCPA)
#' geoId <- "GSE59761"
#' downloadPath <- file.path(tempdir(), geoId)
#' fileList <- RCPA::downloadGEO(GEOID = geoId, protocol = "affymetrix",
#'                                platform ="GPL16311", destDir = downloadPath)
#' # process only 3 samples
#' expression <- RCPA::processAffymetrix(downloadPath, 
#'                    samples = c("GSM1446171", "GSM1446172", "GSM1446173"))
#' }
processAffymetrix <- function(dir, samples = NULL) {
  
  if (!dir.exists(dir)) {
    stop("The input directory does not exist.")
  }
  
  if (!.requirePackage("oligo")) {
    return(NULL)
  }
  
  if (is.null(samples)) {
    samples <- list.files(dir, pattern = ".CEL.gz", full.names = FALSE) %>%
      stringr::str_replace(".CEL.gz", "")
    
    if (length(samples) == 0) {
      stop("There is no CEL.gz file in the directory.")
    } else {
      message("The following samples are found: ", paste0(samples, collapse = ", "))
    }
  }
  
  expression <- oligo::read.celfiles(file.path(dir, paste0(samples, '.CEL.gz'))) %>%
    oligo::rma(normalize = TRUE) %>%
    Biobase::exprs() %>%
    as.data.frame()
  if (sum(is.na(expression)) > 0) stop("There is NA in expression data.")
  
  colnames(expression) <- samples
  
  if (dim(expression)[1] == 0 || dim(expression)[2] == 0) {
    stop("The expression matrix is empty.")
  }
  
  expression %>% log2() %>% as.matrix()
}

#' @title Process and normalize agilent-based dataset
#' @description
#' This function process TXT files and normalize expression data
#' @param dir The path to the directory containing TXT files.
#' @param samples A vector of samples IDs. If NULL, the function will automatically detect the samples in the directory.
#' @param greenOnly Logical, for use with source, should the green (Cy3) channel only be read, or are both red and green required.
#' @return A matrix of normalized expression data. Rows are probes and columns are samples.
#' @details Read and normalize expression data for agilent using limma normexp, loess, and quantile methods
#' @importFrom limma read.maimages backgroundCorrect normalizeWithinArrays normalizeBetweenArrays
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace
#' @export
#' @examples
#' \donttest{
#' library(RCPA)
#' geoId <- "GSE28522"
#'
#' downloadPath <- file.path(tempdir(), geoId)
#' fileList <- RCPA::downloadGEO(GEOID = geoId, protocol = "agilent",
#'                               platform ="GPL4133", destDir = downloadPath)
#'
#' expression <- RCPA::processAgilent(downloadPath, greenOnly = FALSE)
#' }
#'
processAgilent <- function(dir, samples = NULL, greenOnly) {
  
  if (!dir.exists(dir)) {
    stop("The destination directory does not exist.")
  }
  
  if (is.null(samples)) {
    samples <- list.files(dir, pattern = ".TXT.gz", full.names = FALSE) %>%
      stringr::str_replace(".TXT.gz", "")
    
    if (length(samples) == 0) {
      stop("There is no TXT.gz file in the directory.")
    } else {
      message("The following samples are found: ", paste0(samples, collapse = ", "))
    }
  }
  
  raw.data <- read.maimages(file.path(dir, paste0(samples, ".TXT.gz")),
                            source = "agilent",
                            green.only = greenOnly,
                            names = samples
  )
  
  #Correct expression for background using the normexp method
  background_corrected_data <- backgroundCorrect(raw.data, method = "normexp")
  
  if (inherits(background_corrected_data, "RGList")) {
    # Normalize background-corrected data using the loess method for two color array
    norm1.data <- normalizeWithinArrays(background_corrected_data, method = "loess")
    
    # Normalize background-corrected data using the quantile method
    norm2.data <- normalizeBetweenArrays(norm1.data, method = "quantile")
    
    expression <- norm2.data$A
    rownames(expression) <- norm2.data$genes[, "ProbeName"]
  }else {
    # Normalize background-corrected data using the quantile method
    norm.data <- normalizeBetweenArrays(background_corrected_data, method = "quantile")
    
    expression <- norm.data$E
    rownames(expression) <- norm.data$genes[, "ProbeName"]
  }
  
  # if (!is.null(background_corrected_data$G) & !is.null(background_corrected_data$R)) {
  #     # Normalize background-corrected data using the loess method for two color array
  #     norm1.data <- normalizeWithinArrays(background_corrected_data, method = "loess")
  #
  #     # Normalize background-corrected data using the quantile method
  #     norm2.data <- normalizeBetweenArrays(norm1.data, method = "quantile")
  #
  #     expression <- norm2.data$G
  #     rownames(expression) <- norm2.data$genes[, "ProbeName"]
  # } else {
  #     # Normalize background-corrected data using the quantile method
  #     norm.data <- normalizeBetweenArrays(background_corrected_data, method = "quantile")
  #
  #     expression <- norm.data$E
  #     rownames(expression) <- norm.data$genes[, "ProbeName"]
  # }
  
  if (dim(expression)[1] == 0 | dim(expression)[2] == 0) {
    stop("The expression matrix is empty.")
  }
  
  expression %>% as.matrix()
}

#' @title Download GEO data
#' @description This function download and process data from GEO for microarray and RNASeq data.
#' @param GEOID The ID of the GEO dataset.
#' @param protocol The protocol of selected GEO dataset.
#' @param platform The platform of selected GEO dataset.
#' @param destDir A path to save downloaded data. If the directory does not exist, it will be created.
#' @return A vector of file paths to the downloaded files. The first element is the metadata file.
#' @examples
#' \donttest{
#' library(RCPA)
#' # Affymetrix
#' downloadPath <- file.path(tempdir(), "GSE59761")
#' fileList <- RCPA::downloadGEO(GEOID = "GSE59761", protocol = "affymetrix",
#'                                 platform ="GPL16311", destDir = downloadPath)
#' }
#' @importFrom dplyr %>%
#' @importFrom Biobase pData
#' @importFrom utils write.csv
#' @export
downloadGEO <- function(GEOID, protocol = c("affymetrix", "agilent"), platform, destDir) {
  protocol <- match.arg(protocol)
  protocol <- tolower(protocol)
  
  if (!dir.exists(destDir)) {
    message("The destination directory does not exist. It will be created.")
    dir.create(destDir, recursive = TRUE)
  }
  
  .checkURLAvailable("https://ftp.ncbi.nlm.nih.gov")
  
  #Download data with the specified platform from GEO
  GEOObject <- .downloadGEOObject(GEOID, platform, destDir)
  
  #Extract metadata from GEOObject
  GEOObject.metadata <- GEOObject %>% pData()
  
  #Download samples from the current GEO object
  sampleIDs <- GEOObject.metadata["geo_accession"] %>% apply(MARGIN = 1, FUN = function(x) x)
  downloadRes <- .downloadSamples(sampleIDs, protocol, destDir)
  
  if (downloadRes != TRUE) {
    stop("There is an error in downloading samples.")
  }
  
  rownames(GEOObject.metadata) <- sampleIDs
  metadata <- GEOObject.metadata %>% `colnames<-`(make.names(colnames(GEOObject.metadata), unique = TRUE)) %>% as.data.frame()
  write.csv(metadata, file.path(destDir, "metadata.csv"))
  
  if (protocol == "affymetrix") {
    return(
      c(
        "metadata.csv",
        list.files(destDir, pattern = ".CEL.gz", full.names = F)
      )
    )
  }else {
    return(
      c(
        "metadata.csv",
        list.files(destDir, pattern = ".TXT.gz", full.names = F)
      )
    )
  }
}
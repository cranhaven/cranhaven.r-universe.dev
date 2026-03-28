#' Import data from URL or file to HDF5 format
#' 
#' This function downloads data from a URL (if URL is provided) and decompresses it 
#' if needed, then imports the data into an HDF5 file. It supports both local files 
#' and remote URLs as input sources.
#' 
#' @param inFile Character string specifying either a local file path or URL containing 
#'        the data to import
#' @param destFile Character string specifying the file name and path where the HDF5 
#'        file will be stored
#' @param destGroup Character string specifying the group name within the HDF5 file 
#'        where the dataset will be stored
#' @param destDataset Character string specifying the name for the dataset within 
#'        the HDF5 file
#' @param header Logical or character vector. If TRUE, the first row contains column 
#'        names. If a character vector, use these as column names. Default is TRUE.
#' @param rownames Logical or character vector. If TRUE, first column contains row 
#'        names. If a character vector, use these as row names. Default is FALSE.
#' @param overwrite Logical indicating if existing datasets should be overwritten. 
#'        Default is FALSE.
#' @param overwriteFile Logical indicating if the entire HDF5 file should be 
#'        overwritten if it exists. CAUTION: This will delete all existing data. 
#'        Default is FALSE.
#' @param sep Character string specifying the field separator in the input file. 
#'        Default is "\\t" (tab).
#' @param paral Logical indicating whether to use parallel computation. Default is TRUE.
#' @param threads Integer specifying the number of threads to use for parallel 
#'        computation. Only used if paral=TRUE. If NULL, uses maximum available threads.
#'
#' @return No return value. The function writes the data directly to the specified 
#'         HDF5 file.
#'
#' @examples
#' \dontrun{
#' # Import from local file
#' bdImportData_hdf5(
#'   inFile = "data.txt",
#'   destFile = "output.h5",
#'   destGroup = "mydata",
#'   destDataset = "matrix1",
#'   header = TRUE,
#'   sep = "\t"
#' )
#' 
#' # Import from URL
#' bdImportData_hdf5(
#'   inFile = "https://example.com/data.csv",
#'   destFile = "output.h5",
#'   destGroup = "downloaded",
#'   destDataset = "remote_data",
#'   sep = ","
#' )
#' }
#'    
#' @export
bdImportData_hdf5 <- function( inFile, destFile, destGroup, destDataset, 
                               header = TRUE, rownames = FALSE, 
                               overwrite = FALSE, overwriteFile = FALSE, 
                               sep = NULL,  paral = NULL, threads = NULL)
{
    
    untarExtensions <- c("tar.gz", "gzip", "bzip2", "gz", "tgz")
    
    extension <- substr(inFile, regexpr("\\.[^\\.]*$", inFile)[1]+1, nchar(inFile))
    filename <- substr(inFile, regexpr("\\/[^\\/]*$", inFile)[1]+1, nchar(inFile)-nchar(extension)-1)
    importfile <- ""
    
    # if( url.exists(inFile))
    # {
    dfile <- try (download.file(url = inFile, destfile = paste0(getwd(), "/", filename, ".", extension)), TRUE)
    if (inherits(dfile, "try-error")) {
        if(!file.exists(inFile)) {
            stop("File does not exists, please review the route")
        } 
        
        importfile <- inFile
        
    } else {
        inFile <- paste0(filename,".", extension)
    }
    
    
    if(extension == "zip") {
        importfile <- unzip(inFile, list = TRUE )$Name[1]
        unzip(inFile)
    }else if(extension %in% untarExtensions) {
        importfile <- untar(inFile, list = TRUE )[1]
        untar(inFile)
    } else {
        importfile <- inFile
    }
    
    # Import files to hdf5
    bdImportTextFile_hdf5(filename = importfile,
                          outputfile = destFile,
                          outGroup = destGroup, 
                          outDataset = destDataset,
                          sep = sep,
                          header = header,
                          rownames = rownames,
                          overwrite = overwrite,
                          overwriteFile = overwriteFile,
                          paral = paral, 
                          threads = threads)
    # unlink(importfile)
}

#' Read raw spectra
#'
#' This function reads the raw .xmu file, extracts E0 and energy shift and returns a list with name, E0, energy shift, and the raw spectrum
#' @param file The raw .xmu file
#' @param use.eshift Set TRUE, if using energy shift value, defaults to NULL
#' @keywords normalization, correction
#' @export
#' @importFrom utils read.table
#' @examples
#' ## any .xmu file as output from ATHENA (>=0.9.25)

read_raw_spec <- function (file, use.eshift = NULL) {
  
  ## check if energy shift is set, default is FALSE
  if (is.null(use.eshift)) {
    use.eshift <- FALSE
  }
  
  ## create a dummy vector to be filled
  raw.spec.end <- NULL
  
  ## loop over all files
  for (i in 1:length(file)) {
    
    ## extract file name
    file.name <- strsplit(file[i],"\\.")[[1]][1]
    
    ## extract file header
    file.head <- readLines(file[i], n = length(grep("#", readLines(file[i]))))
    
    ## extract E zero from file header
    E.zero.temp.string <- strsplit(file.head[grep("Athena.e0",readLines(file[i]))]," ")[[1]]
    E.zero <- as.numeric(E.zero.temp.string[length(E.zero.temp.string)])
    
    ## extract the element name from file header
    element.temp.string <- strsplit(file.head[grep("Element.symbol",readLines(file[i]))]," ")[[1]]
    element <- element.temp.string[length(element.temp.string)]
    
    ## extract the E-shift
    E.shift.temp.string <- strsplit(file.head[grep("Athena.eshift", readLines(file[i]))], " ")[[1]]
    E.shift <- as.numeric(E.shift.temp.string[length(E.shift.temp.string)])
    
    ## extract the element edge from file header
    edge.temp.string <- strsplit(file.head[grep("Element.edge",readLines(file[i]))]," ")[[1]]
    edge <- edge.temp.string[length(edge.temp.string)]
    
    ## extract the column header information from file header
    header.temp.string <- strsplit(file.head[length(grep("#", readLines(file[i])))], " ")[[1]]
    header.temp.string <- header.temp.string[header.temp.string != ""]
    col.header <- header.temp.string[header.temp.string != "#"]
    
    ## read the table data
    raw.spec <- read.table(file[i], header = FALSE)
    colnames(raw.spec) <- col.header
    
    ## extract the energy and the raw absorption data
    raw.spec <- raw.spec[c("e", "xmu")]
    
    ## set the column names accordingly
    colnames(raw.spec) <- c("energy", "raw.absorption")
    
    if (is.null(raw.spec)) {
    }
    
    ## use energy shift, if set to TRUE
    if (use.eshift == TRUE) {
      raw.spec["energy"] <- raw.spec["energy"] + E.shift
    }
    
    ## create a list with name, element, edge and the data of the spectrum
    raw.spec.end[[i]] <- list("name" = file.name, "element" = element, "edge" = edge, "data" = list("E0" = E.zero, "E.shift" = E.shift, "raw.spec" = raw.spec))
    
    ## close file loop 
  }
  
  ## create and return a list with E zero and the raw spectrum  
  return(raw.spec.end)
  
  ## close function
}
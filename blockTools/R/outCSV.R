#' Export blocked or assigned data to .csv format files
#'
#' Exports output from \code{block} or \code{assignment} to a set of .csv files using \code{write.csv}.
#' 
#' Under the default (\code{file.names = NULL}), each file is named \dQuote{GroupXXX.csv}, where \dQuote{XXX} is the group name taken from the input object.
#' 
#' @author Ryan T. Moore
#' 
#' @param block.obj A list of dataframes, such as output from \code{block} or \code{assignment}.
#' @param namesCol An optional character vector of column names to be used in output files.
#' @param file.names An optional character vector of file names specifying the output file names.
#' @param digits An integer representing the number of decimal places to which to round multivariate distances in output files, passed to \code{round()}.
#' @param ... Additional arguments passed to \code{write.csv}.
#' 
#' @return A set of .csv files, one for each element of the input list of blocked or assigned units, written by \code{write.csv()}.
#' 
#' @keywords design IO
#' 
#' @examples 
#' data(x100)
#' 
#' # Block and assign:
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = "id", block.vars = c("b1", "b2"))
#' assg <- assignment(out, seed = 123)
#' 
#' # create three .csv files of blocks
#' \dontrun{outCSV(out)}
#' # create three .csv files of assigned blocks
#' #   (note: overwrites blocked .csv files)
#' \dontrun{outCSV(assg)}
#' # create three .csv files with custom file names
#' \dontrun{outCSV(assg, file.names = c("file1", "file2", "file3"))}
#' 
#' @seealso \code{\link{outTeX}}, \code{\link{write.csv}}, \code{\link{block}}, \code{\link{assignment}}
#' 
#' @export

outCSV <- function(block.obj, namesCol = NULL, file.names = NULL, digits = 2, ...){

  # takes block, assignment, or diagnose object
  if(!is.null(block.obj$blocks)){ 
    block.obj <- block.obj$blocks
  }
  if(!is.null(block.obj$assg)){ 
    block.obj <- block.obj$assg
  }
  
  for(i in 1:length(block.obj)){
    tab <- block.obj[[i]]
    nm <- names(block.obj)[i]
 
    tab[, ncol(tab)] <- round(tab[, ncol(tab)], digits)

    if(!is.null(namesCol)){
      names(tab) <- namesCol
    }
    if(is.null(file.names)){
    	file.name <- paste("Group", nm, ".csv", sep="")
    }else{
    	file.name <- paste(file.names[i], ".csv", sep = "")
    	}
    
    write.csv(tab, file = file.name, ...)
  }
}

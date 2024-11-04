#' @title Export blocked or assigned data to .tex format files
#'
#' @description Exports output from \code{block} or \code{assignment} to a set of .tex files using \code{print(xtable)}.
#' 
#' @details 
#' Under the default (\code{file.names = NULL}), each file is named 
#' \dQuote{GroupXXX.tex}, where \dQuote{XXX} is the group name taken 
#' from the input object. Under the default (\code{captions = NULL}), 
#' each caption is \dQuote{Group XXX.}, where \dQuote{XXX} is the group 
#' name taken from the input object.
#' 
#' \code{outTeX} appends \code{.tex} to the user-specified \code{file.names}.
#' 
#' The table reference labels are created as \code{t:XXX}, where \code{XXX} 
#' is the file name (without \code{.tex}) for the \code{.tex} file containing that table.
#' 
#' \code{captions} takes a list of strings of length equal to the number of groups 
#' in \code{block.obj$blocks}, if \code{block.obj} is output from \code{block}, 
#' or the number of groups in \code{block.obj$assg}, if \code{block.obj} is output 
#' from \code{assignment}.
#' 
#' The tables in the output .tex files can be integrated into an existing .tex document 
#' using LaTeX code \samp{\\include{GroupXXX}}.
#' 
#' @author Ryan T. Moore
#' 
#' @param block.obj A list of dataframes, such as output from \code{block} or \code{assignment}.
#' @param namesCol An optional character vector of column names to be used in output files.
#' @param file.names An optional character vector of file names specifying the output file names.
#' @param captions An optional character vector of file names specifying the table captions. See Details below.
#' @param digits An integer representing the number of decimal places to which to round multivariate distances in output files, passed to \code{round}.
#' @param ... Additional arguments passed to \code{xtable}.
#' 
#' @return A set of .tex files, one for each element of the input list of blocked or assigned units, written by \code{print(xtable)}.
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
#' # create three .tex files of blocks
#' \dontrun{outTeX(out)}
#' # create three .tex files of assigned blocks
#' #   (note: overwrites blocked .tex files)
#' \dontrun{outTeX(assg)}
#' # create three .tex files with custom file names and captions
#' \dontrun{outTeX(assg, file.names = c("f1", "f2", "f3"), captions = c("C 1.", "C 2.", "C 3."))}
#' 
#' @seealso \code{\link{outCSV}}, \code{\link[xtable]{xtable}}, \code{\link{block}}, \code{\link{assignment}}
#' 
#' @export

outTeX <- function(block.obj, namesCol = NULL, file.names = NULL, 
                   captions = NULL, digits = 2, ...){

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

    if(is.null(captions)){
      caption <- paste("Group ", nm, ".", sep="")
    }else{
    	caption <- captions[[i]]
    }

    ncol.tab <- ncol(tab)
    
    # user-specified column names
    if(!is.null(namesCol)){
      names(tab) <- namesCol
    }

    if(is.null(file.names)){
    	file.name <- paste("Group", nm, ".tex", sep = "")
    	lab <- paste("group.", nm, sep = "")
    }else{
    	file.name <- paste(file.names[[i]], ".tex", sep = "")
    	lab <- paste("t:", file.names[[i]], sep = "")
    }
    
    tab.tex <- xtable::xtable(tab, label = lab, caption = caption, 
                              align = c(rep("c",ncol(tab) + 1)), 
                              digits = rep(digits, ncol(tab) + 1), ...)
                                            
    print(tab.tex, file = file.name, caption.placement = "bottom")
  }
}

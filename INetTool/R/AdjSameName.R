#' adj_rename
#' @description This function constructs a list of adjacency matrices with the
#' same row and column names for all the matrices. The output is the object
#' needed for \code{\link{consensusNet}} function.
#'
#' @param adjL list of adjacency matrices
#'
#' @return a list of adjacency matrices with the same rows and columns name.
#' @export
#'
#' @examples
#' data("tryL_data")
#' adj_rename(tryL_data)

adj_rename <- function(adjL)
{

    #### Same name of nodes for all the networks:
  geneName <- NULL
  for (i in 1:length(adjL))
  {
    genename <- rownames(adjL[[i]])
    geneName <- c(geneName,genename)
  }
  genes <- unique(geneName)
  length(genes)


  ####### Create the same names:
  Mat <- vector(mode = "list", length = length(adjL))
  for (z in 1:length(adjL))
  {

    Mat[[z]] <- matrix(0, nrow=length(genes), ncol= length(genes))
    rownames(Mat[[z]]) <- genes
    colnames(Mat[[z]]) <- genes

    for ( i in 1:(dim(adjL[[z]])[1]))
    {

      for (j in 1:(dim(adjL[[z]])[2]))

      {

        valore <- adjL[[z]][rownames(adjL[[z]])[i],
                                      colnames(adjL[[z]])[j]]

        Mat[[z]][which(rownames(Mat[[z]])==rownames(adjL[[z]])[i]),
                 which(colnames(Mat[[z]])==colnames(adjL[[z]])[j])]<- valore

      }


    }

  }

  return(Mat)
}

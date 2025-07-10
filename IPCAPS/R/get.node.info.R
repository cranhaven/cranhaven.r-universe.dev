
#' Get the information for specified node
#'
#' @description Obtain the information for specified
#' node from the output list of \code{\link{ipcaps}}.
#'
#' @param cluster.obj A list returned from the \code{\link{ipcaps}} function.
#' @param node An integer representing a node number to enquire information as
#' shown in the HTML output files.
#'
#' @return The return value is \code{NULL} if node's information does not exist or a list containing
#' \code{PCs}, \code{eigen.fit}, \code{index}, and \code{label} as explained
#' below:
#' \itemize{
#' \item \code{$PCs} is a matrix of pricipal components of this node.
#' \item \code{$eigen.fit} is a number represeting the EigenFit value of this
#' node.
#' \item \code{$index} is a vector of row number (individuals) of \code{raw.data} (input data).
#' \item \code{$label} is the vector of labels of all individuals that belongs
#' to this node.
#' }
#'
#' @export
#'
#' @examples
#'
#' # Importantly, bed file, bim file, and fam file are required
#' # Use the example files embedded in the package
#'
#' BED.file <- system.file("extdata","ipcaps_example.bed",package="IPCAPS")
#' LABEL.file <- system.file("extdata","ipcaps_example_individuals.txt.gz",package="IPCAPS")
#'
#' my.cluster <- ipcaps(bed=BED.file,label.file=LABEL.file,lab.col=2,out=tempdir())
#'
#' #Here, to obtain the information of specified node, for example, node 3
#' node.info <- get.node.info(my.cluster,3)
#' ls(node.info)

get.node.info <- function(cluster.obj,node){
  PCs <- NULL
  eigen.fit <- NULL
  index <- NULL
  label <- NULL

  if (is.null(cluster.obj$output.dir)){
    cat(paste0("Incorrect parameter, please use the object returned from the function ipcaps as an input\n"))
    return(NULL)
  }else{
    file.name <- file.path(cluster.obj$output.dir,"RData",paste0("node",node,".RData"))
    if (!file.exists(file.name)){
      cat(paste0("Node ",node," doesn't exist\n"))
      return(NULL)
    }else{
      load(file.name)
      res <- list('PCs'=PCs,'eigen.fit'=eigen.fit,'index'=index,'label'=label)
      return(res)
    }
  }
}



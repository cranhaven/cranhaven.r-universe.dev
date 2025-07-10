
#' Export the IPCAPS result to a text file
#'
#' @description Export clustering result of
#' \code{\link{ipcaps}} to text file called 'groups.txt'.
#'
#' @param result.dir A result directory as the \code{$output} object returned
#' from the \code{\link{ipcaps}} function.
#'
#' @return A data frame of exported data containing 4 columns; \code{group},
#' \code{node}, \code{label}, \code{row.number}, as described below for more
#' details:
#' \itemize{
#' \item \code{group} represents group membership of IPCAPS result.
#' \item \code{node} represents node numbers of IPCAPS result.
#' \item \code{label} represents labels of rows in orginal input data.
#' \item \code{row.number} represents row numbers of orginal input data.
#' }
#'
#' @details After running, this function exports the file called 'groups.txt' to the same result
#' directory. If 'groups.txt' already exists in the result directory, the
#' exported file is changed to 'groups1.txt', 'groups2.txt', 'groups3.txt', ...,
#' accordingly.
#'
#' @export
#'
#' @import utils
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
#' #Here, to export the IPCAPS result to a text file
#' exported.data <- export.groups(my.cluster$output.dir)
#' print(dim(exported.data))
#' head(exported.data)

export.groups <- function(result.dir){
  leaf.node <- NULL
  index <- NULL
  label <- NULL

  load(file.path(result.dir,"RData","leafnode.RData"))
  export.data = NULL
  for (i in 1:length(leaf.node)){
    assigned_group = which(leaf.node == leaf.node[i])
    cat(paste0("Exporting node ",leaf.node[i]," as group ",assigned_group,"\n"))
    load(file.path(result.dir,"RData",paste0("node",leaf.node[i],".RData")))
    groups = rep(assigned_group,length(index))
    nodes = rep(leaf.node[i],length(index))
    node.data = data.frame(groups,nodes,label,index)
    export.data = rbind(export.data,node.data)
  }
  colnames(export.data) = c("group","node","label","row.number")
  file.name = file.path(result.dir,"groups.txt")
  if (file.exists(file.name)){
    i = 1
    file.name = file.path(result.dir,paste0("groups",i,".txt"))
    while (file.exists(file.name)){
      i = i + 1
      file.name = file.path(result.dir,paste0("groups",i,".txt"))
    }
  }
  cat(paste0("Note: save as ",file.name,"\n"))
  write.table(export.data,file=file.name,quote=FALSE,sep="\t",row.names=FALSE,col.names=TRUE)
  return(export.data)
}




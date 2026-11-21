#' @title LDA_activity
#'
#' @description calculation of clonogenic activities from data collected in a
#'   limiting dilution assay (LDA) experiment (i.e. cells, wells, positive
#'   wells, group).
#'
#' @param x numeric data.frame or matrix with three columns (cells,
#'   wells, positive wells, group (optional))
#' @param name optional: experiment name (e.g. name of cell line)
#'
#' @return list object with LDA-activities as returned by LDA_activity_single
#'
#' @examples
#' x <- data.frame("cells" = c(10,50,100,250,10,50,100,250),
#'                 "wells" = rep(25,8),
#'                 "positive" = c(2,5,10,20,1,2,6,11),
#'                 "group" = c(rep("A",4),rep("B",4)))
#' act <- LDA_activity(x)
#' @export
#'
LDA_activity <- function(x,name = "LDA cells"){
  if (!(class(x)[1] %in% c("data.frame","matrix"))){
    stop("error: x must be of class data.frame or matrix")
  }
  if (ncol(x) == 3){
    warning("warning: no group variable assigned - treated as one group.")
    act <- LDA_activity_single(x,name)
  }
  if (ncol(x) > 3){
    x <- x[,1:4]
    colnames(x) <- c("cells","wells","positive","group")
    groups <- unique(x$group)
    act <- vector(mode = "list",length = length(groups))
    for (i in seq_along(groups)){
      act[[i]] <- LDA_activity_single(
        x = subset.data.frame(x = x,
                              subset = x$group == groups[i],
                              select = c("cells","wells","positive"),
                              drop = TRUE),name,treat = groups[i])

    }
    class(act) <- "LDA_activity_list"
  }
  return(act)
}

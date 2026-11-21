#' @title LDA_survival
#'
#' @description calculation of clonogenic survival in a table of data from a
#'   limiting dilution assay (LDA) experiment (i.e. cells, wells, positive
#'   wells, group).
#'
#' @param x numeric data.frame or matrix with three columns (cells,
#'   wells, positive wells, group)
#' @param name optional: experiment name (e.g. name of cell line)
#'
#' @return list object with LDA-activities as returned by LDA_activity_single
#'
#' @examples
#' x <- data.frame("cells" = c(10,50,100,250,10,50,100,250),
#'                 "wells" = rep(25,8),
#'                 "positive" = c(2,5,10,20,1,2,6,11),
#'                 "group" = c(rep("A",4),rep("B",4)))
#' act <- LDA_survival(x)
#' @export
#'
LDA_survival <- function(x,name = "cell line a"){
  if (!(class(x)[1] %in% c("data.frame","matrix"))){
    stop("error: x must be of class data.frame or matrix")
  }
  if (ncol(x) < 4){
    stop("error: analysis of clonogenic survival requires at least 2 groups
         in column 4. Consider LDA_activity() for analysis of clonogenic
         activity.")
  }
  act <- LDA_activity(x,name)
  ref <- act[[1]]
  sf_list <- vector(mode = "list",length = length(act)-1)
  for (i in seq_along(sf_list)){
    act.0 <- ref
    act.1 <- act[[i+1]]
    sf <- LDA_survival_single(act.0 = act.0,
                              act.x = act.1)
    #sf$treatment <- act.1$treatment
    sf_list[[i]] <- sf
  }
  return(sf_list)
}

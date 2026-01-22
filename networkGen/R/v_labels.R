# #' @export
# #' @title v.labels
# #' @description. This is for people to choose their own names to put in the node.
# #' @details By default, some name (countries) will be provided. However, it may be better to have your own names if you wish. The output is invisible so you need to check by using the '$' sign.
# #' @param names This is the names input. The more names the better.
# #' @param newValue is the number of names you want to randomly select.
# #' @author Aiden Loe
# #' @examples
# #' logic <- nodeLogic(value = 8, type= "circuit", itemFamily= 1)
# #' a <- v.labels(names=NULL, 4)
# #' a$d.names
#

#Upload data file if not use default dataset in function.

v.labels <- function(names=NULL, newValue)
{
    if(!is.null(names)){
          names[] <- lapply(names, as.character)
          names <- matrix(names, ncol=1)
          #sample dataset
          d.names <- names[sample(1:nrow(names), newValue, replace=FALSE),]
          # create list of items
          d.names <- list(d.names = d.names)

          class(d.names) <- "v.labels"
          invisible(d.names)

    }else{
          default.items <- c("Albania","Andorra","Armenia","Austria","Azerbajian","Belarus","Belgium","Bulgaria","Croatia","Cyprus","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Kazakhstan","Kosovo","Latvia","Malta","Moldova","Monaco","Norway","Poland","Portugal","Romania","Russia","Serbia","Slovakia","Solvenia","Spain","Sweden","Turkey","Ukraine","UK","US")

          d.names <- sample(default.items, newValue, replace=FALSE)
          # create list of items
          d.names <- list(d.names = d.names)

          class(d.names) <- "v.labels"
          invisible(d.names)

    }
}



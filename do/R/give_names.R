#' change vector, dataframe or matrix names
#' @param data one vector, list, dataframe or matrix
#' @param ... one or more names
#' @param row logical, whether the names is row names. Default is FALSE
#' @name give_names
#' @export
#' @return names changed data
give_names <- function(data,...) UseMethod('give_names')
#' @name give_names
#' @method give_names character
#' @export
give_names.character <- function(data,...){
    names(data) <- c(...)
    data
}
#' @name give_names
#' @method give_names numeric
#' @export
give_names.numeric <- function(data,...){
    names(data) <- c(...)
    data
}
#' @name give_names
#' @method give_names logical
#' @export
give_names.logical <- function(data,...){
    names(data) <- c(...)
    data
}
#' @name give_names
#' @method give_names list
#' @export
give_names.list <- function(data,...){
    names(data) <- c(...)
    data
}

#' @name give_names
#' @method give_names data.frame
#' @export
give_names.data.frame <- function(data,...,row=FALSE){
    if (row){
        row.names(data) <- c(...)
    }else{
        colnames(data) <- c(...)
    }
    data
}
#' @name give_names
#' @method give_names matrix
#' @export
give_names.matrix <- function(data,...,row=FALSE){
    if (row){
        row.names(data) <- c(...)
    }else{
        colnames(data) <- c(...)
    }
    data
}




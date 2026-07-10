

#' Subset data
#' Take subset data for 
#' @param data one vector, list, dataframe or matrix
#' @param i element position for vector or list, row number for dataframe or matrix
#' @param j column number for dataframe or matrix
#' @param drop logical, whether to drop original format, default is FALSE
#' @param ... ignore
#' @name select
#' @return selected data
#' @export
#' @examples 
#' x <- c('ab','bc','d')
#' x |> select(!grepl('a'))
#' x |> select(grepl('a'))
#' x |> select(!grepl('a'))
#' x |> select(grepl('a'))
#'
#' x <- mtcars
#' x |> select(,!grepl('m',ignore.case = TRUE))
#' x |> select(grepl('m',ignore.case = TRUE),grepl('m',ignore.case = TRUE))
#' x |> select(!grepl('m',ignore.case = TRUE),!grepl('m',ignore.case = TRUE))
#' 
#' x |> select(grepl('a') & grepl('m'))
#' x |> select(grepl('a|m'))
#' x |> select(am ==1)

select <- function(data,i,...) UseMethod('select')
#' @rdname select
#' @method select character
#' @export
select.character <- function(data,i,...){
    cmd <- paste0(deparse(substitute(i)),collapse = '')
    ck <- grepl('grepl\\(', cmd)
    if (ck){
        ck <- cmd |> 
            Replace("grepl\\(","grepl(x=data,") |> 
            exec()
        data[ck]
    }else{
        data[i]
    }
    
}
#' @rdname select
#' @method select numeric
#' @export
#' 
select.numeric <- function(data,i,...){
    cmd <- paste0(deparse(substitute(i)),collapse = '')
    ck <- grepl('grepl\\(', cmd)
    if (ck){
        ck <- cmd |> 
            Replace("grepl\\(","grepl(x=data,") |> 
            exec()
        data[ck]
    }else{
        data[i]
    }
}
#' @rdname select
#' @method select logical
#' @export
select.logical <- function(data,i,...){
    data[i]
}
#' @rdname select
#' @method select data.frame
#' @export
select.data.frame <- function(data,i,j,drop=FALSE,...){
    if (!missing(i)){
        cmd <- paste0(deparse(substitute(i)),collapse = '')
        ck <- grepl('grepl\\(', cmd)
        if (ck){
            ck <- cmd |> 
                Replace("grepl\\(","grepl(x=rownames(data),") |> 
                exec()
        }else{
            ck <- fmt('with(data,eval(/ ))',substitute(i)) |> 
                exec()
        }
        data <- data[ck,]
    }
    if (!missing(j)){
        cmd <- paste0(deparse(substitute(j)),collapse = '')
        ck <- grepl('grepl\\(', cmd)
        if (ck){
            ck <- cmd |> 
                Replace("grepl\\(","grepl(x=colnames(data),") |> 
                exec()
        }else{
            ck <- j
        }
        data <- data[,ck,drop=drop]
    }
    data
}

#' @rdname select
#' @method select matrix
#' @export
select.matrix <- function(data,i,j,drop=FALSE,...){
    if (!missing(i)){
        cmd <- paste0(deparse(substitute(i)),collapse = '')
        ck <- grepl('grepl\\(', cmd)
        if (ck){
            ck <- cmd |> 
                Replace("grepl\\(","grepl(x=rownames(data),") |> 
                exec()
        }else{
            ck <- fmt('with(data,eval(/ ))',substitute(i)) |> 
                exec()
        }
        data <- data[ck,]
    }
    if (!missing(j)){
        cmd <- paste0(deparse(substitute(j)),collapse = '')
        ck <- grepl('grepl\\(', cmd)
        if (ck){
            ck <- cmd |> 
                Replace("grepl\\(","grepl(x=colnames(data),") |> 
                exec()
        }else{
            ck <- j
        }
        data <- data[,ck,drop=drop]
    }
    data
}
#' @rdname select
#' @method select list
#' @export
select.list <- function(data,i,j,drop=FALSE,...){
    if (!missing(i)){
        cmd <- paste0(deparse(substitute(i)),collapse = '')
        ck <- grepl('grepl\\(', cmd)
        if (ck){
            ck <- cmd |> 
                Replace("grepl\\(","grepl(x=names(data),") |> 
                exec()
            data <- data[ck]
        }else{
            data <- data[i]
        }
    }
    if (!missing(j)) {
        data <- lapply(data, function(i) i[j])
    }
    if (!missing(drop) & length(data)==1) data <- data[[1]]
    data
}
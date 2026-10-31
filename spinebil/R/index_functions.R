#' Matching index functions to the required format.
#'
#' These are convenicence functions that format
#' scagnostics and mine index functions for direct
#' use with the guided tour or other functionalities in this package.
#'
#' @describeIn scag_index Scagnostics index from cassowaryr package
#' @param index_name Index name to select from group of indexes.
#' @return function taking 2-d data matrix and returning the index value
#' @export
scag_index <- function(index_name) {
  function(mat) {
    sR <- cassowaryr::calc_scags_wide(as.data.frame(mat), index_name)
    return(sR[[3]])
  }
}

#' @describeIn scag_index MINE index from minerva package
#' @export
mine_index <- function(index_name){
  function(mat){
    return(minerva::mine(mat[,1], mat[,2])[[index_name]])
  }
}

#' @describeIn scag_index MINE index from minerva package (updated estimator)
#' @export
mine_indexE <- function(index_name){
  function(mat){
    return(minerva::mine(mat[,1], mat[,2], est = "mic_e")[[index_name]])
  }
}


#' @describeIn scag_index rescaling the tourr holes index
#' @export
holesR <- function(){
  function(mat){
    ret <- tourr::holes()(mat)
    if(ret<0.8) ret <- 0.
    else ret <- (ret-0.8) * 5
    return(ret)
  }
}

#' @describeIn scag_index rescaling the tourr cmass index
#' @export
cmassR <- function(){
  function(mat){
    ret <- tourr::cmass()(mat)
    if(ret<0.2) ret <- 0.
    else ret <- (ret-0.2) * 1.25
    return(ret)
  }
}



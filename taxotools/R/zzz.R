#' @importFrom stringi stri_trans_general

'%!in%' <- function(x,y)!('%in%'(x,y))

toproper <- function(x) ifelse(!is.na(x),
                               paste0(toupper(substr(x, 1, 1)),
                                      tolower(substring(x, 2))),NA)
isproper <- function(word){
  if(word == toproper(word)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

islower <- function(word){
  if(word == tolower(word)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

isupper <- function(chr){
  if(chr == toupper(chr)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.empty <- function(val){
  if(is.null(val)){
    return(TRUE)
  }
  if(is.na(val)){
    return(TRUE)
  }
  if(val==""){
    return(TRUE)
  }
  return(FALSE)
}

rename_column <- function(dat,old,new,silent=FALSE){
  if(old %in% colnames(dat)){
    colnames(dat)[which(names(dat) == old)] <- new
  } else {
    if(!silent){
      cat(paste("\nFieldname not found...",old))
    }
  }
  return(dat)
}

utf2ascii <- function(x){
  x <- ifelse(!is.na(x) & Encoding(x)=="UTF-8",
              stringi::stri_trans_general(x,id="Latin-ASCII"),x)
  return(x)
}

str2ascii <- function(x){
  x <- ifelse(!is.na(x) ,
              stringi::stri_trans_general(x,id="Latin-ASCII"),x)
  return(x)
}
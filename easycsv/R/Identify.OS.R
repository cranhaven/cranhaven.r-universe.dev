#' @export
Identify.OS = function(){
  pl <- .Platform$OS.type
  if(tolower(pl) == "windows"){
    os = structure("windows", class = "character")
  }
  else{
    si <- as.list(Sys.info())
      if(tolower(si$sysname) == "linux"){
      os = structure("Linux", class = "character")
    }
    if(tolower(si$sysname) == "darwin"){
      os = structure("MacOSX", class = "character")
    }
  }
  return(os)
}

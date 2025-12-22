down_inmet <- function(
    zipfile = NULL,
    filen = NULL,
    exdir = tempdir(),
    file = NULL,
    message = T
){
  unzipf <- unzip(zipfile = zipfile,
                  files = filen,
                  exdir = exdir,
                  overwrite = TRUE)

  if(!is.null(file)){

    file.copy(unzipf,file)
    if(message){
      print(paste0("Arquivo salvo em ",file))
    }
    invisible(file.remove(unzipf))

  }else{
    if(message){
      print(paste0("Arquivo salvo em ",exdir))
    }
  }
}


#' @importFrom stats setNames
#' @importFrom utils read.csv unzip
#' @export
loadcsvfromZIP <- function(filezip = NULL,
                           txt = FALSE ,
                           encoding = "Latin-1",
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           quote = "\"",
                           fill = TRUE,
                           comment.char = ""){




  if(is.null(filezip)){
    filezip = file.choose()
    fileEnding = substr(filezip,nchar(filezip)-3,nchar(filezip))
    if(fileEnding != ".zip"){
      stop("Please supply a valid .zip file")
    }
    filezip <- unzip(zipfile = filezip)
  }else{
    fileEnding = substr(filezip,nchar(filezip)-3,nchar(filezip))
    if(fileEnding != ".zip"){
      stop("Please supply a valid .zip file")
    filezip <- unzip(zipfile = filezip)

    }
  }
    ending = ifelse(txt == TRUE,
                    "*.txt$",
                    "*.csv$")
    list2env(setNames(object = lapply(filezip,
                                      read.csv,
                                      stringsAsFactors = stringsAsFactors,
                                      encoding = encoding,
                                      header = header,
                                      quote = quote,
                                      fill = fill,
                                      comment.char = comment.char),
                      nm = make.names(
                        paste0("", substr(
                          gsub(ending,"", filezip)
                          ,3, 40 )))), globalenv())

  }


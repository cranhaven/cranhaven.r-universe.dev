#' @importFrom stats setNames
#' @importFrom utils read.csv unzip download.file
#' @export
loadZIPcsvfromURL <- function(urlAddress = NULL,
                              txt = FALSE ,
                              encoding = "Latin-1",
                              stringsAsFactors = FALSE,
                              header = TRUE,
                              quote = "\"",
                              fill = TRUE,
                              comment.char = ""){
  temp <- tempfile()
  if(is.null(urlAddress)){
  stop("Please supply a valid URL containing a .zip file")
  }
  urlend = substr(urlAddress,nchar(urlAddress)-3, nchar(urlAddress))
  if(urlend != ".zip"){
    stop("Please supply a valid URL containing a .zip file")
  }else{
    download.file(urlAddress,
                  destfile = temp,
                  method = "libcurl")
    tempzip <- unzip(zipfile = temp)

    list2env(setNames(object = lapply(tempzip,
                                      read.csv,
                                      encoding = encoding,
                                      stringsAsFactors = stringsAsFactors,
                                      header = header,
                                      quote = quote,
                                      dec = ".",
                                      fill = fill,
                                      comment.char = comment.char),
                                   nm = make.names(
                                          paste0("",
                                                  substr(
                                                    gsub(
                                                      ifelse(txt == TRUE,
                                                                   "*.txt$",
                                                                   "*.csv$"),
                                                      "",
                                                      tempzip),
                                                    3, 40))))
                   , globalenv())

    unlink(temp, recursive = TRUE)
  }


}

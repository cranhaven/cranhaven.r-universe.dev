#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @export
loadcsv_multi <- function(directory = NULL,
                          extension = "CSV",
                          encoding = "Latin-1",
                          stringsAsFactors = FALSE,
                          header = TRUE,
                          quote = "\"",
                          fill = TRUE,
                          comment.char = ""){

  if(is.null(directory)){
    os = Identify.OS()
    if(tolower(os) == "windows"){
      directory <- utils::choose.dir()
      if(tolower(os) == "linux" | tolower(os) == "macosx"){
        directory <- choose_dir()
      }
    }else{
      stop("Please supply a valid local directory")
    }

  }

  directory = paste(gsub(pattern = "\\", "/", directory,
                         fixed = TRUE))



  endings = list()

  if(tolower(extension) == "txt"){
    endings[1] =  "*\\.txt$"
  }
  if(tolower(extension) == "csv"){
    endings[1] =  "*\\.csv$"

  }
  if(tolower(extension) == "both"){
    endings[1] =  "*\\.txt$"
    endings[2] =  "*\\.csv$"
  }
  if((tolower(extension) %in% c("txt","csv","both")) == FALSE){
    stop("Pleas supply a valid value for 'extension',\n
         allowed values are: 'TXT','CSV','BOTH'.")
  }
  tempfiles = list()
  temppath = list()
  num = 1
  for(i in endings){
    temppath = paste(directory,list.files(path = directory, pattern=i), sep = "/")
    tempfiles = list.files(path = directory, pattern=i)
    num = num +1
    if(length(temppath) < 1 | length(tempfiles) < 1){
      num = num+1
    }
    else{
      temppath = unlist(temppath)
      tempfiles = unlist(tempfiles)
      for(tbl in temppath){
        DFname1 = paste0(gsub(directory, "", tbl))
        DFname2 = paste0(gsub("/", "", DFname1))
        DFname3 = paste0(gsub(i, "", DFname2))
        DFable <- read.csv(file = tbl,
                           encoding  = encoding,
                           stringsAsFactors = stringsAsFactors,
                           header = header,
                           quote = quote,
                           fill = fill,
                           comment.char = comment.char
        )
        assign_to_global <- function(pos=1){
          assign(x = DFname3,value = DFable, envir=as.environment(pos) )
        }
        assign_to_global()

        rm(DFable)
      }
    }
  }
}

